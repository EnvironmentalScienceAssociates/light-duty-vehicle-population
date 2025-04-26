library(sf)
library(dplyr)
library(leaflet)

if (!dir.exists("data")) dir.create("data")

# https://www.energy.ca.gov/files/zev-and-infrastructure-stats-data
ld_file = file.path("data", "Vehicle_Population.xlsx")
if (!file.exists(ld_file)){
  download.file("https://www.energy.ca.gov/filebrowser/download/6311?fid=6311", ld_file)
}

# https://gis.data.ca.gov/datasets/CALFIRE-Forestry::california-incorporated-cities-1/about
cities = read_sf("https://egis.fire.ca.gov/arcgis/rest/services/FRAP/Incorp/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") |> 
  st_make_valid()

# https://gis.data.ca.gov/datasets/California::california-county-boundaries-and-identifiers/about
counties = read_sf("https://services3.arcgis.com/uknczv4rpevve42E/arcgis/rest/services/California_County_Boundaries_and_Identifiers_Blue_Version_view/FeatureServer/1/query?outFields=*&where=1%3D1&f=geojson") |> 
  st_make_valid()

# https://gis.data.ca.gov/datasets/CDEGIS::california-zip-codes/about
zip_codes = read_sf("https://services3.arcgis.com/fdvHcZVgB2QSRNkL/arcgis/rest/services/ZipCodes/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson")|> 
  st_make_valid()


leaflet(options = leafletOptions(attributionControl = FALSE)) |>
  setView(lng = -120, lat = 37.5, zoom = 6) |>
  addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") |> 
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |> 
  addLayersControl(baseGroups = c("Topo", "Satellite"),
                   options = layersControlOptions(collapsed = FALSE)) |> 
  addPolygons(data = counties,
              label = ~CDTFA_COUNTY,
              color = "black",
              weight = 2,
              fillOpacity = 0) |>
  # addPolygons(data = filter(cities, CITY %in% tmp$CITY), 
  #             label = ~CITY,
  #             popup = ~COUNTY,
  #             color = "black",
  #             weight = 2,
  #             fillOpacity = 0) |> 
  addPolygons(data = zip_codes, 
              label = ~ZIP_CODE,
              popup = ~PO_NAME,
              weight = 2,
              fillOpacity = 0)

# st_intersection for finding overlap is slow so using st_intersects first
zip_county_inter = st_intersects(zip_codes, counties)
zip_county_list = list()
for (i in 1:nrow(zip_codes)){
  cat("\rRow", i, "of", nrow(zip_codes))
  zip_sub = zip_codes[i,]
  counties_sub = counties[zip_county_inter[[i]],]
  tmp = st_intersection(zip_sub, counties_sub)
  prop_area = st_area(tmp)/st_area(zip_sub)
  sel = which(prop_area == max(prop_area))
  zip_county_list[[i]] = data.frame(zip = zip_sub$ZIP_CODE,
                                    county = counties_sub$CDT_NAME_SHORT[sel],
                                    county_overlap = units::drop_units(prop_area[sel]))
}
zip_county = bind_rows(zip_county_list)

zip_city_inter = st_intersects(zip_codes, cities)
zip_city_list = list()
for (i in 1:nrow(zip_codes)){
  cat("\rRow", i, "of", nrow(zip_codes))
  zip_sub = zip_codes[i,]
  cities_sub = cities[zip_city_inter[[i]],]
  tmp = st_intersection(zip_sub, cities_sub)
  prop_inc = sum(st_area(tmp))/st_area(zip_sub)
  zip_city_list[[i]] = data.frame(zip = zip_sub$ZIP_CODE,
                                  prop_inc = units::drop_units(prop_inc))
}
zip_city = bind_rows(zip_city_list)

county_city_inter = st_intersects(counties, cities)
county_city_list = list()
for (i in 1:nrow(counties)){
  cat("\rRow", i, "of", nrow(counties))
  counties_sub = counties[i,]
  cities_sub = cities[county_city_inter[[i]],]
  tmp = st_intersection(counties_sub, cities_sub)
  prop_inc = sum(st_area(tmp))/st_area(counties_sub)
  county_city_list[[i]] = data.frame(county = counties_sub$CDT_NAME_SHORT,
                                     prop_inc = units::drop_units(prop_inc))
}
county_city = bind_rows(county_city_list)

cities_sf = select(cities, objectid = OBJECTID, city = CITY, county = COUNTY)

cities_file = file.path("data", "cities_sf.rds")
if (!file.exists(cities_file)){
  saveRDS(cities_sf, cities_file)
}

counties_sf = counties |> 
  select(objectid = OBJECTID, county = CDT_NAME_SHORT, area_sqmi = AREA_SQMI) |> 
  left_join(county_city)

counties_file = file.path("data", "counties_sf.rds")
if (!file.exists(counties_file)){
  saveRDS(counties_sf, counties_file)
}

zips_sf = zip_codes |> 
  select(objectid = OBJECTID, zip = ZIP_CODE, area_sqmi = SQMI) |> 
  left_join(zip_county) |> 
  left_join(zip_city)

zips_file = file.path("data", "zips_sf.rds")
if (!file.exists(zips_file)){
  saveRDS(zips_sf, zips_file)
}

# EDA ---------------------------------------------------------------------

counties_pop = readxl::read_xlsx(file.path("data", "Vehicle_Population.xlsx"),
                                 sheet = "County") |> 
  setNames(c("year", "county", "fuel_type_group", "fuel_type", "make", "model", "count")) |> 
  filter(!(county %in% c("Out Of State", "Out of State"))) 

zips_pop = readxl::read_xlsx(file.path("data", "Vehicle_Population.xlsx"),
                             sheet = "ZIP") |> 
  setNames(c("year", "fuel_type_group", "fuel_type", "zip", "count")) |> 
  mutate(zip = as.character(zip)) 

# prop of vehicle count in zip codes that aren't represented in the spatial layer
sum(zips_pop$count[is.na(zips_pop$prop_inc)])/sum(zips_pop$count)

# compare county-level summary based on where spatial intersection occurred
cp_summ = counties_pop |> 
  group_by(county) |> 
  summarise(count = sum(count, na.rm = TRUE)) |> 
  left_join(counties_sf |> 
              st_drop_geometry() |> 
              select(county, area_sqmi, prop_inc)) |> 
  mutate(inc_count_cp = count * prop_inc)

zp_summ = zips_pop |> 
  group_by(zip) |> 
  summarise(count = sum(count, na.rm = TRUE)) |> 
  left_join(zips_sf |> 
              st_drop_geometry() |> 
              select(zip, county, prop_inc)) |> 
  mutate(inc_count_zp = count * prop_inc) |> 
  group_by(county) |> 
  summarise(inc_count_zp = sum(inc_count_zp, na.rm = TRUE))

comp = left_join(zp_summ, cp_summ)

library(ggplot2)

ggplot(comp, aes(x = inc_count_zp, y = inc_count_cp)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)
