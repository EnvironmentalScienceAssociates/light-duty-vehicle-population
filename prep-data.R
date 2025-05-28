library(sf)
library(dplyr)

if (!dir.exists("data")) dir.create("data")

# Human Population --------------------------------------------------------

prep_census <- function(data){
  data |> 
    filter(STNAME == "California" & COUNTY != 0) |> 
    select(county = CTYNAME, starts_with("POPEST")) |> 
    tidyr::pivot_longer(cols = !county, names_to = "year", values_to = "popest") |> 
    mutate(county = sub(" County", "", county),
           year = as.numeric(sub("POPESTIMATE", "", year)))
}

base_url = "https://www2.census.gov/programs-surveys/popest/datasets"

# https://www.census.gov/data/datasets/time-series/demo/popest/2010s-counties-total.html
county_popest = prep_census(read.csv(paste0(base_url, "/2010-2019/counties/totals/co-est2019-alldata.csv")))  |> 
  # https://www.census.gov/data/datasets/time-series/demo/popest/2020s-counties-total.html
  bind_rows(prep_census(read.csv(paste0(base_url, "/2020-2024/counties/totals/co-est2024-alldata.csv"))))
saveRDS(county_popest, file.path("data", "county_popest.rds"))

# Vehicle Population ------------------------------------------------------

# https://www.energy.ca.gov/files/zev-and-infrastructure-stats-data
# file will change annually in April or May and the url will need to be updated
ld_file = file.path("data", "Vehicle_Population.xlsx")
if (!file.exists(ld_file)){
  download.file("https://www.energy.ca.gov/filebrowser/download/7591?fid=7591", 
                ld_file, mode = "wb")
}

ft_lev = c("Battery Electric (BEV)", "Plug-in Hybrid (PHEV)", "Fuel Cell (FCEV)", 
           "Gasoline Hybrid", "Gasoline", "Diesel", "Other")

zip_pop = readxl::read_xlsx(file.path("data", "Vehicle_Population.xlsx"),
                            sheet = "ZIP") |> 
  setNames(c("year", "fuel_type_group", "fuel_type", "zip", "count")) |> 
  group_by(zip, year, fuel_type = fuel_type_group) |> 
  summarise(count = sum(count, na.rm = TRUE)) |> 
  mutate(zip = as.character(zip),
         fuel_type = factor(fuel_type, levels = ft_lev))
saveRDS(zip_pop, file.path("data", "zip_pop.rds"))

county_pop = readxl::read_xlsx(file.path("data", "Vehicle_Population.xlsx"),
                               sheet = "County") |> 
  setNames(c("year", "county", "fuel_type_group", "fuel_type", "make", "model", "count")) |> 
  mutate(county = ifelse(county == "Out Of State", "Out of State", county)) |> 
  group_by(county, year, fuel_type = fuel_type_group) |> 
  summarise(count = sum(count, na.rm = TRUE)) |> 
  mutate(fuel_type = factor(fuel_type, levels = ft_lev))
saveRDS(county_pop, file.path("data", "county_pop.rds"))

file.remove(ld_file)

# Spatial -----------------------------------------------------------------

# https://gis.data.ca.gov/datasets/CALFIRE-Forestry::california-incorporated-cities-1/about
cities = read_sf("https://egis.fire.ca.gov/arcgis/rest/services/FRAP/Incorp/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") |> 
  st_make_valid()

city_sf = cities |> 
  select(objectid = OBJECTID, city = CITY, county = COUNTY) |> 
  mutate(county = sub(" County", "", county))

# https://gis.data.ca.gov/datasets/California::california-county-boundaries-and-identifiers/about
counties = read_sf("https://services3.arcgis.com/uknczv4rpevve42E/arcgis/rest/services/California_County_Boundaries_and_Identifiers_Blue_Version_view/FeatureServer/1/query?outFields=*&where=1%3D1&f=geojson") |> 
  st_make_valid()

county_sf = counties |> 
  select(objectid = OBJECTID, county = CDT_NAME_SHORT)

# https://gis.data.ca.gov/datasets/CDEGIS::california-zip-codes/about
zip_codes = read_sf("https://services3.arcgis.com/fdvHcZVgB2QSRNkL/arcgis/rest/services/ZipCodes/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson") |> 
  st_make_valid()

zip_sf = zip_codes |> 
  select(objectid = OBJECTID, zip = ZIP_CODE)

# # not all zip codes are geographically based (e.g., PO boxes, military installations)
# # zipcodeR package includes a zip_code_db that includes coordinates for some of our missing zip codes
# zip_db_tmp = zipcodeR::zip_code_db |>
#   select(zip = zipcode, major_city, county, state, lat, lon = lng) |>
#   mutate(county = sub(" County", "", county)) |>
#   filter(state %in% c("CA", "NV", "OR") &
#            zip %in% zip_pop$zip & !(zip %in% zip_sf$zip))
# 
# zip_db_sf = zip_db_tmp |>
#   filter(!is.na(lat)) |>
#   st_as_sf(coords = c("lon", "lat"), crs = 4326)
# saveRDS(zip_db_sf, file.path("data", "zip_db_sf.rds"))
# 
# # when users in the app select zip codes spatially,
# # the zip codes in zip_db_county will be selected based on county
# # these zip codes will not be included in the incorporated/unincorporated distinction
# zip_db_county = zip_db_tmp |> 
#   filter(is.na(lat) & !is.na(county)) |> 
#   select(-lat, -lon)
# saveRDS(zip_db_county, file.path("data", "zip_db_county.rds"))

# library(leaflet)
# 
# leaflet(options = leafletOptions(attributionControl = FALSE)) |>
#   setView(lng = -120, lat = 37.5, zoom = 6) |>
#   addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") |>
#   addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
#   addLayersControl(baseGroups = c("Topo", "Satellite"),
#                    options = layersControlOptions(collapsed = FALSE)) |>
#   addPolygons(data = county_sf,
#               label = ~county,
#               color = "black",
#               weight = 2,
#               fillOpacity = 0) |>
#   # addPolygons(data = zip_sf,
#   #             label = ~zip,
#   #             weight = 2,
#   #             fillOpacity = 0) |>
#   addPolygons(data = city_sf,
#               label = ~city,
#               popup = ~county,
#               color = "black",
#               weight = 2,
#               fillOpacity = 0)

## Intersection ------------------------------------------------------------

to_sqmi <- function(x){
  round(units::drop_units(units::set_units(x, mi^2)), 1)
}

intersects <- function(x, y, x_col, y_col){
  # st_intersection for finding overlap is slow so using st_intersects first
  inter = st_intersects(x, y)
  out = list()
  for (i in 1:nrow(x)){
    cat("\rRow", i, "of", nrow(x))
    x_sub = x[i,]
    y_sub = y[inter[[i]],]
    if (nrow(y_sub) > 0){
      y_val = y_sub[[y_col]]
      st_int = st_intersection(x_sub, y_sub)
      area_inc = to_sqmi(st_area(st_int))
    } else {
      y_val = NA
      area_inc = 0
    }
    out[[i]] = data.frame(c1 = x_sub[[x_col]],
                          c2 = y_val,
                          area_sqmi = to_sqmi(st_area(x_sub)),
                          area_inc = area_inc) |> 
      setNames(c(x_col, y_col, "area_sqmi", "area_inc"))
  }
  out
}

county_city = bind_rows(intersects(county_sf, city_sf, "county", "city"))

county_inc = county_city |> 
  group_by(county, area_sqmi) |> 
  summarise(area_inc = sum(area_inc, na.rm = TRUE)) 

county_sf = left_join(county_sf, county_inc) |> 
  rmapshaper::ms_simplify()
saveRDS(county_sf, file.path("data", "county_sf.rds"))

zip_city = bind_rows(intersects(zip_sf, city_sf, "zip", "city"))

zip_inc = zip_city |> 
  group_by(zip, area_sqmi) |> 
  summarise(area_inc = sum(area_inc, na.rm = TRUE))

zip_sf = left_join(zip_sf, zip_inc) |> 
  rmapshaper::ms_simplify()
saveRDS(zip_sf, file.path("data", "zip_sf.rds"))

# zip_inc_point = zip_db_sf |> 
#   st_join(select(city_sf, city), join = st_within) |> 
#   st_drop_geometry() |> 
#   mutate(zip_inc = ifelse(!is.na(city), 1, 0)) |> 
#   select(zip, zip_inc)

# # initially, I explored scaling up the zip code incorporated to the county level
# # but the problems with linking zip codes to geography and with zip codes potentially
# # spanning more than one county meant that I decided to simplify things for now
# county_zip = bind_rows(intersects(county_sf, zip_sf, "county", "zip"))
# county_zip_inc = county_zip |> 
#   left_join(zip_inc_poly) |> 
#   mutate(prop_inc = zip_overlap * zip_inc)
