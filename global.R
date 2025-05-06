options(dplyr.summarise.inform = FALSE)
library(shiny)
library(bslib)
library(shinyWidgets)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(sf)
library(ggplot2)
library(plotly)
library(reactable)

# added popest later which refers to human population
# pop refers to vehicle population
county_popest = readRDS(file.path("data", "county_popest.rds"))
county_pop = readRDS(file.path("data", "county_pop.rds"))
county_sf = readRDS(file.path("data", "county_sf.rds"))
counties = c(county_sf$county, "Out of State")

zip_pop = readRDS(file.path("data", "zip_pop.rds"))
zips = sort(unique(zip_pop$zip))
zip_sf = readRDS(file.path("data", "zip_sf.rds")) |> 
  filter(zip %in% zips)

fuel_types = levels(county_pop$fuel_type)
fuel_type_colors = c("#66a61e", "#7570b3", "#1b9e77", "#d95f02",  
                     "#a6761d", "#e6ab02", "#e7298a") |> 
  setNames(fuel_types)

zevs = c("Battery Electric (BEV)", "Plug-in Hybrid (PHEV)", "Fuel Cell (FCEV)")

year_min = min(county_pop$year, na.rm = TRUE)
year_max = max(county_pop$year, na.rm = TRUE)

map_opts = c("County" = "county", "Zip Code" = "zip")
resp_opts = c("Count" = "count", "Percent" = "percent")
resp_map_opts = c("Count" = "count", "Per Area" = "per_area", "Per Capita" = "per_capita")

# from toupper documentation
simple_cap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}

