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

counties_pop = readRDS(file.path("data", "counties_pop.rds"))
counties_sf = readRDS(file.path("data", "counties_sf.rds"))
zips_pop = readRDS(file.path("data", "zips_pop.rds"))
zips_sf = readRDS(file.path("data", "zips_sf.rds")) |> 
  filter(zip %in% zips_pop$zip)

counties = counties_sf$county
# intentionally excluding zip codes in zips_pop that aren't part of map
zips = zips_sf$zip

fuel_types = levels(counties_pop$fuel_type)
fuel_type_colors = c("#1b9e77", "#d95f02", "#7570b3", "#e7298a", 
                     "#66a61e", "#e6ab02", "#a6761d") |> 
  setNames(fuel_types)

zevs = c("Battery Electric (BEV)", "Plug-in Hybrid (PHEV)", "Fuel Cell (FCEV)")

year_min = min(counties_pop$year, na.rm = TRUE)
year_max = max(counties_pop$year, na.rm = TRUE)

inc_opts = c("Incorporated", "Unincorporated")

