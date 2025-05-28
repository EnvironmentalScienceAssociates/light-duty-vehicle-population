
function(input, output, session) {
  
  # Vehicle Population ------------------------------------------------------
  
  rv <- reactiveValues(shape = NULL, resp_last = "count", resp_map_last = "count")
  
  observe({
    opts = if (length(input$fuel_types) > 1) resp_opts else resp_opts[resp_opts != "percent_selected"]
    if (rv$resp_last != input$resp) rv$resp_last = input$resp
    sel = if(rv$resp_last %in% opts) rv$resp_last else opts[1]
    updateSelectInput(session, "resp", choices = opts, selected = sel)
  })
  
  czSub <- reactive({
    # cz = county zip
    if (input$map_filter == "county"){
      dfx = county_pop
      dfx = dfx[dfx[["county"]] %in% input$counties, ]
    }
    if (input$map_filter == "zip"){
      dfx = zip_pop
      dfx = dfx[dfx[["zip"]] %in% input$zips, ]
    }
    dfx
  })
  
  czYearSub <- reactive({
    dfx = czSub()
    dfx[dfx[["year"]] == input$year, ]
  })
  
  czYearFuelSub <- reactive({
    dfx = czYearSub()
    dfx[dfx[["fuel_type"]] %in% input$fuel_types, ]
  })
  
  czFuelSub <- reactive({
    dfx = czSub()
    dfx[dfx[["fuel_type"]] %in% input$fuel_types, ]
  })

  barSummTotal <- reactive({
    dfx = if (input$resp == "percent_selected") czYearFuelSub() else czYearSub()
    dfx |> 
      group_by(year) |> 
      summarise(total = sum(count, na.rm = TRUE))
  })
  
  barSumm <- reactive({
    czYearFuelSub() |>
      group_by(year, fuel_type) |> 
      summarise(count = sum(count, na.rm = TRUE)) |> 
      left_join(barSummTotal(), by = join_by(year)) |> 
      mutate(percent = round(count/total * 100, 2),
             tooltip_text = paste0("Year: ", year, "<br>",
                                   "Fuel Type: ", fuel_type, "<br>",
                                   "Vehicles: ", count, " (", percent, "%)"))
  })
  
  yLab <- reactive({
    if (input$resp == "count") {
      "Number of Vehicles" 
    } else { 
      pt = if (input$resp == "percent_selected") "Selected" else "All"
      paste("Percent of", pt, "Vehicle Types")
    }
  })
  
  resp <- reactive({
    if (grepl("percent", input$resp)) "percent" else input$resp
  })
  
  output$barPlot <- renderPlotly({
    req(nrow(barSumm()) > 0)
    p = ggplot(barSumm(),
               aes(x = fuel_type, y = .data[[resp()]], fill = fuel_type, text = tooltip_text)) +
      geom_bar(stat = "identity", width = 0.5) +
      labs(y = yLab(), title = paste("Year:", input$year)) +
      scale_fill_manual(name = "Fuel Type", values = fuel_type_colors) +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank())
    
    ggplotly(p, tooltip = "text")
  })
  
  tsSummTotal <- reactive({
    dfx = if (input$resp == "percent_selected") czFuelSub() else czSub()
    dfx |> 
      group_by(year) |> 
      summarise(total = sum(count, na.rm = TRUE))
  })
  
  tsSumm <- reactive({
    czFuelSub() |>
      group_by(year, fuel_type) |> 
      summarise(count = sum(count, na.rm = TRUE)) |> 
      left_join(tsSummTotal(), by = join_by(year)) |> 
      mutate(percent = round(count/total * 100, 2),
             tooltip_text = paste0("Year: ", year, "<br>",
                                   "Fuel Type: ", fuel_type, "<br>",
                                   "Vehicles: ", count, " (", percent, "%)"))
  })
  
  output$tsPlot <- renderPlotly({
    req(nrow(tsSumm()) > 0)
    p = ggplot(tsSumm(),
               aes(x = year, y = .data[[resp()]], color = fuel_type,
                   group = fuel_type, text = tooltip_text)) +
      geom_line(alpha = 0.5) +
      # geom_point(alpha = 0.8) +
      geom_point(data = barSumm(), size = 2) +
      labs(x = "Year", y = yLab()) +
      scale_color_manual(name = "Fuel Type", values = fuel_type_colors) +
      theme_minimal() 
    
    if (length(input$fuel_types) > 1) p = p + theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  })
  
  # Spatial -----------------------------------------------------------------
  
  observeEvent(input$map_filter,{
    opts = if (input$map_filter == "county") resp_map_opts else resp_map_opts[resp_map_opts != "per_capita"]
    if (rv$resp_map_last != input$resp_map) rv$resp_map_last = input$resp_map
    sel = if(rv$resp_map_last %in% opts) rv$resp_map_last else opts[1]
    updateSelectInput(session, "resp_map", choices = opts, selected = sel)
  })

  spatialLayer <- reactive({
    if (input$map_filter == "county") county_sf else zip_sf
  })

  spatialSub <- reactive({
    dfx = spatialLayer()
    if (input$map_filter == "county"){
      dfx = dfx[dfx[["county"]] %in% input$counties, ]
    }
    if (input$map_filter == "zip"){
      dfx = dfx[dfx[["zip"]] %in% input$zips, ]
    }
    dfx
  })

  spatialPopSumm <- reactive({
    top_row <- function(type, x){
      if (type == "county"){
        paste(x, "County")
      } else {
        paste("ZIP", x)
      }
    }

    pop = czYearFuelSub() |>
      group_by(across(all_of(c(input$map_filter, "year")))) |>
      summarise(count = sum(count, na.rm = TRUE))

    tmp = spatialSub() |>
      left_join(pop, by = input$map_filter) |>
      filter(count > 0) |>
      mutate(per_area = count/area_sqmi,
             popup = paste0("<strong>", top_row(input$map_filter, .data[[input$map_filter]]),
                            " (", year, ")</strong><br>",
                            "<strong>Vehicles</strong><br>",
                            count, " total<br>"))

    if (input$map_filter == "county"){
      popest_sub = county_popest[county_popest[["county"]] %in% input$counties,]
      tmp = tmp |>
        left_join(popest_sub, by = join_by(county, year)) |>
        mutate(per_capita = count/popest,
               popup = paste0(popup, round(per_capita, 3), " per capita<br>"))
    }

    mutate(tmp, popup = paste0(popup, round(per_area, 3), " per sq. mi.<br>",
                               "<strong>Area</strong><br>",
                               "Total: ", area_sqmi, " sq. mi.<br>",
                               "Incorporated: ", area_inter, " sq. mi."))
  })

  output$map = renderLeaflet({
    leaflet(options = leafletOptions(attributionControl = FALSE)) |>
      setView(lng = -120, lat = 37.5, zoom = 6) |>
      addProviderTiles(providers$Esri.WorldGrayCanvas) |>
      addDrawToolbar(
        targetGroup = "draw",
        singleFeature = TRUE,
        polylineOptions = FALSE,
        circleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))
  })

  observeEvent(input$map_draw_new_feature, {
    rv$shape = geojsonsf::geojson_sf(jsonify::to_json(input$map_draw_new_feature, unbox = TRUE))
  })

  observeEvent(input$map_draw_edited_features, {
    rv$shape = geojsonsf::geojson_sf(jsonify::to_json(input$map_draw_edited_features, unbox = TRUE))
  })

  observeEvent(input$map_draw_deleted_features, {
    rv$shape = NULL
  })

  observeEvent(rv$shape, {
    # update both at same time so that it is easy to switch from county to zip
    # with the same polygon select
    county_inter = st_join(county_sf, rv$shape) |>
      filter(!is.na(feature_type))
    updatePickerInput(session, "counties", selected = county_inter$county)

    zip_inter = st_join(zip_sf, rv$shape) |>
      filter(!is.na(feature_type))
    updatePickerInput(session, "zips", selected = zip_inter$zip)
  })

  proxy <- leafletProxy("map")

  observe({
    dfx = spatialPopSumm()
    # clear everything when dfx changes
    proxy |>
      clearGroup("poly") 
    
    # only remap when dfx has data
    req(nrow(dfx) > 0)
    
    proxy |>
      addPolygons(data = dfx,
                  weight = 1,
                  opacity = 0.1,
                  color = "black",
                  fillOpacity = 0.7,
                  fillColor = colorNumeric("Blues", dfx[[input$resp_map]])(dfx[[input$resp_map]]),
                  label = dfx[[input$map_filter]],
                  popup = ~popup,
                  group = "poly")
  })

}

