
function(input, output, session) {
  
  # Vehicle Population ------------------------------------------------------
  
  popSub1 <- reactive({
    dfx = if (input$map_filter == "county") county_pop else zip_pop
    if (input$nav != "Bar Plot"){
      dfx = dfx[dfx[["year"]] >= input$years[1] & dfx[["year"]] <= input$years[2], ]
    }
    if (input$nav == "Bar Plot"){
      dfx = dfx[dfx[["year"]] == input$year, ]
    }
    dfx
  })
  
  popSub2 <- reactive({
    dfx = popSub1()
    if (input$map_filter == "county"){
      dfx = dfx[dfx[["county"]] %in% input$counties, ]
    }
    if (input$map_filter == "zip"){
      dfx = dfx[dfx[["zip"]] %in% input$zips, ]
    }
    dfx
  })
  
  popSub3 <- reactive({
    dfx = popSub2()
    dfx = dfx[dfx[["fuel_type"]] %in% input$fuel_types, ]
  })
  
  popSummTotal <- reactive({
    # includes all fuel types
    popSub2() |> 
      group_by(year) |> 
      summarise(total = sum(count, na.rm = TRUE))
  })
  
  popSummTotalFT <- reactive({
    # filtered by fuel_type
    popSub3() |> 
      group_by(year) |> 
      summarise(total = sum(count, na.rm = TRUE))
  })
  
  popSumm <- reactive({
    total = if (input$percent_type == "All") popSummTotal() else popSummTotalFT()
    
    popSub3() |>
      group_by(year, fuel_type) |> 
      summarise(count = sum(count, na.rm = TRUE)) |> 
      left_join(total, by = join_by(year)) |> 
      mutate(percent = round(count/total * 100, 2),
             tooltip_text = paste0("Year: ", year, "<br>",
                                   "Fuel Type: ", fuel_type, "<br>",
                                   "Vehicles: ", count, " (", percent, "%)"))
  })
  
  yLab <- reactive({
    if (input$resp == "count") {
      "Number of Vehicles" 
    } else { 
      paste("Percent of", input$percent_type, "Vehicle Types")
    }
  })
  
  output$barPlot <- renderPlotly({
    req(nrow(popSumm()) > 0)
    p = ggplot(popSumm(),
               aes(x = fuel_type, y = .data[[input$resp]], fill = fuel_type, text = tooltip_text)) +
      geom_bar(stat = "identity", width = 0.5) +
      labs(y = yLab(), title = paste("Year:", input$year)) +
      scale_fill_manual(name = "Fuel Type", values = fuel_type_colors) +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank())
    
    ggplotly(p, tooltip = "text")
  })
  
  output$tsPlot <- renderPlotly({
    req(nrow(popSumm()) > 0)
    p = ggplot(popSumm(),
               aes(x = year, y = .data[[input$resp]], color = fuel_type,
                   group = fuel_type, text = tooltip_text)) +
      geom_line(alpha = 0.5) +
      geom_point(alpha = 0.8) +
      labs(x = "Year", y = yLab()) +
      scale_color_manual(name = "Fuel Type", values = fuel_type_colors) +
      theme_minimal()
    
    ggplotly(p, tooltip = "text")
  })
  
  tableData <- reactive({
    popSub3() |> 
      arrange(across(all_of(c(input$map_filter, "year", "fuel_type")))) |> 
      setNames(c(simple_cap(input$map_filter), "Year", "fuel_type", "count")) |> 
      tidyr::pivot_wider(names_from = fuel_type, values_from = count)
  })
  
  output$table <- renderReactable({
    req(nrow(tableData()) > 0)
    dfx = tableData()
    
    reactable(dfx,
              groupBy = simple_cap(input$map_filter),
              highlight = TRUE,
              fullWidth = TRUE,
              defaultColDef = colDef(
                defaultSortOrder = "desc",
                aggregate = "max",
                headerStyle = list(background = "#f7f7f8")),
              columns = list(Year = colDef(aggregate = "count")),
              showPageSizeOptions = TRUE,
              pageSizeOptions = c(15, 25, 50, 100),
              defaultPageSize = 15)
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("CA-Light-Duty-Vehicle-Population_", 
             simple_cap(input$map_filter), "_", 
             round(as.numeric(Sys.time())), ".csv")
    },
    content = function(file) {
      write.csv(tableData(), file, row.names = FALSE)
    }
  )
  
  # Spatial -----------------------------------------------------------------
  
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
    yr_lbl = "Year with Max ZEVs: "
    zev_lbl = "Max Number of ZEVs: "
    dens_lbl = "Max Density of ZEVs: "
    if (input$nav == "Bar Plot"){
      yr_lbl = "Year: "
      zev_lbl = "Number of ZEVs: "
      dens_lbl = "Density of ZEVs: "
    } 
    
    pop = popSub3() |>
      # already filtered for selected fuel types above
      filter(fuel_type %in% zevs) |> 
      group_by(across(all_of(c(input$map_filter, "year")))) |> 
      summarise(count_zev = sum(count, na.rm = TRUE)) |> 
      group_by(across(all_of(c(input$map_filter)))) |> 
      summarise(year = year[count_zev == max(count_zev, na.rm = TRUE)][1],
                count_zev = max(count_zev, na.rm = TRUE))
    
    spatialSub() |> 
      left_join(pop, by = input$map_filter) |> 
      filter(count_zev > 0) |> 
      mutate(density = round(count_zev/area_sqmi, 2),
             popup = paste0(simple_cap(input$map_filter), ": ", .data[[input$map_filter]], "<br>",                            
                            yr_lbl, year, "<br>",
                            zev_lbl, count_zev, "<br>",
                            dens_lbl, density, " per sq. mi.<br>",
                            "Total Area: ", area_sqmi, " (sq. mi.)<br>",
                            "Incorporated Area: ", area_inc, " (sq. mi.)"))
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
  
  rv <- reactiveValues(shape = NULL)
  
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
    inter = st_join(spatialLayer(), rv$shape) |> 
      filter(!is.na(feature_type))
    
    if (input$map_filter == "county"){
      updatePickerInput(session, "counties", selected = inter$county)
    } else {
      updatePickerInput(session, "zips", selected = inter$zip)
    }
  })
  
  proxy <- leafletProxy("map")
  
  observe({
    req(nrow(spatialPopSumm()) > 0)
    
    dfx = spatialPopSumm()
    
    proxy |>
      clearGroup("poly") |>
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

