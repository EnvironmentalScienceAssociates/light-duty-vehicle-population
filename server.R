
function(input, output, session) {
  
  # Vehicle Population ------------------------------------------------------
  
  popSub1 <- reactive({
    dfx = if (input$map_filter == "county") county_pop else zip_pop
    if (input$display != "Bar Plot"){
      dfx = dfx[dfx[["year"]] >= input$years[1] & dfx[["year"]] <= input$years[2], ]
    }
    if (input$display == "Bar Plot"){
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
    total = if (input$resp == "percent_selected") popSummTotalFT() else popSummTotal()
    
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
      pt = if (input$resp == "percent_selected") "Selected" else "All"
      paste("Percent of", pt, "Vehicle Types")
    }
  })
  
  resp <- reactive({
    if (grepl("percent", input$resp)) "percent" else input$resp
  })
  
  output$barPlot <- renderPlotly({
    req(input$display == "Bar Plot", nrow(popSumm()) > 0)
    p = ggplot(popSumm(),
               aes(x = fuel_type, y = .data[[resp()]], fill = fuel_type, text = tooltip_text)) +
      geom_bar(stat = "identity", width = 0.5) +
      labs(y = yLab(), title = paste("Year:", input$year)) +
      scale_fill_manual(name = "Fuel Type", values = fuel_type_colors) +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank())
    
    ggplotly(p, tooltip = "text")
  })
  
  output$tsPlot <- renderPlotly({
    req(nrow(popSumm()) > 0) # input$display == "Time Series Plot", 
    p = ggplot(popSumm(),
               aes(x = year, y = .data[[resp()]], color = fuel_type,
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
    req(input$display == "Table", nrow(tableData()) > 0)
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
  
  observeEvent(input$map_filter,{
    opts = if (input$map_filter == "county") resp_map_opts else resp_map_opts[1:2]
    updateSelectInput(session, "resp_map", choices = opts)
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
    
    pop = popSub3() |>
      group_by(across(all_of(c(input$map_filter, "year")))) |>
      summarise(count = sum(count, na.rm = TRUE)) |>
      group_by(across(all_of(c(input$map_filter)))) |>
      summarise(year = year[count == max(count, na.rm = TRUE)][1],
                count = max(count, na.rm = TRUE))
    
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
                               "Incorporated: ", area_inc, " sq. mi."))
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
  
  output$mapInfo <- renderUI({
    draw_info = "Counties and zip codes can be selected by drawing polygons
                with the draw toolbar on the left side of the map."
    if (input$display == "Bar Plot"){
      out = paste(draw_info, "<br><br>Map colors represent values from the
                  selected year.")
    }
    if (input$display != "Bar Plot"){
      out = paste(draw_info, "<br><br>Map colors represent values from the year 
      with the max number of vehicles in the selected year range.")
    }
    HTML(out)
  })
  
}

