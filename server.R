
function(input, output, session) {
  
  output$map = renderLeaflet({
    leaflet(options = leafletOptions(attributionControl = FALSE)) |>
      setView(lng = -120, lat = 37.5, zoom = 6) |>
      addProviderTiles(providers$Esri.WorldGrayCanvas)
  })
  
  countiesPopSub <- reactive({
    dfx = counties_pop
    if (input$nav == "Time Series Plot") dfx = filter(dfx, year >= input$years[1] & year <= input$years[2])
    if (input$nav == "Bar Plot") dfx = filter(dfx, year == input$year)
    filter(dfx, county %in% input$counties &
             fuel_type %in% input$fuel_types)
  })
  
  countiesPopSumm <- reactive({
    countiesPopSub() |> 
      group_by(year, fuel_type) |> 
      summarise(count = sum(count, na.rm = TRUE))
  })
  
  countiesPopZEVSumm <- reactive({
    countiesPopSub() |> 
      filter(fuel_type %in% zevs) |> 
      group_by(year, county) |> 
      summarise(count = sum(count, na.rm = TRUE))
  })
  
  output$barPlot <- renderPlotly({
    p = ggplot(countiesPopSumm(), aes(y = count, x = fuel_type, fill = fuel_type)) +
      geom_bar(stat = "identity", width = 0.5) +
      labs(y = "Number of Vehicles", title = paste("Year:", input$year)) +
      scale_fill_manual(name = "Fuel Type", values = fuel_type_colors) +
      theme_minimal() +
      theme(axis.title.x = element_blank(),
            axis.text.x = element_blank())

    ggplotly(p)
  })

}
