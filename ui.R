
page_navbar(
  title = "Light-Duty Vehicle Population in California",
  window_title = "Light-Duty Vehicles",
  id = "nav",
  sidebar = sidebar(
    width = 320,
    pickerInput(inputId = "fuel_types", label = "Fuel Types", multiple = TRUE, 
                choices = fuel_types, selected = zevs,
                options = list(`actions-box` = TRUE, `live-search` = TRUE,
                               `selected-text-format` = "count > 3")),
    sliderInput(inputId = "year", label = "Year", sep = "", step = 1, 
                min = year_min, year_max, value = year_max, 
                animate = animationOptions(interval = 2500)),
    radioButtons("map_filter", "Map Filter", choices = map_opts, 
                 selected = "county", inline = TRUE),
    conditionalPanel(
      condition = 'input.map_filter == "county"',
      pickerInput(inputId = "counties", label = "Counties", multiple = TRUE, 
                  choices = counties, selected = counties,
                  options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 8,
                                 `selected-text-format` = "count > 3")) |>
        tooltip("Counties can also be selected by drawing polygons
                  with the draw toolbar on the left side of the map.")
    ),
    conditionalPanel(
      condition = 'input.map_filter == "zip"',
      pickerInput(inputId = "zips", label = "Zip Codes", multiple = TRUE, 
                  choices = zips, selected = zips,
                  options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 8,
                                 `selected-text-format` = "count > 5")) |>
        tooltip("Zip codes can also be selected by drawing polygons
                  with the draw toolbar on the left side of the map.")
    ),
    br(),
    a(img(src="ESA-small.png", alt="ESA logo", width = "200"), 
      href = "https://esassoc.com/",
      target = "_blank"),
    helpText("For issues with this app, contact Travis Hinkelman (thinkelman@esassoc.com).")
  ),
  nav_panel(
    title = "App",
    layout_columns(
      col_widths = c(5, 7),
      card(
        full_screen = TRUE,
        card_header(
          popover(
            bsicons::bs_icon("gear", class = "ms-auto"),
            title = "Map Settings",
            selectInput("resp_map", "Vehicle Population", choices = resp_map_opts,
                        selected = "count")
          ),
          class = "d-flex justify-content-between"
        ),
        leafletOutput("map")
      ),
      card(
        card_header(
          popover(
            bsicons::bs_icon("gear", class = "ms-auto"), 
            title = "Plot Settings", 
            selectInput("resp", "Vehicle Population", choices = resp_opts,
                        selected = "count")
          ),
          class = "d-flex justify-content-center"
        ),
        conditionalPanel(
          condition = "input.fuel_types.length > 1",
          plotlyOutput("barPlot")
        ),
        plotlyOutput("tsPlot")
      )
    )
  ),
  nav_spacer(),
  nav_item(
    a("Data", href = "https://www.energy.ca.gov/files/zev-and-infrastructure-stats-data",
      target = "_blank")
  ),
  nav_item(
    a("Code", href = "https://github.com/EnvironmentalScienceAssociates/light-duty-vehicle-population",
      target = "_blank")
  )
)

