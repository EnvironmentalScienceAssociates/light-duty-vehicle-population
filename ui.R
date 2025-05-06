
page_sidebar(
  title = "Light-Duty Vehicle Population in California",
  window_title = "Light-Duty Vehicles",
  sidebar = sidebar(
    width = 320,
    accordion(
      open = "Filters",
      accordion_panel(
        title = "Filters",
        conditionalPanel(
          condition = 'input.nav == "Bar Plot"',
          sliderInput(inputId = "year", label = "Year", sep = "", step = 1, 
                      min = year_min, year_max, value = year_max, animate = FALSE)
        ),
        conditionalPanel(
          condition = 'input.nav != "Bar Plot"',
          sliderInput(inputId = "years", label = "Years", sep = "", step = 1, 
                      min = year_min, max = year_max, value = c(year_min, year_max))
        ),
        radioButtons("map_filter", "Map Filter", choices = map_opts, 
                     selected = "county", inline = TRUE),
        conditionalPanel(
          condition = 'input.map_filter == "county"',
          pickerInput(inputId = "counties", label = "Counties", multiple = TRUE, 
                      choices = counties, selected = counties,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 8,
                                     `selected-text-format` = "count > 3")) |>
            tooltip("Counties can also be selected by drawing polygons on the map.")
        ),
        conditionalPanel(
          condition = 'input.map_filter == "zip"',
          pickerInput(inputId = "zips", label = "Zip Codes", multiple = TRUE, 
                      choices = zips, selected = zips,
                      options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 8,
                                     `selected-text-format` = "count > 5")) |>
            tooltip("Zip codes can also be selected by drawing polygons on the map.")
        ),
        pickerInput(inputId = "fuel_types", label = "Fuel Types", multiple = TRUE, 
                    choices = fuel_types, selected = zevs,
                    options = list(`actions-box` = TRUE, `live-search` = TRUE,
                                   `selected-text-format` = "count > 3"))
      ),
      accordion_panel(
        title = "Variables",
        radioButtons("resp", "Vehicle Population (plots)", choices = rv_opts, 
                     selected = "count", inline = TRUE),
        radioButtons("percent_type", "Percent Type", choices = c("All", "Selected"), 
                     selected = "All", inline = TRUE),
        radioButtons("resp_map", "Vehicle Population (map)", choices = rv_map_opts, 
                     selected = "count_zev", inline = TRUE),
      )
    ),
    conditionalPanel(
      condition = 'input.nav == "Table"',
      downloadButton("download", "Download Table", icon = icon("download"))
    ),
    a(img(src="ESA-small.png", alt="ESA logo", width = "200"), 
      href = "https://esassoc.com/",
      target = "_blank"),
    helpText("For issues with this app, contact Travis Hinkelman (thinkelman@esassoc.com).")
  ),
  layout_columns(
    col_widths = c(7, 5),
    navset_card_underline(
      id = "nav",
      full_screen = TRUE,
      nav_panel(
        title = "Bar Plot",
        plotlyOutput("barPlot")
      ),
      nav_panel(
        title = "Time Series Plot",
        plotlyOutput("tsPlot")
      ),
      nav_panel(
        title = "Table",
        reactableOutput("table")
      ),
      nav_menu(
        title = "Links",
        nav_item(HTML('<a href="https://www.energy.ca.gov/files/zev-and-infrastructure-stats-data" target="_blank">Data</a>')),
        nav_item(HTML('<a href="https://github.com/EnvironmentalScienceAssociates/light-duty-vehicle-population" target="_blank">Code</a>'))
      )
    ),
    card(
      full_screen = TRUE,
      leafletOutput("map")
    )
  )
)

