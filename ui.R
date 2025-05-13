
page_navbar(
  title = "Light-Duty Vehicle Population in California",
  window_title = "Light-Duty Vehicles",
  id = "nav",
  sidebar = sidebar(
    width = 320,
    conditionalPanel(
      condition = 'input.display == "Bar Plot"',
      sliderInput(inputId = "year", label = "Year", sep = "", step = 1, 
                  min = year_min, year_max, value = year_max, 
                  animate = animationOptions(interval = 2500))
    ),
    conditionalPanel(
      condition = 'input.display != "Bar Plot"',
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
                                 `selected-text-format` = "count > 3"))
    ),
    conditionalPanel(
      condition = 'input.map_filter == "zip"',
      pickerInput(inputId = "zips", label = "Zip Codes", multiple = TRUE, 
                  choices = zips, selected = zips,
                  options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 8,
                                 `selected-text-format` = "count > 5"))
    ),
    pickerInput(inputId = "fuel_types", label = "Fuel Types", multiple = TRUE, 
                choices = fuel_types, selected = zevs,
                options = list(`actions-box` = TRUE, `live-search` = TRUE,
                               `selected-text-format` = "count > 3")),
    conditionalPanel(
      condition = 'input.display == "Table"',
      downloadButton("download", "Download Table", icon = icon("download"))
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
      col_widths = c(7, 5),
      card(
        full_screen = TRUE,
        card_header(
          popover(
            bsicons::bs_icon("gear", class = "ms-auto"), 
            title = "Display Settings", 
            selectInput(inputId = "display", label = "Display Type", 
                        choices = c("Bar Plot", "Time Series Plot", "Table")),
            conditionalPanel(
              condition = 'input.display != "Table"',
              selectInput("resp", "Vehicle Population", choices = resp_opts,
                          selected = "count")
            )
          ),
          class = "d-flex justify-content-center"
        ),
        conditionalPanel(
          condition = 'input.display == "Bar Plot"',
          plotlyOutput("barPlot"),
          plotlyOutput("tsPlot")
        ),
        # conditionalPanel(
        #   condition = 'input.display == "Time Series Plot"',
        #   plotlyOutput("tsPlot")
        # ),
        conditionalPanel(
          condition = 'input.display == "Table"',
          reactableOutput("table") |> 
            tooltip("The aggregated row shows the number of years in the Year 
                      column and the max number of vehicles across all selected 
                      years in the fuel type columns.")
        )
      ),
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
        leafletOutput("map") |> 
          tooltip(uiOutput("mapInfo"))
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

