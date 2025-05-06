
page_sidebar(
  title = "Light-Duty Vehicle Population in California",
  window_title = "Light-Duty Vehicles",
  sidebar = sidebar(
    width = 320,
    conditionalPanel(
      condition = 'input.nav == "Bar Plot"',
      sliderInput(inputId = "year", label = "Year", sep = "", step = 1, 
                  min = year_min, year_max, value = year_max, 
                  animate = animationOptions(interval = 2500))
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
      condition = 'input.nav == "Table"',
      downloadButton("download", "Download Table", icon = icon("download"))
    ),
    br(),
    br(),
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
      header = card_header(
        tooltip(bsicons::bs_icon("info-circle", class = "ms-auto"),
                textOutput("plotInfo")),
        popover(bsicons::bs_icon("gear", class = "ms-auto"),
                selectInput("resp", "Vehicle Population", choices = resp_opts, 
                            selected = "count"),
                title = "Plot Settings"),
        class = "d-flex justify-content-center"
      ),
      nav_panel(
        title = "Bar Plot",
        plotlyOutput("barPlot")
      ),
      nav_spacer(),
      nav_panel(
        title = "Time Series Plot",
        plotlyOutput("tsPlot")
      ),
      nav_spacer(),
      nav_panel(
        title = "Table",
        reactableOutput("table")
      )
    ),
    navset_card_underline(
      full_screen = TRUE,
      header = card_header(
        tooltip(bsicons::bs_icon("info-circle", class = "ms-auto"),
                textOutput("mapInfo")),
        popover(bsicons::bs_icon("gear", class = "ms-auto"),
                selectInput("resp_map", "Vehicle Population", choices = resp_map_opts,
                            selected = "count"),
                title = "Plot Settings"), 
        class = "d-flex justify-content-between"
      ),
      nav_panel(
        title = "Map",
        leafletOutput("map")
      ),
      nav_spacer(),
      nav_menu(
        title = "Links",
        nav_item(HTML('<a href="https://www.energy.ca.gov/files/zev-and-infrastructure-stats-data" target="_blank">Data</a>')),
        nav_item(HTML('<a href="https://github.com/EnvironmentalScienceAssociates/light-duty-vehicle-population" target="_blank">Code</a>'))
      )
    )
  )
)

