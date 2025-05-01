
page_sidebar(
  title = "Light-Duty Vehicle Population in California",
  window_title = "Light-Duty",
  sidebar = sidebar(
    width = 320,
    conditionalPanel(
      condition = 'input.nav == "Bar Plot"',
      sliderInput(inputId = "year", label = "Year", sep = "", step = 1, 
                  min = year_min, year_max, value = year_max, animate = TRUE)
    ),
    conditionalPanel(
      condition = 'input.nav == "Time Series Plot"',
      sliderInput(inputId = "years", label = "Years", sep = "", step = 1, 
                  min = year_min, max = year_max, value = c(year_min, year_max))
    ),
    radioButtons("map_filter", "Map Filter", choices = c("County", "Zip Code"), 
                 selected = "County", inline = TRUE),
    conditionalPanel(
      condition = 'input.map_filter == "County"',
      pickerInput(inputId = "counties", label = "Counties", multiple = TRUE, 
                  choices = counties, selected = counties,
                  options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 8,
                                 `selected-text-format` = "count > 3"))
    ),
    conditionalPanel(
      condition = 'input.map_filter == "Zip Code"',
      pickerInput(inputId = "zips", label = "Zip Codes", multiple = TRUE, 
                  choices = zips, selected = zips,
                  options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 8,
                                 `selected-text-format` = "count > 5"))
    ),
    pickerInput(inputId = "fuel_types", label = "Fuel Types", multiple = TRUE, 
                choices = fuel_types, selected = zevs,
                options = list(`actions-box` = TRUE, `live-search` = TRUE, size = 4,
                               `selected-text-format` = "count > 3")),
    checkboxGroupInput("inc", "Incorporated Areas", choices = inc_opts,
                       selected = inc_opts) |> 
      tooltip("Assumption is that the vehicle population is distributed in proportion 
      to incorporated and unincorporated area at the zip code level."),
    br(),
    a(img(src="ESA-small.png", alt="ESA logo", width = "200"), 
      href = "https://esassoc.com/",
      target = "_blank"),
    helpText("For issues with this app, contact Travis Hinkelman (thinkelman@esassoc.com).")
  ),
  layout_columns(
    col_widths = c(5, 7),
    card(leafletOutput("map")),
    navset_card_underline(
      id = "nav",
      nav_panel(
        title = "Bar Plot",
        plotlyOutput("barPlot")
      ),
      nav_panel(
        title = "Time Series Plot"
      ),
      nav_menu(
        title = "Links",
        nav_item(HTML('<a href="https://www.energy.ca.gov/files/zev-and-infrastructure-stats-data" target="_blank">Data</a>')),
        nav_item(HTML('<a href="https://github.com/EnvironmentalScienceAssociates/light-duty-vehicle-population" target="_blank">Code</a>'))
      )
    )
  )
)

