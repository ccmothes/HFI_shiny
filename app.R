library(shiny)
library(leaflet)
library(bslib)
library(dplyr)
library(ggplot2)

# Generate sample data
set.seed(123)
years <- 1999:2023
locations <- data.frame(
  lat = rep(runif(10, 35, 45), length(years)),
  lng = rep(runif(10, -100, -80), length(years)),
  year = rep(years, each = 10),
  value = runif(10 * length(years), 10, 100)
)

# Create a dark theme
my_theme <- bs_theme(
  version = 5,
  preset = "darkly",
  bg = "#222222",
  fg = "#FFFFFF"
) #%>% 
  # bs_add_rules(
  #   ".irs--shiny .irs-bar { background: #00bc8c; border-top: 1px solid #00bc8c; border-bottom: 1px solid #00bc8c; }
  #    .irs--shiny .irs-handle { border: 3px solid #00bc8c; background-color: #00bc8c; }
  #    .irs--shiny .irs-handle:hover { background-color: #009670; }
  #    .irs--shiny .irs-from, .irs--shiny .irs-to, .irs--shiny .irs-single { background-color: #00bc8c; }"
  # )

ui <- page_sidebar(
  theme = my_theme,
  title = "Human Footprint Index Dashboard",
  sidebar = sidebar(
    #title = "Controls",
    sliderInput("year", 
                "Select Year:",
                min = min(years),
                max = max(years),
                value = min(years),
                step = 24,
                sep = ""),
    card(
      full_screen = TRUE,
      card_header("Global Mean Annual Change"),
      plotOutput("timeSeries", height = "300px")
    ),
    width = "400px"
  ),
  
  # Main content - just the map
  card(
    full_screen = TRUE,
    height = "800px",
    leafletOutput("map", height = "100%")
  )
)

server <- function(input, output, session) {
  
  # Reactive filtered data
  filtered_data <- reactive({
    locations %>% 
      filter(year == input$year)
  })
  
  # Initial map output
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(lng = -50, lat = 20, zoom = 3)  # Changed to show global view
  })
  
  # change tile url based on selected layer
  url <- reactive({
    paste0(
      "https://tiles.arcgis.com/tiles/swlKRWoduvVuwMcX/arcgis/rest/services/TP_",
      input$year,
      "_3857_12levels/MapServer/tile/{z}/{y}/{x}"
    )
  })
  
  # Update map layer
  observe({
    leafletProxy("map") %>%
      clearGroup("hfi") %>%
      addTiles(
        url(),
        group = "hfi",
        options = tileOptions(maxNativeZoom = 12)
      )
  })
  
  # Time series output
  output$timeSeries <- renderPlot({
    locations %>%
      group_by(year) %>%
      summarize(avg_value = mean(value)) %>%
      ggplot(aes(x = year, y = avg_value)) +
      geom_line(color = "#00bc8c", size = 1) +
      geom_point(color = "#00bc8c", size = 3) +
      geom_point(data = . %>% filter(year == input$year),
                 color = "#FF7070", size = 4) +
      theme_dark() +
      labs(x = "Year",
           y = "Average Value",
           title = "Time Series of Average Values") +
      theme(
        plot.title = element_text(hjust = 0.5),
        plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
        plot.background = element_rect(fill = "#222222", color = NA),
        panel.background = element_rect(fill = "#2c2c2c", color = NA),
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "#3c3c3c"),
        panel.grid.minor = element_line(color = "#3c3c3c"),
        text = element_text(color = "#FFFFFF"),
        axis.text = element_text(color = "#FFFFFF"),
        axis.line = element_blank()
      )
  }, bg = "#222222")
  
}

shinyApp(ui, server)