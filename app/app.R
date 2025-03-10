library(shiny)
library(shinyWidgets)
library(leaflet)
library(bslib)
library(dplyr)
library(ggplot2)
library(plotly)
library(sf)

# read in vector layers
ipcc <- read_sf("app_data/IPCC_regions/referenceRegions.shp")
countries <- read_sf("app_data/countries.shp") %>% arrange(name)

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
  fg = "#FFFFFF",
  primary = "#00bc8c",
  secondary = "#BC0032"
)

ui <- page_sidebar(
  theme = my_theme,
  #includeCSS("www/style.css"),
  title = "Human Footprint Index Dashboard",
  sidebar = sidebar(
    radioGroupButtons(
      "map_type",
      choices = c("Annual Map", "Change Map"),
      selected = "Annual Map"
    ), 
    #title = "Controls",
    sliderInput("year", 
                "Select Year:",
                min = min(years),
                max = max(years),
                value = max(years),
                step = 24,
                sep = ""),
    em("For change maps, must select a year greater than 1999"),
    accordion(open = FALSE,
              accordion_panel(
                "Regional Summaries",
                radioGroupButtons(
                  "level",
                  "Summarize By:",
                  choices = c("Country", "IPCC Region"),
                  selected = "Country"
                ),            
                conditionalPanel(
                  "input.level == 'Country'",
                  materialSwitch("add_country", "Add Layer to Map:", value = FALSE),
                  selectizeInput(
                    "country",
                    "Select Country",
                    choices = unique(countries$name),
                    options = list(
                      placeholder = 'Please select an option below',
                      onInitialize = I('function() { this.setValue(""); }')
                    )
                  ),
                  plotlyOutput("country_histogram", height = "600px")
                )
              )
            ),     
    card(
      full_screen = TRUE,
      card_header("Mean Annual Change"),
      plotlyOutput("timeSeries", height = "300px")
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
  
  # reactive color palette for country summaries
  country_pal <- reactive({
    
    var <- paste0(input$year, "_int16")
    
    colorNumeric(palette = c("#A1E8A1", "#FF6600"), domain = countries$var)
    
  })
  
  
  # Initial map output
  output$map <- renderLeaflet({
    leaflet() %>%
      #addProviderTiles("OpenStreetMap") %>% 
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(lng = 0, lat = 30, zoom = 2)
      #addPolygons(data = countries, color = "#00bc8c", weight = 1, fillOpacity = 0, popup = ~name, group = "Countries") %>% 
      # addLayersControl(
      #   overlayGroups = c("Countries", "Borders"),  # Specify the layers to toggle
      #   options = layersControlOptions(collapsed = FALSE)  # Control options
      # )
  })
  
  # this makes it so the proxy map is rendered in the background, otherwise the map is empty when you first navigate to this page
  outputOptions(output, "map", suspendWhenHidden=FALSE)
  
  # change tile url based on selected year and map type
  
  url <- reactive({
    if (input$map_type == "Annual Map") {
      paste0(
        "https://tiles.arcgis.com/tiles/swlKRWoduvVuwMcX/arcgis/rest/services/TP_",
        input$year,
        "_3857_12levels/MapServer/tile/{z}/{y}/{x}"
      )
    } else if (input$map_type == "Change Map") {
      if (input$year == 1999) {
        paste0(
          "https://tiles.arcgis.com/tiles/swlKRWoduvVuwMcX/arcgis/rest/services/TP_",
          input$year,
          "_3857_12levels/MapServer/tile/{z}/{y}/{x}"
        )
      } else {
        paste0(
          "https://tiles.arcgis.com/tiles/swlKRWoduvVuwMcX/arcgis/rest/services/TP_Change_",
          input$year,
          "_1999_1k/MapServer/tile/{z}/{y}/{x}"
        )
        
      }
    } else {
      return(NULL)
    }
    
  })
  
  # Update map layer
  observe({
    if (is.null(url())) {
      leafletProxy("map") %>%
        clearGroup(c("hfi", "Countries"))
    }
    
    leafletProxy("map") %>%
      clearGroup(c("hfi", "Countries")) %>%
      clearControls() %>% 
      addTiles(
        url(),
        group = "hfi",
        options = tileOptions(maxNativeZoom = 12)
      )
    
    # Add country layers
    if(input$add_country) {
        leafletProxy("map") %>%
        addPolygons(
          data = countries,
          fillColor = ~country_pal()(get(paste0(input$year, "_int16"))),
          fillOpacity = 0.85,
          weight = 0.5,
          color = "#444444",
          group = "Countries", 
          #label = ~ paste0(var_labels[input$socio_layers], ": ", format(round(get(input$socio_layers), 0), big.mark = ",")),
          popup = ~ paste(
            "<strong>",name, "</strong>",
            "<br>", paste(input$year, "Mean HFI:"),
            round(get(paste0(input$year, "_int16")), 2)
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal = country_pal(),
          values = countries[[paste0(input$year, "_int16")]],
          title = "Average HFI by Country",
          group = "Countries"
        )
    } else {
      leafletProxy("map") %>% 
        clearShapes() %>% 
        clearControls()
    }

  })
  
  
  
  ### zoom to region ------
  observeEvent(input$country, {
    if (input$country == "") {
      leafletProxy("map")
    } else {
      zoom <- reactive({
        countries %>%
          filter(name == input$country) %>%
          st_bbox() %>%
          st_as_sfc(crs = st_crs(countries)) %>%
          st_centroid() %>%
          st_coordinates()
        
      })
      
      leafletProxy('map') %>%
        setView(lng = zoom()[1],
                lat = zoom()[2],
                zoom = 6)
    }
    
    
  })
  
  
  # country histogram --------------------------
  output$country_histogram <- renderPlotly({
    
    # Calculate histogram bins for all countries
    hist_data <- reactive({
      countries %>%
        st_drop_geometry() %>%
        group_by(name) %>%
        summarise(Value = mean(get(paste0(
          input$year, "_int16"
        ))))
    })
    
    # Create the plot
    p <- plot_ly(
      data = hist_data(),
      x = ~Value,
      y = ~reorder(name, -Value),
      type = "bar",
      color = ~ifelse(name == input$country, "Highlighted", "All Countries"),
      colors = c("All Countries" = "lightgray", "Highlighted" = "red")
    ) %>%
      layout(
        #title = "Value Distribution by Country",
        xaxis = list(title = "Mean HFI"),
        yaxis = list(title = ""),
        barmode = "stack"
      )
    
    return(p)
    
  })
  
  
  
  output$timeSeries <- renderPlotly({
    # Data preparation
    avg_data <- locations %>%
      group_by(year) %>%
      summarize(avg_value = mean(value))
    
    # Create Plotly figure
    plot_ly(avg_data, x = ~year, y = ~avg_value, type = "scatter", mode = "lines+markers",
            line = list(color = "#00bc8c", width = 2, shape = "linear"),
            marker = list(color = "#00bc8c", size = 6),
            name = "Global Mean",legendgroup = "avg", showlegend = FALSE) %>%
      # Highlight selected year
      add_trace(data = avg_data %>% filter(year == input$year),
                x = ~year, y = ~avg_value, type = "scatter", mode = "markers",
                marker = list(color = "#BC0032", size = 8), 
                name = "Global Mean",legendgroup = "avg", showlegend = TRUE) %>%
      layout(
        #title = list(text = "Time Series of Average Values", x = 0.5),
        xaxis = list(title = "Year", color = "#FFFFFF"),
        yaxis = list(title = "Average HFI", color = "#FFFFFF"),
        plot_bgcolor = "#222222",
        paper_bgcolor = "#222222",
        font = list(color = "#FFFFFF"),
        margin = list(l = 40, r = 40, t = 60, b = 40),
        legend = list(orientation = "h", x = 0.5, y = -0.2, xanchor = "center") 
      )
  })


}

shinyApp(ui, server)