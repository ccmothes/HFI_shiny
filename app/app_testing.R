library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(leaflet)
library(bslib)
library(dplyr)
library(readr)
library(scales)
library(stringr)
library(ggplot2)
library(plotly)
library(sf)

# read in vector layers
ipcc <- read_sf("app_data/IPCC_regions/referenceRegions.shp") %>%
  # clean up names and create shortened title
  mutate(
    NAME = str_to_title(NAME),
    NAME = if_else(
      NAME == "Southern Topical Pacific",
      "Southern Tropical Pacific",
      NAME
    ),
    NAME_short = str_remove(NAME, "\\s*\\[.*\\]")
  ) %>%
  arrange(NAME)

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

# Test time series data
test_props <- read_csv("app_data/proportion_timeseries.csv") %>% 
  mutate(class = case_when(class == "Low" ~ "Natural (<10)",
                           class == "Medium" ~ "Used (>10 & <35)",
                           class == "High" ~ "Settlements (>35)",
                           .default = class)) %>% 
  mutate(class = factor(class, levels = c("Natural (<10)", "Used (>10 & <35)", "Settlements (>35)")))

test_freqs <- read_csv("app_data/distribution_timeseries.csv")

# Create a dark theme
my_theme <- bs_theme(
  version = 5,
  preset = "darkly",
  bg = "#222222",
  fg = "#FFFFFF",
  primary = "#00bc8c",
  secondary = "#BC0032"
)

# UI --------------------------------------------

# Landing Page UI
landing_page_ui <- function() {
  page_fluid(
    theme = my_theme,
    includeCSS("www/style.css"),
    div(
      class = "container py-4",
      style = "max-width: 1200px;",
      
      # Header with title and description
      div(
        class = "p-4 mb-4 rounded-3 text-center",
        style = "background-color: #212426;",
        h1("Human Footprint Index", style = "color: #00bc8c;"),
        p(class = "lead", "Explore global human impact on the environment through interactive visualization tools and data resources"),
        p("The Human Footprint Index (HFI) quantifies human influence on the Earth's land surface based on infrastructure, agriculture, and population density.")
      ),
      
      # Cards section
      # First row - Interactive Data Explorer (full width)
      div(
        class = "row mb-4",
        div(
          class = "col-12",
          div(
            class = "card",
            style = "background-color: #121314; border-color: #FFF;",
            div(
              class = "card-body text-center",
              h3(class = "card-title", "Interactive Data Explorer"),
              p(class = "card-text", "Explore the global Human Footprint Index through an interactive dashboard with regional comparisons, time series analysis, and spatial visualization."),
              div(
                class = "pt-3",
                actionButton(
                  "go_to_explorer", 
                  "Launch Explorer", 
                  icon = icon("globe"), 
                  class = "btn btn-info btn-lg px-5"
                )
              )
            )
          )
        )
      ),
      
      # Second row - Three other cards
      div(
        class = "row row-cols-1 row-cols-md-3 g-4",
        
        # Card 2: Google Earth Engine
        div(
          class = "col",
          div(
            class = "card h-100",
            style = "background-color: #121314; border-color: #FFF;",
            div(
              class = "card-body d-flex flex-column",
              h3(class = "card-title", "Google Earth Engine"),
              p(class = "card-text", "Access the HFI dataset through Google Earth Engine for advanced geospatial analysis and integration with other environmental datasets."),
              div(
                class = "mt-auto pt-3",
                actionButton(
                  "go_to_gee", 
                  "Access Earth Engine", 
                  icon = icon("map"), 
                  class = "btn btn-warning w-100",
                  onclick = "window.open('https://code.earthengine.google.com/f46e81f6ada4c8608b963e6d255efd87', '_blank')"
                )
              )
            )
          )
        ),
        
        # Card 3: REST URL Access
        div(
          class = "col",
          div(
            class = "card h-100",
            style = "background-color: #121314; border-color: #FFF;",
            div(
              class = "card-body d-flex flex-column",
              h3(class = "card-title", "REST URL Access"),
              p(class = "card-text", "Integrate HFI data into your applications with our REST URL endpoints. Fetch specific regions, time periods, or analysis-ready datasets."),
              div(
                class = "mt-auto pt-3",
                actionButton(
                  "go_to_api", 
                  "Access REST URLs", 
                  icon = icon("code"), 
                  class = "btn btn-secondary w-100",
                  onclick = "window.open('https://developers.google.com/earth-engine/datasets/catalog', '_blank')"
                )
              )
            )
          )
        ),
        
        # Card 4: Raw Data Download
        div(
          class = "col",
          div(
            class = "card h-100",
            style = "background-color: #121314; border-color: #FFF;",
            div(
              class = "card-body d-flex flex-column",
              h3(class = "card-title", "Raw Data Download"),
              p(class = "card-text", "Download the complete HFI dataset in various formats (GeoTIFF, CSV, shapefile) for use in your own GIS software or analysis."),
              div(
                class = "mt-auto pt-3",
                actionButton(
                  "go_to_download", 
                  "Download Data", 
                  icon = icon("download"), 
                  class = "btn btn-success w-100",
                  onclick = "window.open('https://mountainscholar.org/home', '_blank')"
                )
              )
            )
          )
        )
      ),
      
      # Footer
      div(
        class = "pt-5 mt-5 text-center",
        p("Copyright © 2025 Human Footprint Index Project", style = "color: #999;"),
        p("Please cite: [Your Citation Here]", style = "color: #999;")
      )
    )
  )
}

# Dashboard UI (your existing UI)
dashboard_ui <- function() {
  page_sidebar(
    theme = my_theme,
    includeCSS("www/style.css"),
    title = div(
      actionButton("go_to_home", "← Home", class = "btn btn-outline-light btn-sm", style = "margin-right: 15px;"),
      "Machine Learning Human Footprint Index (mlHFI) Dashboard"
    ),
    sidebar = sidebar(
      class = "sidebar-scrollable",  # Add custom class for styling
      radioGroupButtons(
        "map_type",
        choices = c("Annual Map", "Change Map"),
        selected = "Annual Map"
      ),
      sliderInput(
        "year",
        "Select Year:",
        min = min(years),
        max = max(years),
        value = max(years),
        step = 24,
        sep = ""
      ),
      em("For change maps, must select a year greater than 1999"),
      ## Regional summaries ------------------
      accordion(
        open = FALSE,
        accordion_panel(
          "Regional Summaries",
          radioGroupButtons(
            "level",
            "Summarize By:",
            choices = c("Country", "IPCC Region"),
            selected = "Country"
          ),
          # Reset view button
          actionButton(
            "reset_view",
            "Reset Map View",
            icon = icon("globe"),
            class = "btn-primary btn-sm",
            style = "margin-top: 10px; width: 100%;"
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
            )
          ),
          conditionalPanel(
            "input.level == 'IPCC Region'",
            materialSwitch("add_ipcc", "Add Layer to Map:", value = FALSE),
            selectizeInput(
              "ipcc",
              "Select IPCC Region",
              choices = unique(ipcc$NAME),
              options = list(
                placeholder = 'Please select an option below',
                onInitialize = I('function() { this.setValue(""); }')
              )
            )
          ),
          ### Tabset viz panel ----------------
          tabsetPanel(
            id = "viz_tabs",
            type = "pills",
            tabPanel(
              "Distribution",
              div(style = "padding-top: 10px;"),
              conditionalPanel(
                "input.level == 'Country'",
                plotlyOutput("country_heatmap", height = "350px")
              ),
              conditionalPanel(
                "input.level == 'IPCC Region'",
                plotlyOutput("ipcc_heatmap", height = "350px")
              )
            ),
            tabPanel(
              "Similarity",
              div(style = "padding-top: 10px;"),
              conditionalPanel(
                "input.level == 'Country'",
                plotlyOutput("country_similar", height = "350px")
              ),
              conditionalPanel(
                "input.level == 'IPCC Region'",
                plotlyOutput("ipcc_similar", height = "300px")
              )
            ),
            tabPanel(
              "Time Series",
              div(style = "padding-top: 10px;"),
              conditionalPanel(
                "input.level == 'Country' && input.country != ''",
                plotlyOutput("country_time_series", height = "350px"),
                hr(),
                plotlyOutput("country_time_props", height = "350px"),
                hr(),
                plotlyOutput("country_time_ridgeline", height = "350px")
              ),
              conditionalPanel(
                "input.level == 'IPCC Region' && input.ipcc != ''",
                plotlyOutput("ipcc_time_series", height = "350px"),
                plotlyOutput("ipcc_proportions", height = "350px")
              ),
              conditionalPanel(
                "(input.level == 'Country' && input.country == '') || (input.level == 'IPCC Region' && input.ipcc == '')",
                div(
                  style = "text-align: center; padding: 50px 20px;",
                  icon("exclamation-circle", style = "font-size: 30px; color: #BC0032;"),
                  h5("Please select a region to view time series data")
                )
              )
            )
          )
        )
      ),
      ### Global mean -------------
      card(
        full_screen = TRUE,
        card_header("Mean Annual Change"),
        plotlyOutput("timeSeries", height = "300px")
      ),
      width = "500px"
    ),
    
    ## Map content -------------
    leafletOutput("map", height = "100%")
  )
}

# Main UI with navigation
ui <- function(request) {
  fluidPage(
    theme = my_theme,
    uiOutput("main_content")
  )
}

# SERVER ------------------------------------------
server <- function(input, output, session) {
  
  # Reactive value to track current page
  current_page <- reactiveVal("landing")
  
  # Main content renderer
  output$main_content <- renderUI({
    if (current_page() == "landing") {
      landing_page_ui()
    } else {
      dashboard_ui()
    }
  })
  
  # Navigation observers
  observeEvent(input$go_to_explorer, {
    current_page("dashboard")
  })
  
  observeEvent(input$go_to_home, {
    current_page("landing")
  })
  
  # Reactive filtered data
  filtered_data <- reactive({
    req(input$year)                     # Only require input exists
    
    locations %>%
      filter(year == input$year)
  })
  
  # reactive color palette for country summaries
  country_pal <- reactive({
    req(input$year)                     # Only require input exists
    
    var <- paste0(input$year, "_int16")
    
    colorNumeric(colorRampPalette(
      c(
        "#ffffff",
        "#fcbba1",
        "#fc9272",
        "#fb6a4a",
        "#ef3b2c",
        "#cb181d",
        "#a50f15",
        "#67000d"
      )
    )(50), domain = countries[[var]])
    
  })
  
  # reactive color palette for IPCC summaries
  ipcc_pal <- reactive({
    req(input$year)                     # Only require input exists
    
    var <- paste0(input$year, "_int16")
    
    colorNumeric(colorRampPalette(
      c(
        "#fee0d2",
        "#fcbba1",
        "#fc9272",
        "#fb6a4a",
        "#ef3b2c",
        "#cb181d",
        "#a50f15",
        "#67000d"
      )
    )(10), domain = ipcc[[var]])
    
  })
  
  # Map output -------------------------------------
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      setView(lng = 0,
              lat = 30,
              zoom = 2)
  })
  
  # this makes it so the proxy map is rendered in the background, otherwise the map is empty when you first navigate to this page
  outputOptions(output, "map", suspendWhenHidden = FALSE)
  
  # change tile url based on selected year and map type
  url <- reactive({
    req(current_page() == "dashboard", input$map_type, input$year)
    
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
  
  ## Update map layer -------------------------------
  observe({
    req(current_page() == "dashboard")  # Only run when on dashboard
    
    map_proxy <- leafletProxy("map")
    
    # Clear all groups initially
    map_proxy %>%
      clearControls() %>% 
      clearGroup(c("Countries", "IPCC"))
    
    # Add base tile layer if a URL is provided
    if (!is.null(url())) {
      map_proxy %>%
        addTiles(url(),
                 group = "hfi",
                 options = tileOptions(maxNativeZoom = 12))
    }
    
    # Add or remove Country layer
    if (input$add_country) {
      withProgress({
        for (i in 1:10) {
          incProgress(1 / 10, message = "Loading shapefile...")
          Sys.sleep(0.5)
        }
        
        map_proxy %>%
          addPolygons(
            data = countries,
            fillColor = ~ country_pal()(get(paste0(
              input$year, "_int16"
            ))),
            fillOpacity = 0.95,
            weight = 0.5,
            color = "#444444",
            group = "Countries",
            popup = ~ paste(
              "<strong>",
              name,
              "</strong>",
              "<br>",
              paste(input$year, "Mean HFI:"),
              round(get(paste0(
                input$year, "_int16"
              )), 2)
            )
          ) %>%
          addLegend(
            position = "bottomright",
            pal = country_pal(),
            values = countries[[paste0(input$year, "_int16")]],
            title = "Average HFI by Country",
            group = "Countries"
          )
      })
    } else {
      map_proxy %>%
        clearGroup("Countries")
    }
    
    # Add or remove IPCC layer
    if (input$add_ipcc) {
      map_proxy %>%
        addPolygons(
          data = ipcc,
          fillColor = ~ ipcc_pal()(get(paste0(
            input$year, "_int16"
          ))),
          fillOpacity = 0.85,
          weight = 0.5,
          color = "#444444",
          group = "IPCC",
          popup = ~ paste(
            "<strong>",
            NAME,
            "</strong>",
            "<br>",
            paste(input$year, "Mean HFI:"),
            round(get(paste0(
              input$year, "_int16"
            )), 2)
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal = ipcc_pal(),
          values = ipcc[[paste0(input$year, "_int16")]],
          title = "Average HFI by IPCC Region",
          group = "IPCC"
        )
    } else {
      map_proxy %>%
        clearGroup("IPCC")
    }
  })
  
  ### zoom to country ------
  observeEvent(input$country, {
    req(current_page() == "dashboard")
    
    if (input$country == "") {
      leafletProxy("map")
    } else {
      zoom <- reactive({
        countries %>%
          filter(name == input$country) %>%
          st_centroid() %>%
          st_coordinates()
      })
      
      leafletProxy('map') %>%
        clearShapes() %>% 
        setView(lng = zoom()[1],
                lat = zoom()[2],
                zoom = 6) %>% 
        addPolygons(data =  countries %>%
                      filter(name == input$country),
                    color = "yellow")
    }
  })
  
  ### zoom to ipcc ------
  observeEvent(input$ipcc, {
    req(current_page() == "dashboard")
    
    if (input$ipcc == "") {
      leafletProxy("map")
    } else {
      zoom <- reactive({
        ipcc %>%
          filter(NAME == input$ipcc) %>%
          st_centroid() %>%
          st_coordinates()
      })
      
      leafletProxy('map') %>%
        setView(lng = zoom()[1],
                lat = zoom()[2],
                zoom = 4)
    }
  })
  
  # Add observer for reset view button
  observeEvent(input$reset_view, {
    req(current_page() == "dashboard")
    
    leafletProxy("map") %>%
      clearShapes() %>% 
      setView(lng = 0,
              lat = 30,
              zoom = 2)
    
    if (input$level == "Country") {
      updateSelectizeInput(session, "country", selected = "")
    } else {
      updateSelectizeInput(session, "ipcc", selected = "")
    }
  })
  
  # [All your existing chart outputs remain the same - country_heatmap, ipcc_heatmap, etc.]
  # I'll include just a few key ones for brevity:
  
  # Chart Outputs -------------------------------------------------
  
  # Calculate country data (used in multiple visualizations)
  country_data <- reactive({
    req(current_page() == "dashboard", input$year)
    
    countries %>%
      st_drop_geometry() %>%
      group_by(name, type) %>%
      summarise(value = mean(get(paste0(
        input$year, "_int16"
      ))))
  })
  
  # Calculate IPCC data (used in multiple visualizations)
  ipcc_data <- reactive({
    req(current_page() == "dashboard", input$year)
    
    ipcc %>%
      st_drop_geometry() %>%
      group_by(NAME, NAME_short) %>%
      summarise(value = mean(get(paste0(
        input$year, "_int16"
      ))))
  })
  
  # Country heatmap visualization
  output$country_heatmap <- renderPlotly({
    req(current_page() == "dashboard")  # Only render when on dashboard
    
    # Create bins in increments of 10
    bin_breaks <- seq(0, 100, by = 10)
    bin_labels <- paste0(bin_breaks[-length(bin_breaks)], "-", bin_breaks[-1] - 1)
    
    data <- country_data() %>%
      mutate(bin_group = cut(
        value,
        breaks = bin_breaks,
        labels = bin_labels,
        include.lowest = TRUE
      ))
    
    # Count countries in each bin
    bin_counts <- data %>%
      group_by(bin_group) %>%
      summarise(count = n()) %>%
      arrange(bin_group)
    
    # Generate a color palette with increasing intensity
    n_bins <- length(bin_labels)
    colors <- colorRampPalette(c("#C6DBEF", "#08519C"))(n_bins)
    
    # Create a cleaner histogram visualization
    p <- plot_ly(
      bin_counts,
      x = ~ bin_group,
      y = ~ count,
      type = "bar",
      marker = list(color = colors, line = list(
        width = 1, color = "#444"
      ))
    ) %>%
      layout(
        title = list(
          text = "Distribution of mean HFI per country",
          font = list(size = 14, color = "#FFFFFF"),
          y = 0.9
        ),
        xaxis = list(
          title = "Mean HFI",
          showgrid = FALSE,
          categoryorder = "array",
          categoryarray = bin_labels,
          color = "#FFFFFF",
          tickfont = list(color = "#FFFFFF")
        ),
        yaxis = list(
          title = "Number of Countries",
          showgrid = TRUE,
          gridcolor = "#444",
          color = "#FFFFFF",
          tickfont = list(color = "#FFFFFF")
        ),
        bargap = 0.1,
        plot_bgcolor = "#222222",
        paper_bgcolor = "#222222",
        font = list(color = "#FFFFFF"),
        legend = list(
          font = list(color = "#FFFFFF"),
          y = 0.8,
          x = 0
        ),
        margin = list(
          l = 5,
          r = 5,
          t = 60,
          b = 5
        )
      )
    
    # Add annotation for selected country if applicable
    if (!is.null(input$country) && input$country != "") {
      selected_value <- data %>%
        filter(name == input$country) %>%
        pull(value)
      
      selected_bin <- data %>%
        filter(name == input$country) %>%
        pull(bin_group)
      
      p <- p %>%
        add_annotations(
          x = selected_bin,
          y = bin_counts %>% filter(bin_group == selected_bin) %>% pull(count),
          text = paste0(input$country, ": ", round(selected_value, 1)),
          showarrow = TRUE,
          arrowhead = 2,
          arrowsize = 1,
          arrowwidth = 2,
          arrowcolor = "#BC0032",
          font = list(color = "#FFFFFF")
        )
    }
    
    return(p)
  })
  
  # Country time ridgeline chart
  output$country_time_ridgeline <- renderPlotly({
    req(current_page() == "dashboard")
    
    plot_ly(
      data = test_freqs %>% filter(year == input$year),
      x = ~value,
      y = ~count,
      type = "bar",
      marker = list(color = "yellow", opacity = 0.75)
    ) %>%
      layout(
        title = list(
          text = paste("HFI Regional Distribution for", input$year),
          font = list(color = "#FFFFFF", size = 12),
          x = 0.5
        ),
        xaxis = list(
          title = list(text = "HFI Value", font = list(color = "#FFFFFF", size = 12)),
          tickfont = list(color = "#FFFFFF"),
          gridcolor = "#444444",
          dtick = 10
        ),
        yaxis = list(
          title = list(text = "Count", font = list(color = "#FFFFFF", size = 12)),
          tickfont = list(color = "#FFFFFF"),
          gridcolor = "#444444",
          tickformat = ".2e"
        ),
        plot_bgcolor = "#222222",
        paper_bgcolor = "#222222",
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })
  
  # Global time series
  output$timeSeries <- renderPlotly({
    req(current_page() == "dashboard")
    
    # Data preparation
    avg_data <- locations %>%
      group_by(year) %>%
      summarize(avg_value = mean(value))
    
    # Create Plotly figure
    plot_ly(
      avg_data,
      x = ~ year,
      y = ~ avg_value,
      type = "scatter",
      mode = "lines+markers",
      line = list(
        color = "#00bc8c",
        width = 2,
        shape = "linear"
      ),
      marker = list(color = "#00bc8c", size = 6),
      name = "Global Mean",
      legendgroup = "avg",
      showlegend = FALSE
    ) %>%
      # Highlight selected year
      add_trace(
        data = avg_data %>% filter(year == input$year),
        x = ~ year,
        y = ~ avg_value,
        type = "scatter",
        mode = "markers",
        marker = list(color = "#BC0032", size = 8),
        name = "Global Mean",
        legendgroup = "avg",
        showlegend = TRUE
      ) %>%
      layout(
        xaxis = list(
          title = "Year",
          color = "#FFFFFF",
          tickfont = list(color = "#FFFFFF")
        ),
        yaxis = list(
          title = "Average HFI",
          color = "#FFFFFF",
          tickfont = list(color = "#FFFFFF")
        ),
        plot_bgcolor = "#222222",
        paper_bgcolor = "#222222",
        font = list(color = "#FFFFFF"),
        margin = list(
          l = 40,
          r = 40,
          t = 60,
          b = 40
        ),
        legend = list(
          orientation = "h",
          x = 0.5,
          y = -0.2,
          xanchor = "center",
          font = list(color = "#FFFFFF")
        )
      )
  })
  
  # Add remaining chart outputs as needed...
  # [Include all your other output functions here]
}

shinyApp(ui, server)