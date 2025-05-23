# Modifications to app.R to add a landing page

library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(leaflet)
library(bslib)
library(dplyr)
library(stringr)
library(ggplot2)
library(plotly)
library(sf)

# Read in vector layers
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

# Create a dark theme
my_theme <- bs_theme(
  version = 5,
  preset = "darkly",
  bg = "#212426",             # Deep navy blue (darker than before)
  fg = "#E6F1FF",             # Soft blue-white (less harsh than pure white)
  primary = "#00bc8c",
  secondary = "#BC0032", # Magenta (more vibrant and modern than the previous red)
  info = "#3A86FF",           # Light cyan (for informational elements)
  warning = "#F6AD55",        # Soft orange (for warnings)
  danger = "#7B0828",         # Soft red (for errors/danger)
  success = "#8A6FDF"        # Soft green (for success messages)
   #bg = "#222222",
  # fg = "#FFFFFF",
  # primary = "#00bc8c",
  # secondary = "#BC0032"
)

# UI --------------------------------------------

# Create a navigation function
landing_page <- function() {
  page_fluid(
    theme = my_theme,
    div(
      class = "container py-4",
      style = "max-width: 1200px;",
      
      # Header with title and description
      div(
        class = "p-4 mb-4 rounded-3 text-center",
        style = "background-color: #212426;",
        h1("Human Footprint Index", style = "color: #00bc8c;"),
        p(class = "lead", "Explore global human impact on the environment through interactive visualization tools and data resources"),
        p("The Human Footprint Index (HFI) quantifies human influence on the Earth's land surface based on ... {placeholder}")
      ),
      
      # Cards section
      div(
        class = "row row-cols-1 row-cols-md-2 g-4",
        
        # Card 1: Interactive Data Explorer
        div(
          class = "col",
          div(
            class = "card h-100",
            style = "background-color: #121314; border-color: #FFF;",
            div(
              class = "card-body d-flex flex-column",
              h3(class = "card-title", "Interactive Data Explorer"),
              p(class = "card-text", "Explore the global Human Footprint Index through an interactive dashboard with regional comparisons, time series analysis, and spatial visualization."),
              div(
                class = "mt-auto pt-3",
                actionButton(
                  "go_to_explorer", 
                  "Launch Explorer", 
                  icon = icon("globe"), 
                  class = "btn btn-info w-100"
                )
              )
            )
          )
        ),
        
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
        
        # Card 3: Google API
        div(
          class = "col",
          div(
            class = "card h-100",
            style = "background-color: #121314; border-color: #FFF;",
            div(
              class = "card-body d-flex flex-column",
              h3(class = "card-title", "API Access"),
              p(class = "card-text", "Integrate HFI data into your applications with our Google Cloud API. Fetch specific regions, time periods, or analysis-ready datasets."),
              div(
                class = "mt-auto pt-3",
                actionButton(
                  "go_to_api", 
                  "API Documentation", 
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
        p("Copyright Placeholder © 2025", style = "color: #999;"),
        p("Citation Placeholder", style = "color: #999;")
      )
    )
  )
}

# Main app UI with navigation
ui <- navbarPage(
  title = "Human Footprint Index",
  id = "nav",
  theme = my_theme,
  header = tags$style(HTML("
    /* Reduce gap between navbar and content */
    .container-fluid {
      padding-top: 0 !important;
      margin-top: 0 !important;
      height: 0 !important
    }
    
    /* Remove default padding from the tab content */
    .tab-content {
      padding-top: 0 !important;
    }
    
    /* Adjust spacing for the page_sidebar */
    .page-sidebar {
      margin-top: 0 !important;
    }
    
    /* Adjust navbar bottom margin */
   /* .navbar {
      margin-bottom: 0 !important;
    } */
  ")),
  tabPanel(
    "Home",
    landing_page()
  ),
  tabPanel(
    "Explorer",
    page_sidebar(
      theme = my_theme,
      title = "Interactive Data Explorer",
      sidebar = sidebar(
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
                "High/Low",
                div(style = "padding-top: 10px;"),
                conditionalPanel(
                  "input.level == 'Country'",
                  plotlyOutput("country_high_low", height = "350px")
                ),
                conditionalPanel(
                  "input.level == 'IPCC Region'",
                  plotlyOutput("ipcc_high_low", height = "300px")
                )
              ),
              tabPanel(
                "Time Series",
                div(style = "padding-top: 10px;"),
                conditionalPanel(
                  "input.level == 'Country' && input.country != ''",
                  plotlyOutput("country_time_series", height = "350px")
                ),
                conditionalPanel(
                  "input.level == 'IPCC Region' && input.ipcc != ''",
                  plotlyOutput("ipcc_time_series", height = "350px")
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
  )
)

# SERVER ------------------------------------------
server <- function(input, output, session) {
  
  # Navigation handler
  observeEvent(input$go_to_explorer, {
    updateNavbarPage(session, "nav", selected = "Explorer")
  })
  
  # Reactive filtered data
  filtered_data <- reactive({
    locations %>%
      filter(year == input$year)
  })
  
  # reactive color palette for country summaries
  country_pal <- reactive({
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
    map_proxy <- leafletProxy("map")
    
    # Clear all groups initially
    map_proxy %>%
      clearControls()
    
    # # Add base tile layer if a URL is provided
    if (!is.null(url())) {
      map_proxy %>%
        addTiles(url(),
                 group = "hfi",
                 options = tileOptions(maxNativeZoom = 12))
    }
    
    
    # Add or remove Country layer
    if (input$add_country) {
      withProgress({
        #Sys.sleep(5)  # Simulate loading time
        
        # Simulate steps in data loading
        for (i in 1:10) {
          incProgress(1 / 10, message = "Loading shapefile...")
          Sys.sleep(0.5)  # Simulate work
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
      # Clear group and control
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
      # Clear group and control
      map_proxy %>%
        clearGroup("IPCC")
    }
    
    
  })
  
  
  
  ### zoom to country ------
  observeEvent(input$country, {
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
        setView(lng = zoom()[1],
                lat = zoom()[2],
                zoom = 6)
    }
    
    
  })
  
  ### zoom to ipcc ------
  observeEvent(input$ipcc, {
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
    # Reset the map view to global view
    leafletProxy("map") %>%
      setView(lng = 0,
              lat = 30,
              zoom = 2)
    
    # Also optionally clear the country/region selection
    if (input$level == "Country") {
      updateSelectizeInput(session, "country", selected = "")
    } else {
      updateSelectizeInput(session, "ipcc", selected = "")
    }
  })
  
  # Chart Outputs -------------------------------------------------
  
  # Country heatmap visualization
  output$country_heatmap <- renderPlotly({
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
        ) # add space to top
      )
    
    # Add annotation for selected country if applicable
    if (input$country != "") {
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
  
  # IPCC heatmap visualization
  output$ipcc_heatmap <- renderPlotly({
    # Create bins in increments of 5
    bin_breaks <- seq(0, max(ipcc_data()$value) + 5, by = 5)
    bin_labels <- paste0(bin_breaks[-length(bin_breaks)], "-", bin_breaks[-1] - 1)
    
    data <- ipcc_data() %>%
      mutate(bin_group = cut(
        value,
        breaks = bin_breaks,
        labels = bin_labels,
        include.lowest = TRUE
      ))
    
    # Count regions in each bin
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
          text = "Distribution of Mean HFI per IPCC Region",
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
          title = "Number of Regions",
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
        ) # add space to top
      )
    
    # Add annotation for selected region if applicable
    if (input$ipcc != "") {
      selected_value <- data %>%
        filter(NAME == input$ipcc) %>%
        pull(value)
      
      selected_bin <- data %>%
        filter(NAME == input$ipcc) %>%
        pull(bin_group)
      
      # Get the short name for display
      selected_name <- data %>%
        filter(NAME == input$ipcc) %>%
        pull(NAME_short)
      
      p <- p %>%
        add_annotations(
          x = selected_bin,
          y = bin_counts %>% filter(bin_group == selected_bin) %>% pull(count),
          text = paste0(selected_name, ": ", round(selected_value, 1)),
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
  
  # Calculate country data (used in multiple visualizations)
  country_data <- reactive({
    countries %>%
      st_drop_geometry() %>%
      group_by(name, type) %>%
      summarise(value = mean(get(paste0(
        input$year, "_int16"
      ))))
  })
  
  # Calculate IPCC data (used in multiple visualizations)
  ipcc_data <- reactive({
    ipcc %>%
      st_drop_geometry() %>%
      group_by(NAME, NAME_short) %>%
      summarise(value = mean(get(paste0(
        input$year, "_int16"
      ))))
  })
  
  ## High/Low -------------------
  # Country High/Low visualization
  output$country_high_low <- renderPlotly({
    data <- country_data() %>%
      #filter out dependencies etc
      filter(type %in% c("Country", "Sovereign country", "Sovereignty")) %>%
      arrange(desc(value))
    
    # Get top 10 and bottom 10
    top10 <- head(data, 10)
    bottom10 <- tail(data, 10)
    
    # Start with top10 and bottom10
    display_data <- bind_rows(top10, bottom10)
    
    # Determine if selected country is already in High/Low 10
    selected_in_display <- FALSE
    if (input$country != "") {
      selected_in_display <- input$country %in% c(top10$name, bottom10$name)
    }
    
    # Add selected country if it exists and isn't already in High/Low 10
    if (input$country != "" && !selected_in_display) {
      selected <- data %>% filter(name == input$country)
      display_data <- bind_rows(top10, selected, bottom10)
    }
    
    # Add a group column for coloring
    display_data <- display_data %>%
      mutate(
        group = case_when(
          name %in% top10$name & name == input$country ~ "Selected Top 10",
          name %in% bottom10$name &
            name == input$country ~ "Selected Bottom 10",
          name %in% top10$name ~ "Top 10",
          name %in% bottom10$name ~ "Bottom 10",
          TRUE ~ "Selected"
        )
      )
    
    # Create a horizontal bar chart
    p <- plot_ly(
      display_data,
      y = ~ reorder(name, value),
      x = ~ value,
      type = "bar",
      orientation = "h",
      color = ~ group,
      colors = c(
        "Top 10" = "#BC0032",
        "Bottom 10" = "#00bc8c",
        "Selected" = "#f39c12",
        "Selected Top 10" = "#e74c3c",
        "Selected Bottom 10" = "#2ecc71"
      )
    ) %>%
      layout(
        title = list(
          text = "Countries with Highest and Lowest HFI",
          font = list(size = 14, color = "#FFFFFF"),
          y = 0.9
        ),
        xaxis = list(
          title = "Human Footprint Index",
          showgrid = TRUE,
          gridcolor = "#444",
          color = "#FFFFFF",
          tickfont = list(color = "#FFFFFF")
        ),
        yaxis = list(
          title = "",
          showgrid = FALSE,
          color = "#FFFFFF",
          tickfont = list(color = "#FFFFFF", size = 10),
          showticklabels = TRUE,
          dtick = 1 # Force a tick for each position
        ),
        margin = list(
          l = 1,
          r = 1,
          t = 70,
          b = 5
        ),
        # Reduces margins, slight top margin for spacing
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = 0.98,
          yanchor = "bottom",
          itemstacking = "false",
          itemwidth = 50,
          bgcolor = "rgba(0, 0, 0, 0)"
        ),
        plot_bgcolor = "#222222",
        paper_bgcolor = "#222222",
        font = list(color = "#FFFFFF")
      )
    
    return(p)
  })
  
  # IPCC High/Low visualization
  output$ipcc_high_low <- renderPlotly({
    data <- ipcc_data() %>%
      arrange(desc(value))
    
    # Get top 5 and bottom 5 (or all if less than 20 total)
    max_regions <- min(5, floor(nrow(data) / 2))
    top5 <- head(data, max_regions)
    bottom5 <- tail(data, max_regions)
    
    # Start with top10 and bottom10
    display_data <- bind_rows(top5, bottom5)
    
    # Determine if selected region is already in High/Low
    selected_in_display <- FALSE
    if (input$ipcc != "") {
      selected_in_display <- input$ipcc %in% c(top5$NAME, bottom5$NAME)
    }
    
    # Add selected region if it exists and isn't already in High/Low
    if (input$ipcc != "" && !selected_in_display) {
      selected <- data %>% filter(NAME == input$ipcc)
      display_data <- bind_rows(top5, selected, bottom5)
    }
    
    # Add a group column for coloring
    display_data <- display_data %>%
      mutate(
        group = case_when(
          NAME %in% top5$NAME & NAME == input$ipcc ~ "Selected Top",
          NAME %in% bottom5$NAME &
            NAME == input$ipcc ~ "Selected Bottom",
          NAME %in% top5$NAME ~ "Top Regions",
          NAME %in% bottom5$NAME ~ "Bottom Regions",
          TRUE ~ "Selected"
        )
      )
    
    # Create a horizontal bar chart
    p <- plot_ly(
      display_data,
      y = ~ reorder(NAME_short, value),
      x = ~ value,
      type = "bar",
      orientation = "h",
      color = ~ group,
      colors = c(
        "Top Regions" = "#BC0032",
        "Bottom Regions" = "#00bc8c",
        "Selected" = "#f39c12",
        "Selected Top" = "#e74c3c",
        "Selected Bottom" = "#2ecc71"
      )
    ) %>%
      layout(
        title = list(
          text = "IPCC Regions with Highest and Lowest HFI",
          font = list(size = 14, color = "#FFFFFF"),
          y = 0.9
        ),
        xaxis = list(
          title = "Human Footprint Index",
          showgrid = TRUE,
          gridcolor = "#444",
          color = "#FFFFFF",
          tickfont = list(color = "#FFFFFF")
        ),
        yaxis = list(
          title = "",
          showgrid = FALSE,
          color = "#FFFFFF",
          tickfont = list(color = "#FFFFFF", size = 12),
          showticklabels = TRUE,
          dtick = 1 # Force a tick for each position
        ),
        bargap = 0.2,
        # Remove gaps between bars
        bargroupgap = 0,
        # Remove gaps between bar groups
        barmode = "stack",
        # Ensures bars are directly adjacent
        margin = list(
          l = 1,
          r = 1,
          t = 60,
          b = 1
        ),
        # Reduces margins, slight top margin for spacing
        legend = list(
          orientation = "h",
          x = 1,
          xanchor = "right",
          y = 0,
          yanchor = "bottom",
          itemwidth = 40,
          itemsizing = "constant",
          bgcolor = "rgba(0, 0, 0, 0)",
          traceorder = "normal"  # Add this line
        ),
        plot_bgcolor = "#222222",
        paper_bgcolor = "#222222",
        font = list(color = "#FFFFFF")
      )
    
    return(p)
  })
  
  ##  Country time series ---------------------------
  output$country_time_series <- renderPlotly({
    req(input$country)
    
    # For this example, we'll create sample time series data
    years_data <- years
    values <- runif(length(years_data), 30, 70) # Simple random trend
    
    # Add some trend for visual appeal
    for (i in 2:length(values)) {
      values[i] <- values[i - 1] + rnorm(1, 0, 3)
      if (values[i] < 10)
        values[i] <- 10
      if (values[i] > 90)
        values[i] <- 90
    }
    
    ts_data <- data.frame(year = years_data, value = values)
    
    # Global average for comparison
    global_data <- data.frame(year = years_data, value = runif(length(years_data), 40, 60))
    
    # Create time series plot
    p <- plot_ly() %>%
      add_trace(
        data = ts_data,
        x = ~ year,
        y = ~ value,
        type = "scatter",
        mode = "lines+markers",
        name = input$country,
        line = list(color = "#00bc8c", width = 3)
      ) %>%
      add_trace(
        data = global_data,
        x = ~ year,
        y = ~ value,
        type = "scatter",
        mode = "lines",
        name = "Global Average",
        line = list(
          color = "#3498db",
          width = 2,
          dash = "dash"
        )
      ) %>%
      layout(
        title = list(
          text = paste0("HFI Trend for ", input$country),
          font = list(size = 14, color = "#FFFFFF"),
          y = 0.95
        ),
        xaxis = list(
          title = "Year",
          showgrid = TRUE,
          gridcolor = "#444",
          color = "#FFFFFF",
          tickfont = list(color = "#FFFFFF")
        ),
        yaxis = list(
          title = "Human Footprint Index",
          showgrid = TRUE,
          gridcolor = "#444",
          color = "#FFFFFF",
          tickfont = list(color = "#FFFFFF")
        ),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = 0.85,
          yanchor = "bottom",
          font = list(color = "#FFFFFF")
        ),
        plot_bgcolor = "#222222",
        paper_bgcolor = "#222222",
        font = list(color = "#FFFFFF")
      )
    
    return(p)
  })
  
  # IPCC time series (similar structure to country)
  output$ipcc_time_series <- renderPlotly({
    req(input$ipcc)
    
    # For this example, we'll create sample time series data
    years_data <- years
    values <- runif(length(years_data), 30, 70)
    
    # Add some trend for visual appeal
    for (i in 2:length(values)) {
      values[i] <- values[i - 1] + rnorm(1, 0, 2)
      if (values[i] < 10)
        values[i] <- 10
      if (values[i] > 90)
        values[i] <- 90
    }
    
    ts_data <- data.frame(year = years_data, value = values)
    
    # Global average for comparison
    global_data <- data.frame(year = years_data, value = runif(length(years_data), 40, 60))
    
    # Create time series plot
    selected_region <- ipcc %>%
      filter(NAME == input$ipcc) %>%
      pull(NAME_short)
    
    p <- plot_ly() %>%
      add_trace(
        data = ts_data,
        x = ~ year,
        y = ~ value,
        type = "scatter",
        mode = "lines+markers",
        name = selected_region,
        line = list(color = "#00bc8c", width = 3)
      ) %>%
      add_trace(
        data = global_data,
        x = ~ year,
        y = ~ value,
        type = "scatter",
        mode = "lines",
        name = "Global Average",
        line = list(
          color = "#3498db",
          width = 2,
          dash = "dash"
        )
      ) %>%
      layout(
        title = list(
          text = paste0("HFI Trend for ", selected_region),
          font = list(size = 14, color = "#FFFFFF"),
          y = 0.95
        ),
        xaxis = list(
          title = "Year",
          showgrid = TRUE,
          gridcolor = "#444",
          color = "#FFFFFF",
          tickfont = list(color = "#FFFFFF")
        ),
        yaxis = list(
          title = "Human Footprint Index",
          showgrid = TRUE,
          gridcolor = "#444",
          color = "#FFFFFF",
          tickfont = list(color = "#FFFFFF")
        ),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = 0.85,
          yanchor = "bottom",
          font = list(color = "#FFFFFF")
        ),
        plot_bgcolor = "#222222",
        paper_bgcolor = "#222222",
        font = list(color = "#FFFFFF")
      )
    
    return(p)
  })
  
  # Global time series (kept from original)
  output$timeSeries <- renderPlotly({
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
        #title = list(text = "Time Series of Average Values", x = 0.5),
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
  
}

shinyApp(ui, server)