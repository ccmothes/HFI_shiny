library(shiny)
library(shinyWidgets)
library(shinycssloaders)
library(leaflet)
library(leaflegend)
library(bslib)
library(dplyr)
library(tidyr)
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


global_means <- read_csv("app_data/global_means.csv")

# Time series data
country_props <- read_csv("app_data/country_mlhfi_props_long.csv") %>% 
  mutate(class = case_when(category == "low" ~ "Natural (<10)",
                           category == "medium" ~ "Used (>10 & <35)",
                           category == "high" ~ "Settlements (>35)",
                           .default = category)) %>% 
  mutate(class = factor(class, levels = c("Natural (<10)", "Used (>10 & <35)", "Settlements (>35)")))

country_freqs <- read_csv("app_data/country_mlhfi_distribution.csv")


ipcc_props <- read_csv("app_data/ipcc_mlhfi_props_long.csv") %>%
  # clean up names
  mutate(
    NAME = str_to_title(NAME),
    NAME = if_else(
      NAME == "Southern Topical Pacific",
      "Southern Tropical Pacific",
      NAME
    )
  ) %>% mutate(
    class = case_when(
      category == "low" ~ "Natural (<10)",
      category == "medium" ~ "Used (>10 & <35)",
      category == "high" ~ "Settlements (>35)",
      .default = category
    )
  ) %>%
  mutate(class = factor(
    class,
    levels = c("Natural (<10)", "Used (>10 & <35)", "Settlements (>35)")
  ))

ipcc_freqs <- read_csv("app_data/ipcc_mlhfi_distribution.csv") %>% 
  #Clean names to match shapefile
  mutate(
    NAME = str_to_title(NAME),
    NAME = if_else(
      NAME == "Southern Topical Pacific",
      "Southern Tropical Pacific",
      NAME
    )
  )


# vector of years
years <- 1999:2024


# Create a dark theme
my_theme <- bs_theme(
  version = 5,
  preset = "darkly",
  bg = "#222222",
  fg = "#FFFFFF",
  primary = "#00bc8c",
  secondary = "#fcffa4", #"#BC0032",
  info = "#4a0c6b",  # "#3A86FF",           
  warning =  "#a52c60", # "#F6AD55",       
  danger = "#7B0828",        
  success = "#ed6925" # "#8A6FDF"   
)

# UI --------------------------------------------

ui <- page_navbar(
    id = "navbar_id",
    title = "Machine Learning Human Footprint Index",
    theme = my_theme,
    includeCSS("www/style.css"),
    navbar_options = navbar_options(bg = "#00bc8c"),
    
    nav_panel(
      title = "Home",
      div(
        class = "container py-4",
        style = "max-width: 1200px;",
        
        # Header with title and description
        div(
          class = "p-4 mb-4 rounded-3 text-center",
          style = "background-color: #212426;",
          h1("Machine Learning Human Footprint Index (ml-HFI)", style = "color: #00bc8c;"),
          p(
            class = "lead",
            "Explore global human impact on the environment through interactive visualization tools and data resources"
          ),
          p(
            "The ml-HFI quantifies human influence on the Earth's land surface  using a convolutional neural network (CNN)
                  trained on an existing Human Footprint Index (HFI) dataset, with Landsat imagery as input features.
                  The ml-HFI ranges from 0 to 100, where 0 represents intact natural areas and higher values indicate
                  increasing human pressure. Global annual data is available at 300m resolution from 1999-2024."
          )
        ),
        # Publication Announcement
        div(
          class = "d-flex justify-content-center mb-4",
          div(
            class = "card mb-4",
            style = "background-color: #121314; border: 2px solid rgba(0, 188, 140, 0.3); box-shadow: 0 4px 6px rgba(0, 188, 140, 0.1); max-width: 800px;",
            div(
              class = "card-body text-center py-3",
              h5(class = "card-title mb-2", style = "color: #00bc8c;", "📄 New Publication"),
              p(
                class = "card-text mb-3",
                style = "font-size: 0.95rem;",
                tags$span(
                  "This work was recently published in",
                  tags$em("Machine Learning: Earth"),
                  "titled 'Uncovering patterns of converging human-induced pressure on global lands'. Click
                  the link below to read more on the methods and findings related to this dataset."
                )
              ),
              actionButton(
                "go_to_pub",
                "View Publication",
                icon = icon("book-open"),
                class = "btn btn-primary w-50",
                onclick = "window.open('https://iopscience.iop.org/article/10.1088/3049-4753/ae2278', '_blank')"
              )
            )
          )
        ),
        
        
        # Cards section
        # First row - Interactive Data Explorer (full width)
        div(class = "row mb-4", div(
          class = "col-12",
          div(
            class = "card",
            style = "background-color: #121314; border-color: #FFF;",
            div(
              class = "card-body text-center",
              h3(class = "card-title", "Interactive Data Explorer"),
              p(
                class = "card-text",
                "Explore the global ml-HFI through an interactive dashboard with regional comparisons, time series analysis, and spatial visualization."
              ),
              # Add image here as icon
              tags$img(
                src = "map_view.png",
                alt = "Interactive Map Preview",
                style = "width: 500px; height: auto; margin: 15px 0; border-radius: 8px; box-shadow: 0 4px 8px rgba(0, 0, 0, 0.3);"
              ),
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
        )),
        
        # Second row - Two other cards
        div(
          class = "row row-cols-1 row-cols-md-2 g-4",
          
          # Card 2: Google Earth Engine
          div(
            class = "col",
            div(
              class = "card h-100",
              style = "background-color: #121314; border-color: #FFF;",
              div(
                class = "card-body d-flex flex-column",
                h3(class = "card-title", "Google Earth Engine"),
                p(
                  class = "card-text",
                  "Access the ml-HFI dataset through Google Earth Engine for advanced geospatial analysis and integration with other environmental datasets."
                ),
                div(
                  class = "mt-auto pt-3",
                  actionButton(
                    "go_to_gee",
                    "Access Earth Engine",
                    icon = icon("map"),
                    class = "btn btn-warning w-100"
                    #onclick = "window.open('https://code.earthengine.google.com/f46e81f6ada4c8608b963e6d255efd87', '_blank')"
                  )
                )
              )
            )
          ),
          
          
          # Card 3: Raw Data Download
          div(
            class = "col",
            div(
              class = "card h-100",
              style = "background-color: #121314; border-color: #FFF;",
              div(
                class = "card-body d-flex flex-column",
                h3(class = "card-title", "Raw Data Download"),
                p(
                  class = "card-text",
                  "Download the complete ml-HFI dataset for use in your own GIS software or analysis."
                ),
                div(
                  class = "mt-auto pt-3",
                  actionButton(
                    "go_to_download",
                    "Download Data",
                    icon = icon("download"),
                    class = "btn btn-success w-100",
                    onclick = "window.open('https://doi.org/10.5061/dryad.m63xsj4fk', '_blank')"
                  )
                )
              )
            )
          )
        ),
        hr(),
        div(
          class = "pt-4 mt-4",
          div(
            class = "row",
            # Left column - Logo and developer info
            div(
              class = "col-md-6 text-center text-md-start mb-3",
              p(
                style = "color: #CCCCCC; margin-bottom: 10px; font-size: 0.85rem;",
                paste("Application developed by the Geospatial Centroid at CSU. Last Updated", format(Sys.Date(), "%B %Y"))
              ),
              tags$img(
                src = "Centroid_logo.png",
                alt = "Geospatial Centroid Logo",
                style = "max-width: 200px; height: auto;"
              )
            ),
            # Right column - Citation and contact
            div(
              class = "col-md-6 text-center text-md-start",
              p(
                style = "color: #CCCCCC; margin-bottom: 10px; font-size: 0.85rem;",
                tags$strong("Citation: "),
                "Orihuela-Pinto, B., Keys, P. W., Davenport, F. V., & Barnes, E. A. (2025). Uncovering patterns of converging human-induced pressure on global lands. ",
                tags$em("Machine Learning: Earth, 1"),
                "(1), 01LT03. https://doi.org/10.1088/3049-4753/ae2278"
              ),
              p(
                style = "color: #CCCCCC; font-size: 0.85rem;",
                tags$strong("Contact: "),
                "Pat Keys, ",
                tags$a(
                  href = "mailto:pkeys@bu.edu",
                  "pkeys@bu.edu",
                  style = "color: #00bc8c;"
                )
              )
            )
          )
        )
      )
    ),
    ### Data Explorer ----------------------------------
    nav_panel(title = "Data Explorer", page_sidebar(
     # fillable = FALSE,
      sidebar = sidebar(
        radioGroupButtons(
          "map_type",
          choices = c("Annual Map", "Change Map"),
          selected = "Annual Map"
        ),
        conditionalPanel(
          "input.map_type == 'Annual Map'",
          
          sliderTextInput(
            inputId = "year",
            label = "Select Year:",
            choices = 1999:2024,
            selected = 1999,
            grid = TRUE
          )
        ),
        conditionalPanel(
          "input.map_type == 'Change Map'",
          em("Change map was calculated as:"),
          em("(mean of 2022, 2023, 2024) - (mean of 1999, 2000, 2001)."),
          em("Only changes greater than 10 and less than -10 are shown.")
        ),
        checkboxGroupInput(
          "map_layers",
          "Add Map Layers:",
          choices = list("Country Boundaries" = "countries", "IPCC Boundaries" = "ipcc"),
          selected = NULL
        ),
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
              ),
              # plotlyOutput("country_histogram", height = "600px")
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
              ),
              # plotlyOutput("ipcc_histogram", height = "600px")
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
                  hr(),
                  plotlyOutput("ipcc_time_props", height = "350px"),
                  hr(),
                  plotlyOutput("ipcc_time_ridgeline", height = "350px")
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
        ### Global mean
        card(
          full_screen = TRUE,
          card_header("Mean Annual Change"),
          plotlyOutput("timeSeries", height = "300px")
        ),
        width = "500px"
      ),
      
      ## Map content
      leafletOutput("map", height = "100%") #"100%"
    )),
   
)

# SERVER ------------------------------------------
server <- function(input, output, session) {
  
  # page navigation
  observeEvent(input$go_to_explorer, {
    updateNavbarPage(session, "navbar_id", selected = "Data Explorer")
  })  
  
  # Reactive filtered data ---------------------------------

  
  # reactive color palette for country summaries
  country_pal <- reactive({
    var <- paste0("mlHFI_", input$year)
    
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
    var <- paste0("mlHFI_", input$year)
    
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
  
  
  ## Reactive map legend
  
  map_legend <- reactive({
    
    if(input$map_type == "Annual Map") {
      
      colors <- c("#000004", "#1b0c42", "#4a0c6b", "#781c6d", "#a52c60", 
                  "#cf4446", "#ed6925", "#fb9b06", "#f7d03c", "#fcffa4")
      values <- seq(0, 94, length.out = 10)
      
      title <- "ml-HFI"
      
      lab_format <- function(x) {
        prettyNum(x, format = "f", big.mark = ",", digits =
                    3, scientific = FALSE)
      }
      
      pal <- colorNumeric(palette = rev(colors), domain = c(0, 94))
      
      return(list(values = values, title = title, num_format = lab_format, pal = pal))
      
    } else {
      
      if(input$map_type == "Change Map") {
        
        colors <- c("#4dac26", "#b8e186", "transparent", "#f1b6da", "#d01c8b")
        
        values <- c(-41,41)
        
        title <- "Change in ml-HFI"
        
        # Edit labels
        customLabFormat <- function(x) {
          case_when(
            x <= -34.9 ~ "<= -35",
            x >= 34.9 ~ ">= 35",
            abs(x) < 0.1 ~ "0",
            TRUE ~ as.character(round(x))
          )
        }
        
        
        # Create color pal
        pal <- colorNumeric(
          palette = colors, 
          domain = c(-41, 41),
          na.color = "transparent",
          reverse = TRUE
        )
        
        return(list(values = values, title = title, num_format = customLabFormat, pal = pal))
        
      } 
      
    }
  })

 
  
  
  # Map output -------------------------------------
  output$map <- renderLeaflet({
    leaflet() %>%
      #addProviderTiles("OpenStreetMap") %>%
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
        "https://tiles.arcgis.com/tiles/KNdRU5cN6ENqCTjk/arcgis/rest/services/Global_",
        input$year,
        "_mlhfi_mosaic_tiled/MapServer/tile/{z}/{y}/{x}"
      )

    } else if (input$map_type == "Change Map") {
     # if (input$year == 1999) {
        # paste0(
        #   "https://tiles.arcgis.com/tiles/swlKRWoduvVuwMcX/arcgis/rest/services/TP_",
        #   input$year,
        #   "_3857_12levels/MapServer/tile/{z}/{y}/{x}"
        # )
      "https://tiles.arcgis.com/tiles/KNdRU5cN6ENqCTjk/arcgis/rest/services/extreme_diff_2022to2024_vs_1999to2000_tif/MapServer/tile/{z}/{y}/{x}"
      
      # } else {
      #   paste0(
      #     "https://tiles.arcgis.com/tiles/swlKRWoduvVuwMcX/arcgis/rest/services/TP_Change_",
      #     input$year,
      #     "_1999_1k/MapServer/tile/{z}/{y}/{x}"
      #   )
        
     # }
    } else {
      return(NULL)
    }
    
  })
  
  ## Update map layer -------------------------------
  observe({
    map_proxy <- leafletProxy("map")
    
    # Clear all groups initially
    map_proxy %>%
      clearControls() %>% 
      clearGroup(c("Countries", "IPCC"))
    
    # # Add base tile layer if a URL is provided
    if (!is.null(url())) {
      map_proxy %>%
        clearGroup("hfi") %>% 
        clearControls() %>% 
        addTiles(url(),
                 group = "hfi",
                 options = tileOptions(maxNativeZoom = 12)) %>% 
        addLegendNumeric(
          position = "bottomright",
          pal = map_legend()[["pal"]],
          values = map_legend()[["values"]],
          title = map_legend()[["title"]],
          numberFormat = map_legend()[["num_format"]],
          group = "hfi",
          height = 150,
          decreasing = TRUE
        )
    }
    
    # Add/remove boundary layers
    if ("countries" %in% input$map_layers) {
      
      map_proxy %>%
        addPolygons(
          data = countries,
          fillOpacity = 0,
          weight = 1.5,
          color = "white",
          group = "country_borders",
          popup = ~ paste(
            "<strong>",
            name,
            "</strong>",
            "<br>",
            paste(input$year, "Mean HFI:"),
            round(get(paste0("mlHFI_",
                             input$year
            )), 2)
          )
        )
    } else {
      # Clear group and control
      map_proxy %>%
        clearGroup("country_borders")
    }
    
    if ("ipcc" %in% input$map_layers) {
      
      map_proxy %>%
        addPolygons(
          data = ipcc,
          fillOpacity = 0,
          weight = 1.5,
          color = "white",
          group = "ipcc_borders",
          popup = ~ paste(
            "<strong>",
            NAME,
            "</strong>",
            "<br>",
            paste(input$year, "Mean HFI:"),
            round(get(paste0("mlHFI_",
                             input$year
            )), 2)
          )
        )
    } else {
      # Clear group and control
      map_proxy %>%
        clearGroup("ipcc_borders")
    }
    
    
    # Add or remove Country layer
    if (input$add_country) {
      #withProgress({
        # #Sys.sleep(5)  # Simulate loading time
        # 
        # # Simulate steps in data loading
        # for (i in 1:5) {
        #   incProgress(1 / 5, message = "Loading shapefile...")
        #   Sys.sleep(0.5)  # Simulate work
        # }
        
        map_proxy %>%
          addPolygons(
            data = countries,
            fillColor = ~ country_pal()(get(paste0("mlHFI_", input$year))),
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
              round(get(paste0("mlHFI_",
                input$year
              )), 2)
            )
          ) %>%
          addLegend(
            position = "bottomright",
            pal = country_pal(),
            values = countries[[paste0("mlHFI_", input$year)]],
            title = "Average HFI by Country",
            group = "Countries"
          )
      # })
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
          fillColor = ~ ipcc_pal()(get(paste0("mlHFI_",
            input$year
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
            round(get(paste0("mlHFI_",
              input$year
            )), 2)
          )
        ) %>%
        addLegend(
          position = "bottomright",
          pal = ipcc_pal(),
          values = ipcc[[paste0("mlHFI_", input$year)]],
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
        clearGroup("selected") %>% 
        setView(lng = zoom()[1],
                lat = zoom()[2],
                zoom = 6) %>% 
        addPolygons(
          data =  countries %>%
            filter(name == input$country),
          color = "#00bc8c",
          opacity = 1,
          fillOpacity = 0,
          group = "selected"
        )
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
        clearGroup("selected") %>% 
        setView(lng = zoom()[1],
                lat = zoom()[2],
                zoom = 4) %>% 
        addPolygons(
          data =  ipcc %>%
            filter(NAME == input$ipcc),
          color = "#00bc8c",
          opacity = 1,
          fillOpacity = 0,
          group = "selected"
        )
    }
    
    
  })
  
  # Add observer for reset view button
  observeEvent(input$reset_view, {
    # Reset the map view to global view
    leafletProxy("map") %>%
      clearShapes() %>% 
      setView(lng = 0,
              lat = 30,
              zoom = 2)
    
    # Clear the country/region selection
    if (input$level == "Country") {
      updateSelectizeInput(session, "country", selected = "")
    } else {
      updateSelectizeInput(session, "ipcc", selected = "")
    }
    
    # Clear all layer check boxes
    updateCheckboxGroupInput(session, "map_layers", selected = character(0))
    updateMaterialSwitch(session, "add_country", value = FALSE)
    updateMaterialSwitch(session, "add_ipcc", value = FALSE)
    
  })
  
  
  # Chart Outputs -------------------------------------------------
  
  ## Data ------------------------------------------------
  
  # Calculate country data (used in multiple visualizations)
  country_data <- reactive({
    countries %>%
      st_drop_geometry() %>%
      group_by(name, type) %>%
      summarise(value = mean(get(paste0("mlHFI_", input$year))))
     # group_by(name, type) %>%
     # summarise(value = mean(get(paste0(
      #  input$year, "_int16"
     # ))))
  })
  
  # Calculate IPCC data (used in multiple visualizations)
  ipcc_data <- reactive({
    ipcc %>%
      st_drop_geometry() %>%
      group_by(NAME, NAME_short) %>%
      summarise(value = mean(get(paste0("mlHFI_",
        input$year
      ))))
  })
  
  
  ## Distribution --------------------------
  
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
    colors <- viridis::inferno(n_bins)
    
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
          text = paste(input$year, "Distribution of Mean HFI per Country"),
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
          arrowcolor = "#00bc8c", #"#BC0032",
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
      mutate(
        bin_group = cut(
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
    colors <- viridis::inferno(n_bins)
    #colors <- colorRampPalette(c("#C6DBEF", "#08519C"))(n_bins)
    
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
          text = paste(input$year, "Distribution of Mean HFI per IPCC Region"),
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
          arrowcolor = "#00bc8c",
          font = list(color = "#FFFFFF")
        )
    }
    
    return(p)
  })
  
  ## Similarity -------------------
  # Country HFI similarity visualization
  output$country_similar <- renderPlotly({
    data <- country_data() %>%
      #filter out dependencies etc
      filter(type %in% c("Country", "Sovereign country", "Sovereignty")) %>%
      arrange(desc(value)) %>% 
      ungroup() %>% 
      mutate(rank = row_number())
    
      # filter data to zoom into selected country
      if (input$country != "") {
        selected_rank <- data %>% filter(name == input$country) %>% pull(rank)
        display_data <- data %>% filter(rank %in% seq(selected_rank -
                                                        5, selected_rank + 5))
      } else {
        display_data <- data
      }
      
      
      # Define color scheme
    colors <- viridis::inferno(251, direction = -1)
        # Highlight selected country
        if (input$country != "") {
          colors[display_data$name == input$country] <- "#00bc8c"
        }
      
      # Create plotly bar chart
      p <- plot_ly(
        data = display_data,
        x = ~name,
        y = ~value,
        type = "bar",
        marker = list(color = colors),
        hoverinfo = "text",
        hovertext = ~paste(
          "Country:", name, 
          "<br>HFI:", round(value, 2)
        )
      ) 
      
      # Layout configuration
      p <- p %>% layout(
        title = list(
          text = "Mean Human Footprint Index by Country",
          font = list(size = 14, color = "#FFFFFF"),
          y = 0.95
        ),
        xaxis = list(
          title = "",
          tickangle = 45,
          categoryorder = "array",
          categoryarray = display_data$name,
          showgrid = TRUE,
          gridcolor = "#444",
          color = "#FFFFFF",
          tickfont = list(color = "#FFFFFF")
        ),
        margin = list(
          b = 100,
          l = 60,
          r = 40,
          t = 80
        ),
        hoverlabel = list(
          bgcolor = "white",
          font = list(family = "Arial", size = 12)
        ),
        yaxis = list(
          title = "Human Footprint Index",
          showgrid = TRUE,
          gridcolor = "#444",
          color = "#FFFFFF",
          tickfont = list(color = "#FFFFFF")
        ),
        plot_bgcolor = "#222222",
        paper_bgcolor = "#222222",
        font = list(color = "#FFFFFF")
      )
      
      
      return(p)
    })
    
  
  # IPCC HFI similarity visualization
  output$ipcc_similar <- renderPlotly({
    data <- ipcc_data() %>%
      mutate(NAME = str_remove(NAME, "\\s*\\[.*\\]$")) %>% 
      arrange(desc(value)) %>% 
      ungroup() %>% 
      mutate(rank = row_number())
    
    # filter data to zoom into selected country
    # if (input$ipcc != "") {
    #   selected_rank <- data %>% filter(name == input$ipcc) %>% pull(rank)
    #   display_data <- data %>% filter(rank %in% seq(selected_rank -
    #                                                   5, selected_rank + 5))
    # } else {
    #   display_data <- data
    # }
    
    
    # Define color scheme
    colors <- viridis::inferno(33, direction = -1)
    # Highlight selected country
    # if (input$country != "") {
    #   colors[display_data$name == input$country] <- "#00BCD4"
    # }
    
    # Create plotly bar chart
    p <- plot_ly(
      data = data,
      #data = display_data,
      x = ~NAME,
      y = ~value,
      type = "bar",
      marker = list(color = colors),
      hoverinfo = "text",
      hovertext = ~paste(
        "IPCC Region:", NAME, 
        "<br>HFI:", round(value, 2)
      )
    ) 
    
    # Layout configuration
    p <- p %>% layout(
      title = list(
        text = "Mean Human Footprint Index by IPCC Region",
        font = list(size = 14, color = "#FFFFFF"),
        y = 0.95
      ),
      xaxis = list(
        title = "",
        tickangle = 45,
        categoryorder = "array",
        categoryarray = data$NAME,
        #categoryarray = display_data$name,
        showgrid = TRUE,
        gridcolor = "#444",
        color = "#FFFFFF",
        tickfont = list(color = "#FFFFFF")
      ),
      margin = list(
        b = 100,
        l = 60,
        r = 40,
        t = 80
      ),
      hoverlabel = list(
        bgcolor = "white",
        font = list(family = "Arial", size = 12)
      ),
      yaxis = list(
        title = "Human Footprint Index",
        showgrid = TRUE,
        gridcolor = "#444",
        color = "#FFFFFF",
        tickfont = list(color = "#FFFFFF")
      ),
      plot_bgcolor = "#222222",
      paper_bgcolor = "#222222",
      font = list(color = "#FFFFFF")
    )
    
    
    return(p)
  })
  
  
  ## Time Series --------------------
  
  ###  Country time series ---------------------------
  output$country_time_series <- renderPlotly({
    req(input$country)
  
    
    ts_data <- countries %>% 
      st_drop_geometry() %>% 
      select(name, mlHFI_1999:mlHFI_2024) %>% 
      pivot_longer(cols = -name,
                   names_to = "year",
                   values_to = "value") %>% 
      separate(year, sep = "_", into = c("var", "year")) %>% 
      select(-var) %>% 
      filter(name == input$country)
    
    # Global average for comparison
    #global_data <- data.frame(year = years_data, value = runif(length(years_data), 40, 60))
    
    # Create time series plot
    p <- plot_ly() %>%
      add_trace(
        data = ts_data,
        x = ~ year,
        y = ~ value,
        type = "scatter",
        mode = "lines+markers",
        name = input$country,
        line = list(color = "lightgray", width = 3),
        marker = list(color = "#00bc8c") 
      ) %>%
      add_trace(
        data = global_means,
        x = ~ year,
        y = ~ mean,
        type = "scatter",
        mode = "lines",
        name = "Global Average",
        line = list(
          color = "lightgray",
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
          #tick0 = 0,
          #dtick = 2, # extend y axis
          showgrid = TRUE,
          gridcolor = "#444",
          color = "#FFFFFF",
          tickfont = list(color = "#FFFFFF")
        ),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = 0.92,
          yanchor = "bottom",
          font = list(color = "#FFFFFF")
        ),
        margin = list(t = 50),
        plot_bgcolor = "#222222",
        paper_bgcolor = "#222222",
        font = list(color = "#FFFFFF")
      )
    
    return(p)
  })
  
  
  ### Country Proportion Time series
  output$country_time_props <- renderPlotly({
    req(input$country)
    
    # Create color vector for classes
    class_colors <- c("Natural (<10)" = "#7B2982", "Used (>10 & <35)" = "#E16462", "Settlements (>35)" = "#F0F921")
    
    # Create plotly stacked bar chart
    plot_ly(
      data = country_props %>% filter(name == input$country),
      x = ~year,
      y = ~proportion,
      color = ~class,
      colors = class_colors,
      type = "bar",
      hoverinfo = "text",
      text = ~paste("Class:", class, "<br>Proportion:", round(proportion, 3), "<br>Year:", year),
      textposition = "none"
    ) %>%
      layout(
        title = list(
          text = "Proportional Change Over Time",
          font = list(size = 14, color = "#FFFFFF"),
          y = 0.95,
          x = 0.5,  # Center the title (x = 0.5)
          xanchor = "center"  # Ensure title is centered
        ),
        xaxis = list(
          title = "",
          type = "category",
          #tickangle = -45,
          tickmode = "array",
          tickvals = years,  # Force all years to be shown
          ticktext = years,  # Labels for the ticks
          tickfont = list(size = 10),  # Smaller font to fit all values
          showgrid = TRUE,
          gridcolor = "#444",
          color = "#FFFFFF",
          tickfont = list(color = "#FFFFFF")
        ),
        yaxis = list(
          title = "Proportion",
          showgrid = TRUE,
          gridcolor = "#444",
          color = "#FFFFFF",
          tickfont = list(color = "#FFFFFF")
        ),
        legend = list(
          orientation = "v",
          x = 0.5,
          xanchor = "center",
          y = -0.3,
          yanchor = "top",
          itemwidth = 60,
          font = list(color = "#FFFFFF"),
          traceorder = "reversed"  # Ensures the order follows the factor levels
        ),
        plot_bgcolor = "#222222",
        paper_bgcolor = "#222222",
        #font = list(color = "#FFFFFF")
        #yaxis = list(title = "Proportion"),
        barmode = "stack",
        #legend = list(title = list(text = "Class")),
        hoverlabel = list(bgcolor = "white"),
        margin = list(t = 40, b = 100)  # Increased bottom margin to accommodate labels
      )
    
  })
  
  
  
  

  
  ### Country ridgline time series chart
  
  output$country_time_ridgeline <- renderPlotly({
    
    plot_ly(
      data = country_freqs %>% filter(year == input$year & name == input$country),
      x = ~value,
      y = ~count,
      type = "bar",
      marker = list(color = "#00bc8c", opacity = 0.75)
    ) %>%
      layout(
        title = list(
          text = paste(input$year, input$country, "Distribution of HFI Values"),
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
          tickformat = ".2e"  # Scientific notation
        ),
        plot_bgcolor = "#222222",
        paper_bgcolor = "#222222",
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
    
  })
  
  
  ### IPCC time series (similar structure to country) -----------
  output$ipcc_time_series <- renderPlotly({
    req(input$ipcc)
    
    
    ts_data <- ipcc %>% 
      st_drop_geometry() %>% 
      select(NAME, mlHFI_1999:mlHFI_2024) %>% 
      pivot_longer(cols = -NAME,
                   names_to = "year",
                   values_to = "value") %>% 
      separate(year, sep = "_", into = c("var", "year")) %>% 
      select(-var) %>% 
      filter(NAME == input$ipcc)
    
    # Global average for comparison
    #global_data <- data.frame(year = years_data, value = runif(length(years_data), 40, 60))
    
    # Create time series plot
    p <- plot_ly() %>%
      add_trace(
        data = ts_data,
        x = ~ year,
        y = ~ value,
        type = "scatter",
        mode = "lines+markers",
        name = input$ipcc,
        line = list(color = "lightgray", width = 3),
        marker = list(color = "#00bc8c")
      ) %>%
      add_trace(
        data = global_means,
        x = ~ year,
        y = ~ mean,
        type = "scatter",
        mode = "lines",
        name = "Global Average",
        line = list(
          color = "lightgray",
          width = 2,
          dash = "dash"
        )
      ) %>%
      layout(
        title = list(
          text = paste0("HFI Trend for ", input$ipcc),
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
          #tick0 = 0,
          #dtick = 2, # extend y axis
          showgrid = TRUE,
          gridcolor = "#444",
          color = "#FFFFFF",
          tickfont = list(color = "#FFFFFF")
        ),
        legend = list(
          orientation = "h",
          x = 0.5,
          xanchor = "center",
          y = 0.92,
          yanchor = "bottom",
          font = list(color = "#FFFFFF")
        ),
        margin = list(t = 50),
        plot_bgcolor = "#222222",
        paper_bgcolor = "#222222",
        font = list(color = "#FFFFFF")
      )
    
    return(p)
  })
  
  
  ### IPCC Proportion Time series
  output$ipcc_time_props <- renderPlotly({
    req(input$ipcc)
    
    # Create color vector for classes
    class_colors <- c("Natural (<10)" = "#7B2982", "Used (>10 & <35)" = "#E16462", "Settlements (>35)" = "#F0F921")
    
    # Create plotly stacked bar chart
    plot_ly(
      data = ipcc_props %>% filter(NAME == input$ipcc),
      x = ~year,
      y = ~proportion,
      color = ~class,
      colors = class_colors,
      type = "bar",
      hoverinfo = "text",
      text = ~paste("Class:", class, "<br>Proportion:", round(proportion, 3), "<br>Year:", year),
      textposition = "none"
    ) %>%
      layout(
        title = list(
          text = "Proportional Change Over Time",
          font = list(size = 14, color = "#FFFFFF"),
          y = 0.95,
          x = 0.5,  # Center the title (x = 0.5)
          xanchor = "center"  # Ensure title is centered
        ),
        xaxis = list(
          title = "",
          type = "category",
          #tickangle = -45,
          tickmode = "array",
          tickvals = years,  # Force all years to be shown
          ticktext = years,  # Labels for the ticks
          tickfont = list(size = 10),  # Smaller font to fit all values
          showgrid = TRUE,
          gridcolor = "#444",
          color = "#FFFFFF",
          tickfont = list(color = "#FFFFFF")
        ),
        yaxis = list(
          title = "Proportion",
          showgrid = TRUE,
          gridcolor = "#444",
          color = "#FFFFFF",
          tickfont = list(color = "#FFFFFF")
        ),
        legend = list(
          orientation = "v",
          x = 0.5,
          xanchor = "center",
          y = -0.3,
          yanchor = "top",
          itemwidth = 60,
          font = list(color = "#FFFFFF"),
          traceorder = "reversed"  # Ensures the order follows the factor levels
        ),
        plot_bgcolor = "#222222",
        paper_bgcolor = "#222222",
        #font = list(color = "#FFFFFF")
        #yaxis = list(title = "Proportion"),
        barmode = "stack",
        #legend = list(title = list(text = "Class")),
        hoverlabel = list(bgcolor = "white"),
        margin = list(t = 40, b = 100)  # Increased bottom margin to accommodate labels
      )
    
  })
  
  ### IPCC ridgline time series chart
  
  output$ipcc_time_ridgeline <- renderPlotly({
    
    plot_ly(
      data = ipcc_freqs %>% filter(year == input$year & NAME == input$ipcc),
      x = ~value,
      y = ~count,
      type = "bar",
      marker = list(color = "#00bc8c", opacity = 0.75)
    ) %>%
      layout(
        title = list(
          text = paste(input$year, input$ipcc, "Distribution of HFI Values"),
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
          tickformat = ".2e"  # Scientific notation
        ),
        plot_bgcolor = "#222222",
        paper_bgcolor = "#222222",
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
    
  })
  
  
  
  ### Global time series (kept from original) -----------------------
  output$timeSeries <- renderPlotly({
    # Data preparation

    # Create Plotly figure
    plot_ly(
      global_means,
      x = ~ year,
      y = ~ mean,
      type = "scatter",
      mode = "lines+markers",
      line = list(
        color = "lightgray",
        width = 2,
        shape = "linear"
      ),
      marker = list(color = "lightgray", size = 6),
      name = "Global Mean",
      legendgroup = "avg",
      showlegend = FALSE
    ) %>%
      # Highlight selected year
      add_trace(
        data = global_means %>% filter(year == input$year),
        x = ~ year,
        y = ~ mean,
        type = "scatter",
        mode = "markers",
        marker = list(color = "#00bc8c", size = 12),
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