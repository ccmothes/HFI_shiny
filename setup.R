# setup script for processing functions

#Install (if necessary) and load all required packages ----------------

packageLoad <-
  function(x) {
    for (i in 1:length(x)) {
      if (!x[i] %in% installed.packages()) {
        install.packages(x[i])
      }
      library(x[i], character.only = TRUE)
    }
  }

# vector of packages to load
packages <- c(
  'sf',
  'terra',
  'tidyverse',
  'leaflet',
  'shiny',
  'shinyWidgets',
  'bslib',
  'plotly',
  'tigris',
  'rnaturalearth',
  'rnaturalearthdata',
  'geodata',
  'remotes'
)

packages <- c(packages)


packageLoad(packages)

if (!"rnaturalearthhires" %in% installed.packages()) {
  remotes::install_github("ropensci/rnaturalearthhires")
}


# source all functions --------------------------

# purrr::map(list.files(
#   path = "src/",
#   pattern = "*.R",
#   full.names = TRUE,
#   recursive = TRUE
# ),
# source)