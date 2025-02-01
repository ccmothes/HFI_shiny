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
  'tigris',
  'rnaturalearth',
  'rnaturalearthdata',
  'rnaturalearthhires',
  'geodata'
)

packages <- c(packages)


packageLoad(packages)


# source all functions --------------------------

# purrr::map(list.files(
#   path = "src/",
#   pattern = "*.R",
#   full.names = TRUE,
#   recursive = TRUE
# ),
# source)