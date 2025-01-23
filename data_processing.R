# Code to process and test large rasters

source("setup.R")
install.packages("gdalUtilities")


hfi_1999 <- terra::rast("L:\\Projects_active\\2024_x2025_Human_Footprint_Index\\data\\1999_int16.tif")

hfi_1999_projected <- terra::project(hfi_1999, "EPSG:3857")

hfi_2023 <- terra::rast("L:\\Projects_active\\2024_x2025_Human_Footprint_Index\\data\\2023_int16.tif")

hfi_change <- terra::rast("L:\\Projects_active\\2024_x2025_Human_Footprint_Index\\data\\2023m1999_int16.tif")


# attempt plotting (takes so long due to projection)
leaflet() %>%
addProviderTiles("OpenStreetMap") %>%
  addRasterImage(hfi_1999, colors = terrain.colors(100), opacity = 0.8)


# Define the input file and output directory
#input_raster <- "path_to_your_large_raster.tif"  # Replace with your raster file path
output_dir <- "data/"  # Directory to store the output tiles

# Define the zoom levels (e.g., from 0 to 5)
zoom_levels <- "0-5"

# Run gdal2tiles.py via system call
system(paste("gdal2tiles.py -z", zoom_levels, hfi_1999, output_dir))

