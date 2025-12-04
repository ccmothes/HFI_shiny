source("setup.R")

countries <- terra::vect("data/countries.shp")

ipcc <- terra::vect("data/IPCC_regions/referenceRegions.shp")

rasters <- terra::rast("data/cleaned_rasters.tif")


# value frequency
freqs_all <- terra::freq(rasters, zones = countries)

# add ID column to countries to join name
countries_join <- countries %>%
  as.data.frame() %>%
  select(name) %>%
  rowid_to_column(var = "zone")

# clean
freqs_clean <- freqs_all %>%
  mutate(year = case_when(layer == 1 ~ 1999,
                          layer == 2 ~ 2000,
                          layer == 3 ~ 2001,
                          layer == 4 ~ 2022,
                          layer == 5 ~ 2023,
                          layer == 6 ~ 2024)) %>%
  left_join(countries_join, by = "zone") %>%
  select(-c(layer, zone))

write_csv(freqs_clean, "data/mlhfi_distribution.csv")


# Proportion calculation
props_long <- freqs_clean %>%
  mutate(
    category = case_when(
      value < 10 ~ "low",
      value >= 10 & value <= 35 ~ "medium",
      value > 35 ~ "high"
    )
  ) %>%
  # Group by country, year, and category
  group_by(name, year, category) %>%
  summarise(category_count = sum(count), .groups = "drop") %>%
  # Calculate total count per country-year
  group_by(name, year) %>%
  mutate(
    total_count = sum(category_count),
    proportion = category_count / total_count
  ) %>%
  ungroup()

props_wide <- props_long %>%
  select(-c(category_count, total_count)) %>%
  pivot_wider(names_from = year, values_from = proportion) %>%
  mutate(across(everything(), ~ifelse(is.na(.), 0, .)))

# save
write_csv(props_long, "data/mlhfi_props_long.csv")
write_csv(props_wide, "data/mlhfi_props_wide.csv")

