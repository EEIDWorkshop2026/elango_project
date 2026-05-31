# Packages

library(tidyverse)
library(sf)
library(terra)
#Gives shapes and locations of countries/states
library(rnaturalearth)
#Gives worldclim data for temp and precip
library(geodata)

# Loading Datasets

load("mosqDF.Rsave")

mosqDF_pert <- mosqDF %>% 
  filter(str_detect(species, "perturbans"), sample_value > 0)


mosqDF_mel <- mosqDF %>% 
  filter(str_detect(species, "melanura"), sample_value > 0)

# Load USA states data
usa_states <- ne_states(country = "United States of America", returnclass = "sf")

# Filter for Virginia
virginia <- subset(usa_states, name == "Virginia")

# Plot with spatial cropping window
ggplot() +
  geom_sf(data = virginia, fill = "lightblue", color = "white") +
  geom_point(data = mosqDF_mel, aes(x = sample_long_dd, y = sample_lat_dd), color = "red", size = 1, alpha = 0.5) +
  geom_point(data = mosqDF_pert, aes(x = sample_long_dd, y = sample_lat_dd), fill = "blue", color = "blue", size = 1, alpha = 0.2, shape = 22) +
  coord_sf(xlim = c(-76.88, -76.4), ylim = c(36.95, 36.5), expand = FALSE) +
  theme_minimal()

#========================================================================================
# Co-variate Analysis
library(daymetr)

# # 1. Combine and format your unique coordinates
# # daymetr requires a specific dataframe structure: site, latitude, longitude
# unique_coords <- bind_rows(
#   mosqDF_mel %>% select(sample_long_dd, sample_lat_dd),
#   mosqDF_pert %>% select(sample_long_dd, sample_lat_dd)
# ) %>%
#   distinct() %>%
#   # Create a unique ID for each site
#   mutate(site_id = paste0("site_", row_number())) %>%
#   # Reorder columns to match daymetr requirements
#   select(site_id, sample_lat_dd, sample_long_dd)
# 
# # 2. Extract Daymet data for each specific point
# # We use purrr::map2 to loop through each latitude/longitude pair and query the API
# # Note: You must specify a time range. Daymet provides DAILY data.
# start_year <- 2023
# end_year <- 2023
# 
# daymet_points <- unique_coords %>%
#   mutate(
#     # Fetch the weather data internally (returns a nested list for each site)
#     weather_data = purrr::map2(sample_lat_dd, sample_long_dd, ~{
#       download_daymet(
#         site = "mosquito_site",
#         lat = .x,
#         lon = .y,
#         start = start_year,
#         end = end_year,
#         internal = TRUE # Keeps data in R rather than saving files to disk
#       )$data
#     })
#   ) %>%
#   # Unnest the list to expand the daily data back into a flat dataframe
#   unnest(weather_data)

#========================================================================================

load("daymet_points.Rsave")

# 3. Clean up the columns and calculate average temperature
daymet_final <- daymet_points %>%
  rename(
    lat = sample_lat_dd,
    long = sample_long_dd,
    year = year,
    yday = yday,
    max_temp = tmax..deg.c.,
    min_temp = tmin..deg.c.,
    precip = prcp..mm.day.,
    vapor = vp..Pa.
  ) %>%
  mutate(
    # Replicate your previous temp_avg calculation
    avg_temp = (max_temp + min_temp) / 2
  ) %>%
  # Keep only the columns you care about
  select(site_id, lat, long, year, yday, min_temp, max_temp, avg_temp, precip, vapor)

head(daymet_final)

#========================================================================================
# Making 2 separate Dataframes


# 1. Create a Daymet dataframe exclusively for the melanura coordinates
daymet_mel <- daymet_final %>%
  inner_join(
    # Extract only the unique coordinates from the melanura dataset
    mosqDF_mel %>% select(sample_lat_dd, sample_long_dd) %>% distinct(),
    # Match the lat/long columns between the two dataframes
    by = c("lat" = "sample_lat_dd", "long" = "sample_long_dd")
  )

# 2. Create a Daymet dataframe exclusively for the perturbans coordinates
daymet_pert <- daymet_final %>%
  inner_join(
    # Extract only the unique coordinates from the perturbans dataset
    mosqDF_pert %>% select(sample_lat_dd, sample_long_dd) %>% distinct(),
    # Match the lat/long columns between the two dataframes
    by = c("lat" = "sample_lat_dd", "long" = "sample_long_dd")
  )

# View the results
head(daymet_mel)
head(daymet_pert)


#========================================================================================
# Daymet all pixels of Suffolk VA

#========================================================================================
# Daymet Data; grabbing every pixel within boundary of Suffolk, VA

library(tigris)

# 1. Define the exact bounding box from your plot image
# Longitude (x): -76.88 to -76.4 | Latitude (y): 36.5 to 36.95
plot_extent <- ext(-76.88, -76.4, 36.5, 36.95)
