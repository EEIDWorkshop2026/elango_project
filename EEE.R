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
# Daymet Data; grabbing every pixel within boundary of Suffolk, VA

# library(tigris)
# 
# library(tidyverse)
# library(terra)
# library(daymetr)
# 
# # 1. Define the exact bounding box from your plot image
# plot_extent <- ext(-76.88, -76.4, 36.5, 36.95)
# 
# # 2. Create a grid of points inside this box spaced ~1km apart
# empty_grid <- rast(plot_extent, res = 0.015, crs = "EPSG:4326")
# 
# # 3. Extract the latitude and longitude of every single pixel center
# pixel_coords <- as.data.frame(crds(empty_grid)) %>%
#   rename(
#     sample_long_dd = x,
#     sample_lat_dd = y
#   ) %>%
#   mutate(site_id = paste0("pixel_", row_number()))
# 
# # 4. Run the Daymet extraction with error handling for water pixels
# start_year <- 2023
# end_year <- 2023
# 
# daymet_points_all <- pixel_coords %>%
#   mutate(
#     weather_data = purrr::map2(sample_lat_dd, sample_long_dd, ~{
#       
#       #Sys.sleep(0.4) # Brief pause to keep the server happy
#       
#       # tryCatch prevents water pixels from crashing the loop
#       tryCatch({
#         download_daymet(
#           site = "mosquito_site",
#           lat = .x,
#           lon = .y,
#           start = start_year,
#           end = end_year,
#           internal = TRUE 
#         )$data
#       }, error = function(e) {
#         # If the server throws an "outside spatial coverage" error, return NULL
#         return(NULL)
#       })
#     })
#   ) %>%
#   # Filter out any pixels that returned NULL (the water cells)
#   filter(!purrr::map_lgl(weather_data, is.null)) %>%
#   # Unnest the successful land pixels into a flat dataframe
#   unnest(weather_data)

daymet_points_all <- readRDS("daymet_points_all.RDS")

# View the final dataset (water pixels will be cleanly skipped)
head(daymet_points_all)

#========================================================================================
# Co-variates for just a month

daymet_final_all <- daymet_points_all %>% 
  filter(yday >= 152, yday <= 182) %>% 
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
  mutate(avg_temp = (max_temp + min_temp)/2) %>% 
  select(site_id, lat, long, year, yday, min_temp, max_temp, avg_temp, precip, vapor) %>% 
  group_by(site_id, lat, long, year) %>% 
  summarize(
    # Calculate the average for temperatures and vapor pressure
    mean_min_temp = mean(min_temp, na.rm = TRUE),
    mean_max_temp = mean(max_temp, na.rm = TRUE),
    mean_avg_temp = mean(avg_temp, na.rm = TRUE),
    mean_vapor = mean(vapor, na.rm = TRUE),
    mean_precip = mean(precip, na.rm = TRUE), 
    .groups = "drop" # Ungroups the data so it doesn't cause issues later
  )

#========================================================================================
# Making DF into Raster

raster_df <- daymet_final_all %>%
  ungroup() %>% # Always a good habit after summarizing
  select(
    long, lat, # Coordinates MUST be first
    mean_min_temp, mean_max_temp, mean_avg_temp, mean_precip, mean_vapor # Variables follow
  )

# 2. Convert the dataframe directly into a SpatRaster
# type="xyz" tells terra that the first two columns are our spatial grid
# We assign the standard WGS84 latitude/longitude CRS (EPSG:4326)
daymet_raster <- rast(raster_df, type = "xyz", crs = "EPSG:4326")

# 3. Verify the conversion worked
print(daymet_raster)

# 4. Plot one of the layers to visually confirm the shape (e.g., Average Temp)
plot(daymet_raster[["mean_avg_temp"]], main = "Average Temp (Days 152-182)")

# 5. Save the new raster to your working directory
# This creates a single multi-band GeoTIFF file containing all 5 of your variables
writeRaster(daymet_raster, "suffolk_daymet_monthly_summary.tif", overwrite = TRUE)

daymet_suff <- readRDS("daymet_suff.RDS")

#========================================================================================

# source("rb.R")
# 
# df_mel <- daymet_mel %>% select(6:10)
# df_pert <- daymet_pert %>% select(6:10)
# 
# mela_rb <- rb(df_mel, v = 500, d = 2, p = 0.5)
# #pert_rb <- rb(df_pert, v = 500, d = 2, p = 0.5)
# 
# daymet_final_all_1 <- daymet_final_all %>% select(5:7, 9, 8) %>% rename(
#   min_temp = mean_min_temp,
#   max_temp = mean_max_temp,
#   avg_temp = mean_avg_temp,
#   precip = mean_precip,
#   vapor = mean_vapor
# )
# 
# 
# mela_preds <- rb.test(mela_rb, daymet_final_all_1)


#========================================================================================
# Min and Max of Each Covariate

summary_mel <- daymet_mel %>%
  summarise(
    pop = "mel",
    min_min_temp = min(min_temp, na.rm = TRUE),
    max_min_temp = max(min_temp, na.rm = TRUE),
    min_max_temp = min(max_temp, na.rm = TRUE),
    max_max_temp = max(max_temp, na.rm = TRUE),
    min_avg_temp = min(avg_temp, na.rm = TRUE),
    max_avg_temp = max(avg_temp, na.rm = TRUE),
    min_precip   = min(precip, na.rm = TRUE),
    max_precip   = max(precip, na.rm = TRUE),
    min_vapor    = min(vapor, na.rm = TRUE),
    max_vapor    = max(vapor, na.rm = TRUE)
  )

summary_pert <- daymet_pert %>%
  summarise(
    pop = "pert",
    min_min_temp = min(min_temp, na.rm = TRUE),
    max_min_temp = max(min_temp, na.rm = TRUE),
    min_max_temp = min(max_temp, na.rm = TRUE),
    max_max_temp = max(max_temp, na.rm = TRUE),
    min_avg_temp = min(avg_temp, na.rm = TRUE),
    max_avg_temp = max(avg_temp, na.rm = TRUE),
    min_precip   = min(precip, na.rm = TRUE),
    max_precip   = max(precip, na.rm = TRUE),
    min_vapor    = min(vapor, na.rm = TRUE),
    max_vapor    = max(vapor, na.rm = TRUE)
  )

daymet_min_max <- bind_rows(summary_mel, summary_pert)

print(daymet_min_max)