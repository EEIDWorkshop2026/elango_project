# Libraries
library(tidyverse)
library(terra)
library(sf)


# Sourced Functions
source("VecDyn_Dataset_Access.R")

#========================================================================

# DayMet Data Input

daymet <- read.csv("daymet.csv")

daymet <- daymet %>% 
  mutate(
    T_avg = (tmax..deg.c. + tmin..deg.c.)/2
  )

# VectDyn Data Input

getDataset(ID = 220)

dataset <- dataset %>% 
  filter(year(sample_start_date) == 2021)

#========================================================================
# Rough Plot

plot <- ggplot() +
  # Use geom_col instead of geom_histogram when you have a specific y-axis variable
  geom_col(data = dataset, aes(x = yday(sample_start_date), y = sample_value), fill = "skyblue") + 
  # Your original temperature line
  geom_line(data = daymet, aes(x = yday, y = T_avg * 200), color = "red", linewidth = 1) +
  scale_y_continuous(
    name = "Sample Value", 
    sec.axis = sec_axis(~ . / 200, name = "Temperature (C)")
  ) +
  theme_bw() +
  labs(
    x = "Day of Year",
    y = "Value",
    title = "Dataset Values and Average Temperature by Day of Year"
  )

print(plot)

#========================================================================
