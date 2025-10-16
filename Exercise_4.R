library(rvest)
library(tidyverse)
library(janitor)
library(tidygeocoder)
library(maps)

# Read and clean table
url <- "https://en.wikipedia.org/wiki/List_of_United_States_cities_by_crime_rate_(2012)"
citiesCR <- read_html(url) %>% html_table(fill = TRUE)
citiesCR <- clean_names(citiesCR[[1]])

# Prepare and geocode
citiesCR <- citiesCR %>%
  mutate(location = paste(city, state, sep = ", ")) %>%
  geocode(location, method = "osm", lat = latitude, long = longitude)

head(citiesCR)
names(citiesCR)

citiesCR <- citiesCR %>% slice(-c(1, 2))

#The column names in the Wikipedia table caused problems
#so, specify "violent_crime" manually

citiesCR <- citiesCR %>%
  rename(
    violent_crime = yearly_crime_rates_per_100_000_people_1_2_6)

# Get a basic US map
us_map <- map_data("state")

# Make sure your state names are lowercase to match map data
citiesCR <- citiesCR %>%
  mutate(state = tolower(state))

# Convert violent_crime to numeric
citiesCR <- citiesCR %>%
  mutate(violent_crime = as.numeric(gsub(",", "", violent_crime)))


# Filter geographic outliers
lon_min <- -125
lon_max <- -66
lat_min <- 24
lat_max <- 50

citiesCR_lower48 <- citiesCR %>%
  filter(longitude >= lon_min & longitude <= lon_max,
         latitude  >= lat_min  & latitude  <= lat_max)


# Plot
ggplot() +
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group),
               fill = "gray95", color = "gray70") +
  geom_point(
    data = citiesCR_lower48,
    aes(x = longitude, y = latitude, size = violent_crime, color = violent_crime),
    alpha = 0.7
  ) +
  scale_color_viridis_c(option = "plasma") +
  labs(
    title = "Violent Crime Rates in Major U.S. Cities (2012)",
    size = "Crime rate",
    color = "Crime rate",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()
