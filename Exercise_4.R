library(rvest)
library(tidyverse)
library(janitor)
library(tidygeocoder)
library(maps)

# Read and clean table
url <- "https://en.wikipedia.org/wiki/List_of_United_States_cities_by_crime_rate_(2012)" #using rvest package, look trhough the webpage and get all the goodies and read them into R 
citiesCR <- read_html(url) %>% html_table(fill = TRUE) #html_table looks through all your html stuff and finds the tables, fill=TRUE helps with bad tables that have missing data or similar. Note, this is going to save it as a list of tables! 
citiesCR <- clean_names(citiesCR[[1]]) #citiesCR[[1]] accesses the first table in your list of tables
#clean_names uses hte janitor package to clean up column names. clean_names turns everything into snake case 

# Prepare and geocode using dplyr package 
citiesCR <- citiesCR %>%
  mutate(location = paste(city, state, sep = ", ")) %>% 
  #mutate creates or changes columns. We are creating a column called location and co,mbine the   values of city and state into a string like, "los angeles, California"
  geocode(location, method = "osm", lat = latitude, long = longitude)
  #use the ggmap package to send the location strings to a geocoding place to get lat and long 
  #method = osm = method OpenStreetMap (geocoder provider, free, open source) 
  #The new coordinates get stored into new columns: latitude and longitude

head(citiesCR) #just checking out our data 
names(citiesCR)

citiesCR <- citiesCR %>% slice(-c(1, 2)) #additional cleanup. Slice selects rows by position, we want to remove the first two rows from the dataframe because they are bad apparently 

#The column names in the Wikipedia table caused problems
#so, specify "violent_crime" manually

citiesCR <- citiesCR %>%
  rename(
    violent_crime = yearly_crime_rates_per_100_000_people_1_2_6)

# Get a basic US map
us_map <- map_data("state") #map_data is from ggplot2, and the US map coordinates are from the maps package. Gives us a dataframe with lat/long points that outline the states' boundaries 

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
ggplot() + #create an empty ggplot object 
  geom_polygon(data = us_map, aes(x = long, y = lat, group = group),
               fill = "gray95", color = "gray70") +
  #draws filled in polygons (US states) to tell ggplot what the boundaries are, and fills them with grey colors 
  
  geom_point(
    data = citiesCR_lower48,
    aes(x = longitude, y = latitude, size = violent_crime, color = violent_crime),
    alpha = 0.7
  ) +
  #add a layer of points (the cities) over the base maps 
  #aes says where to place those cities, size is based on violent crime, and color is also based on violent crime so the more violent places get bigger and brighter 
  # alpha = 0.7 makes the points a bit transparent so overlapping cities are easier to see 
  
  scale_color_viridis_c(option = "plasma") +
  #applies Viridis color scale gradient 
  #These are colorblind friendly, uniform, and jus tlook nice 
  
  labs(
    title = "Violent Crime Rates in Major U.S. Cities (2012)",
    size = "Crime rate",
    color = "Crime rate",
    x = "Longitude",
    y = "Latitude"
  ) +
  #these are just labels 
  
  theme_minimal() 
# minimal theme removes 'unnecessary' chart things, like backgrounds, gridlines, borders 

