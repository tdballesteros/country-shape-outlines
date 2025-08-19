
# ...

# set variables ------------------------------------------------------------------------------------

# set the number of cities
number_of_cities <- 200

# set the number of connections between cities
number_of_connections <- 5

# load libraries -----------------------------------------------------------------------------------

library(sf)
library(osmdata)
library(alphahull)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)

# load data ----------------------------------------------------------------------------------------

# load Czechia city popuation data
## source: https://en.wikipedia.org/wiki/List_of_cities_and_towns_in_the_Czech_Republic
## Accessed 15 August 2025
czechia_cities <- read.csv("czech_cities_by_population.csv")

# Get world map data at a specified scale (e.g., "small", "medium", "large")
czechia_outline <- rnaturalearth::ne_countries(scale = "large", returnclass = "sf") %>%
  dplyr::filter(sovereignt == "Czechia") %>%
  dplyr::select(sovereignt)

# load custom functions ----------------------------------------------------------------------------

# pull Open Street Map coordinates of cities
city_coords_from_op_str_map <- function(city_name){
  city_coordinates <- osmdata::getbb(city_name) %>% # Obtain the bounding box corners from open street map
    t() %>% 
    data.frame() %>%
    sf::st_as_sf(coords = c("x", "y")) %>%
    sf::st_bbox() %>% # get the bounding box of the corners
    sf::st_as_sfc() %>% # convert bounding box to polygon
    sf::st_centroid() %>% # get the centroid of the polygon
    sf::st_as_sf() %>% # store as simple feature
    sf::`st_crs<-`(4326)  # set the coordinate system to WGS84 (GPS etc.)
  
  city_coordinates %>% 
    dplyr::mutate(name_of_city = city_name) %>% # add input city name in a column
    dplyr::rename(geometry = x) %>% # Rename the coordinate column
    dplyr::relocate(name_of_city, geometry) %>% # reorder the columns
    return()
}

# format data --------------------------------------------------------------------------------------

czechia_cities <- czechia_cities %>%
  dplyr::mutate(City_full = paste0(City,", Czech Republic"))

# pull list of cities
cities_list <- dplyr::pull(czechia_cities, City_full)

# error list
cities_error <- c(
  "Brandýs nad Labem-Stará Bolesla, Czech Republic"
)
cities_list <- cities_list[!cities_list %in% cities_error]

# pull coordinates
x <- lapply(cities_list, city_coords_from_op_str_map)
x2 <- do.call(rbind.data.frame, x)

# combine with population df
x2 <- x2 %>%
  dplyr::left_join(czechia_cities,
                   dplyr::join_by("name_of_city" == "City_full"))


# calculate line segments --------------------------------------------------------------------------

df1 <- x2 %>%
  dplyr::slice_max(Population, n = number_of_cities) %>%
  dplyr::select(City = name_of_city)

df <- expand.grid(df1$City, df1$City) %>%
  dplyr::select(City1 = Var1,
                City2 = Var2) %>%
  dplyr::left_join(df1,
                   dplyr::join_by("City1" == "City")) %>%
  dplyr::left_join(df1,
                   dplyr::join_by("City2" == "City"))

for(r in 1:nrow(df)){
  
  dist <- sf::st_distance(df$geometry.x[r],
                          df$geometry.y[r])
  
  df$dist[r] <- dist
  
}

df <- df %>%
  # remove self-self relationships
  dplyr::filter(dist != 0) %>%
  # filter to only include the number_of_connections number of closest cities
  dplyr::group_by(City1) %>%
  dplyr::slice_min(dist, n = number_of_connections) %>%
  dplyr::ungroup() %>%
  dplyr::rename(
    POINTA = geometry.x,
    POINTB = geometry.y
  )

df <- df %>%
  mutate(line_segment = st_sfc(mapply(function(a, b) {
    # Combine the point geometries into a single multipoint feature
    points <- st_combine(c(a, b))
    
    # Cast the multipoint to a linestring to create the line segment
    st_cast(points, "LINESTRING")
  }, POINTA, POINTB))) %>%
  dplyr::select(geomety = line_segment) %>%
  sf::st_as_sf(crs = 4326)

ggplot() +
  geom_sf(data = czechia_outline) +
  geom_sf(data = df)

