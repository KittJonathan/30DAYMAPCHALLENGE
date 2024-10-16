# PACKAGES ----------------------------------------------------------------

# library(osmdata)
library(tidyverse)
library(showtext)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
# library(SpatialEpi)

# IMPORT FONTS ------------------------------------------------------------

font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

bbx <- rbind(x = c(-3.38421, -3.04295), y = c(55.88519, 56.01360))
colnames(bbx) <- c("min", "max")

tram <- bbx |> 
  opq() |> 
  add_osm_feature(key = "railway",
                  value = "tram") |> 
  osmdata_sf()

tram2 <- tram$osm_lines |> 
  filter(wikipedia == "en:Edinburgh Trams")

stops <- tram$osm_points |> 
  filter(!is.na(name))

tram$osm_points$name

ggplot() +
  geom_sf(data = tram2$geometry) +
  geom_sf(data = stops$geometry)

# HEX MAP -----------------------------------------------------------------

# world <- ne_countries(scale = "medium", returnclass = "sf")

sort(unique(world$geounit))

world <- map_data("world")

test <- world |> 
  filter(region == "UK", subregion == "Scotland") |> 
  sf::st_as_sf(coords = c("long", "lat")) |> 
  group_by(subregion, group) |> 
  summarise(do_union = FALSE) |> 
  st_cast("POLYGON") |> 
  ungroup()

ggplot() +
  geom_sf(data = test)

unique(world$region)



uk <- world |> 
  filter(sovereignt == "United Kingdom")

sort(unique(uk$admin))

UK <- ne_countries(scale = "medium",
                   country = "United Kingdom", 
                   returnclass = "sf") |>
  st_geometry() |> ## only geometry needed to clip the hexagonal grid
  st_transform(27700) ## reproject to British National Grid

hexgrid <- st_make_grid(UK,
                        cellsize = 2e4, ## unit: metres; change as required
                        what = 'polygons',
                        square = FALSE ## !
) |>
  st_as_sf()

hexgrid_UK <- hexgrid[c(unlist(st_contains(UK, hexgrid)), 
                        unlist(st_overlaps(UK, hexgrid))) ,] 

UK |> plot(col = "white")
hexgrid_UK |> plot(add = TRUE)

ggplot(data = hexgrid_UK) +
  geom_sf()


# TEST --------------------------------------------------------------------

UK <- ne_countries(scale = "large",
                   country = "United Kingdom",
                   returnclass = "sf") |>
  st_geometry() |> ## only geometry needed to clip the hexagonal grid
  st_transform(27700) ## reproject to British National Grid


hexgrid <- st_make_grid(UK,
                        cellsize = 2e4, ## unit: metres; change as required
                        what = 'polygons',
                        square = FALSE ## !
) |>
  st_as_sf()

hexgrid_UK <- hexgrid[c(unlist(st_contains(UK, hexgrid)), 
                        unlist(st_overlaps(UK, hexgrid))) ,] 

scotland_boundaries <- sf::st_read("2024/00-DATA/bdline_essh_gb/Data/Supplementary_Country/country_region.shp") |> 
  filter(NAME == "Scotland")

ggplot() +
  geom_sf(data = scotland_boundaries)

test <- scotland_boundaries |> 
  st_geometry()

hexgrid <- st_make_grid(test,
                        cellsize = 100, ## unit: metres; change as required
                        what = 'polygons',
                        square = FALSE ## !
) |>
  st_as_sf()

ggplot(data = hexgrid) +
  geom_sf()

world <- map_data("world")

scotland <- world |> 
  filter(region == "UK", subregion == "Scotland")

test <- scotland |> 
  sf::st_as_sf(coords = c("long", "lat")) |> 
  group_by(subregion, group) |> 
  summarise(do_union = FALSE) |> 
  st_cast("POLYGON") |> 
  ungroup()

ggplot(test) +
  geom_sf()
  







world <- ne_countries(scale = 110)

small_scale_map <- ggplot() +
  geom_sf(data = world) +
  coord_sf(xlim = c(-20, 50), ylim = c(33, 80)) +
  ggtitle("Europe")

small_scale_map

europe <- ne_countries(scale = 50, continent = "Europe") 
medium_scale_map <- ggplot() +
  geom_sf(data = europe) +
  coord_sf(xlim = c(5, 30), ylim = c(55, 71)) +
  ggtitle("Norden")
medium_scale_map

