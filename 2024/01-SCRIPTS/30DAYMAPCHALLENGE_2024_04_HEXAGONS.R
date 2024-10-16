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

# https://osdatahub.os.uk/downloads/open/BoundaryLine
# https://www.paulamoraga.com/book-geospatial/sec-arealdataexamplespatial.html
# https://stackoverflow.com/questions/67694691/drawing-map-with-ggplot-and-draw-dots-in-map
# https://nrennie.rbind.io/blog/creating-typewriter-maps-r/
# https://rpubs.com/Hailstone/326118


# HEX MAP -----------------------------------------------------------------

sco_wales <- sf::st_read("2024/00-DATA/bdline_essh_gb/Data/GB/scotland_and_wales_region.shp")

world <- ne_countries(scale = "medium", returnclass = "sf")

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

