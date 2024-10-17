# PACKAGES ----------------------------------------------------------------

library(osmdata)
library(tidyverse)
library(showtext)
library(sf)
# library(rnaturalearth)
# library(rnaturalearthdata)
# library(SpatialEpi)

# IMPORT FONTS ------------------------------------------------------------

font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

bbx <- rbind(x = c(-3.38, -3.1), y = c(55.92, 56))
colnames(bbx) <- c("min", "max")

motorway <- bbx |> 
  opq() |> 
  add_osm_feature(key = "highway",
                  value = "motorway") |> 
  osmdata_sf()

motorway_link <- bbx |> 
  opq() |> 
  add_osm_feature(key = "highway",
                  value = "motorway_link") |> 
  osmdata_sf()

primary <- bbx |> 
  opq() |> 
  add_osm_feature(key = "highway",
                  value = "primary") |> 
  osmdata_sf()

primary_link <- bbx |> 
  opq() |> 
  add_osm_feature(key = "highway",
                  value = "primary_link") |> 
  osmdata_sf()

secondary <- bbx |> 
  opq() |> 
  add_osm_feature(key = "highway",
                  value = "secondary") |> 
  osmdata_sf()

secondary_link <- bbx |> 
  opq() |> 
  add_osm_feature(key = "highway",
                  value = "secondary_link") |> 
  osmdata_sf()

tertiary <- bbx |> 
  opq() |> 
  add_osm_feature(key = "highway",
                  value = "tertiary") |> 
  osmdata_sf()

tertiary_link <- bbx |> 
  opq() |> 
  add_osm_feature(key = "highway",
                  value = "tertiary_link") |> 
  osmdata_sf()

residential <- bbx |> 
  opq() |> 
  add_osm_feature(key = "highway",
                  value = "residential") |> 
  osmdata_sf()

living_street <- bbx |> 
  opq() |> 
  add_osm_feature(key = "highway",
                  value = "living_street") |> 
  osmdata_sf()

unclassified <- bbx |> 
  opq() |> 
  add_osm_feature(key = "highway",
                  value = "unclassified") |> 
  osmdata_sf()

service <- bbx |> 
  opq() |> 
  add_osm_feature(key = "highway",
                  value = "service") |> 
  osmdata_sf()

footway <- bbx |> 
  opq() |> 
  add_osm_feature(key = "highway",
                  value = "footway") |> 
  osmdata_sf()

tram <- bbx |> 
  opq() |> 
  add_osm_feature(key = "railway",
                  value = "tram") |> 
  osmdata_sf()

tram_line <- tram$osm_lines |> 
  filter(wikipedia == "en:Edinburgh Trams")

tram_stops <- tram$osm_points |> 
  filter(!is.na(name))

p <- ggplot() +
  geom_sf(data = motorway$osm_lines,
          inherit.aes = FALSE,
          color = "#374c80",
          alpha = 0.3,
          linewidth = 0.2) +
  geom_sf(data = motorway_link$osm_lines,
          inherit.aes = FALSE,
          color = "#374c80",
          alpha = 0.3,
          linewidth = 0.2) +
  geom_sf(data = primary$osm_lines,
          inherit.aes = FALSE,
          color = "#374c80",
          alpha = 0.3,
          linewidth = 0.2) +
  geom_sf(data = primary_link$osm_lines,
          inherit.aes = FALSE,
          color = "#374c80",
          alpha = 0.3,
          linewidth = 0.2) +
  geom_sf(data = secondary$osm_lines,
          inherit.aes = FALSE,
          color = "#374c80",
          alpha = 0.3,
          linewidth = 0.2) +
  geom_sf(data = secondary_link$osm_lines,
          inherit.aes = FALSE,
          color = "#374c80",
          alpha = 0.3,
          linewidth = 0.2) +
  geom_sf(data = tertiary$osm_lines,
          inherit.aes = FALSE,
          color = "#374c80",
          alpha = 0.3,
          linewidth = 0.2) +
  geom_sf(data = tertiary_link$osm_lines,
          inherit.aes = FALSE,
          color = "#374c80",
          alpha = 0.3,
          linewidth = 0.2) +
  geom_sf(data = residential$osm_lines,
          inherit.aes = FALSE,
          color = "#374c80",
          alpha = 0.3,
          linewidth = 0.2) +
  geom_sf(data = living_street$osm_lines,
          inherit.aes = FALSE,
          color = "#374c80",
          alpha = 0.3,
          linewidth = 0.2) +
  geom_sf(data = unclassified$osm_lines,
          inherit.aes = FALSE,
          color = "#374c80",
          alpha = 0.3,
          linewidth = 0.2) +
  geom_sf(data = service$osm_lines,
          inherit.aes = FALSE,
          color = "#374c80",
          alpha = 0.3,
          linewidth = 0.2) +
  geom_sf(data = footway$osm_lines,
          inherit.aes = FALSE,
          color = "#374c80",
          alpha = 0.3,
          linewidth = 0.2) +
  geom_sf(data = tram_line$geometry,
          inherit.aes = FALSE,
          color = "#7a5195",
          linewidth = 2.5) +
  geom_sf(data = tram_stops$geometry,
          inherit.aes = FALSE,
          color = "#7a5195",
          size = 6, shape = 21, fill = "#001017") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#001017"))

ggsave(filename = "2024/02-MAPS/05_A_JOURNEY.png", plot = p, 
       dpi = 320, width = 12, height = 6)


+
  geom_sf(data = tram_line$geometry,
          inherit.aes = FALSE,
          color = "darkblue",
          alpha = 0.4,
          linewidth = 0.8)

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

