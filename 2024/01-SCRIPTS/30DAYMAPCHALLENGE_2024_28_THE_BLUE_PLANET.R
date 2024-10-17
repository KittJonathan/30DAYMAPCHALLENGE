# PACKAGES ----------------------------------------------------------------

# library(osmdata)
library(tidyverse)
library(ggOceanMaps)
library(ggspatial)
library(showtext)
# library(sf)
# library(rnaturalearth)
# library(rnaturalearthdata)
# library(SpatialEpi)

# https://mikkovihtakari.github.io/ggOceanMaps/articles/ggOceanMaps.html

# TEST --------------------------------------------------------------------

basemap(limits = 60)

basemap(limits = c(-20, 20, 40, 59))

dt <- data.frame(lon = c(120, 120, -120, -120), lat = c(60, 80, 80, 60))

basemap(limits = c(120, -120, 60, 80)) +
  ggspatial::geom_spatial_polygon(
    data = dt, 
    aes(x = lon, y = lat), fill = NA, color = "red")

basemap(limits = 60, projection.grid = TRUE, grid.col = "red")

basemap(limits = c(-2e6, 1e6, 0, 3e6), shapefiles = "Arctic") 

basemap(limits = c(-7.7, -0.178, 54, 59.68), bathymetry = TRUE)

basemap(limits = -60, glaciers = TRUE, shapefiles = "Antarctic")
