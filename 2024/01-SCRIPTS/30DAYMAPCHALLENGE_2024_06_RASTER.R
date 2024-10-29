# 30 DAY MAP CHALLENGE
# 2024
# 06 - RASTER

# https://osdatahub.os.uk/downloads/open/Terrain50

# 📦 LOAD PACKAGES --------------------------------------------------------

pacman::p_load(tidyverse, showtext, sf, osmdata)

# 🔠 IMPORT FONT ----------------------------------------------------------

font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

# ⬇️ IMPORT DATASET -------------------------------------------------------

# bbx <- rbind(x = c(-3.20684, -3.17156), y = c(55.94491, 55.95369))
# bbx <- rbind(x = c(-3.5, -3), y = c(55.5, 56))
# colnames(bbx) <- c("min", "max")

# buildings <- bbx |> 
#   opq() |> 
#   add_osm_feature(key = "building") |> 
#   osmdata_sf()

tramway <- opq(bbox = "Edinburgh") |>
  add_osm_feature(key = "route",
                  value = "tram") |>
  osmdata_sf()

ggplot() +
  geom_sf(data = tramway$osm_multilines,
          aes(col = start_date))


place <- "Edinburgh"

buildings <- opq(place) |> 
  add_osm_feature(key = "building") |> 
  osmdata_sf()

map_bg <- map_data("world") |> 
  filter(region == "UK",
         subregion != "Northern Ireland")

ferry <- opq("Scotland") |> 
  add_osm_feature(key = "route",
                  value = "ferry") |> 
  osmdata_sf()

# 🧹 CLEAN DATASET --------------------------------------------------------

aberdeen_lerwick <- ferry$osm_lines |> 
  filter(name == "Aberdeen-Lerwick",
         operator == "NorthLink Ferries")

# 📊 CREATE MAP -----------------------------------------------------------

p <- ggplot() +
  geom_polygon(data = map_bg,
               aes(x = long, y = lat, group = group),
               fill = "#003f5c",
               alpha = 0.5) +
  geom_sf(data = aberdeen_lerwick$geometry,
          col = "#003f5c", linewidth = 2) +
  geom_point(aes(x = -2.1, y = 57.15),
             shape = 21, col = "#003f5c", fill = "#9ee1ff",
             size = 4, stroke = 2) +
  geom_point(aes(x = -1.12, y = 60.1),
             shape = 21, col = "#003f5c", fill = "#9ee1ff",
             size = 4, stroke = 2) +
  geom_text(aes(x = -1.95, y = 57.0, label = "Aberdeen"),
            family = "Roboto Condensed", size = 16,
            hjust = 0) +
  geom_text(aes(x = -0.85, y = 60.2, label = "Lerwick"),
            family = "Roboto Condensed", size = 16,
            hjust = 0) +
  geom_text(aes(x = -1.16, y = 58.5, label = "216 miles"),
            family = "Roboto Condensed", size = 10,
            hjust = 0) +
  coord_sf(xlim = c(-8, 0),
           ylim = c(56, 61)) +
  labs(title = "The longest ferry crossing in Scotland") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#9ee1ff", colour = "#9ee1ff"),
        plot.background = element_rect(fill = "#9ee1ff", colour = "#9ee1ff"),
        plot.title = element_text(family = "Roboto Condensed",
                                  colour = "#003f5c", face = "bold",
                                  hjust = 0.5, size = 60, margin = margin(t = 10, b = 10)),
        plot.caption = element_text(family = "Roboto Condensed",
                                    colour = "#003f5c", face = "italic",
                                    hjust = 0.5, size = 25, margin = margin(b = 10)),
        plot.title.position = "plot",
        plot.caption.position = "plot")

# 💾 SAVE MAP -------------------------------------------------------------

ggsave(filename = "2024/02-MAPS/01_FINISHED/05_A_JOURNEY.png", plot = p, 
       dpi = 320, width = 6, height = 6)

