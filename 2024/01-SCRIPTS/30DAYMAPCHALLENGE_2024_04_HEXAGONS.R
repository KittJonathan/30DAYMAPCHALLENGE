# 30 DAY MAP CHALLENGE
# 2024
# 04 - HEXAGONS

# 📦 LOAD PACKAGES --------------------------------------------------------

pacman::p_load(tidyverse, sf, showtext)

# 🔠 IMPORT FONT ----------------------------------------------------------

font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

# ⬇️ IMPORT DATASET -------------------------------------------------------

# OS Data Hub
# https://osdatahub.os.uk/

landslides <- sf::st_read("2024/00-DATA/GeoSureHexGrids/GeoSureHexGrids/Data/GB_Hex_5km_GS_Landslides_v8.shp")

# 📊 CREATE MAP -----------------------------------------------------------

cols <- c("Low" = "#ffffbf",
          "Moderate" = "#ffd480",
          "Significant" = "#e69900")

p <- ggplot() +
  geom_sf(data = landslides,
          aes(fill = Legend),
          col = "#9ee1ff") +
  scale_fill_manual(values = cols) +
  labs(title = "Landslide risk",
       caption = "Data source: https://osdatahub.os.uk") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#9ee1ff", colour = "#9ee1ff"),
        plot.background = element_rect(fill = "#9ee1ff", colour = "#9ee1ff"),
        plot.title = element_text(family = "Roboto Condensed",
                                  colour = "#003f5c", face = "bold",
                                  hjust = 0.5, size = 60, margin = margin(t = 10, b = 5)),
        plot.caption = element_text(family = "Roboto Condensed",
                                    colour = "#003f5c", face = "italic",
                                    hjust = 0.5, size = 25, margin = margin(b = 10)),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        legend.title = element_blank(),
        legend.position = "right",
        legend.text = element_text(family = "Roboto Condensed", colour = "#003f5c",
                                   size = 25),
        legend.key.size = unit(0.4, "cm"))

# 💾 SAVE MAP -------------------------------------------------------------

ggsave(filename = "2024/02-MAPS/01_FINISHED/04_HEXAGONS.png", plot = p, 
       dpi = 320, width = 6, height = 6)