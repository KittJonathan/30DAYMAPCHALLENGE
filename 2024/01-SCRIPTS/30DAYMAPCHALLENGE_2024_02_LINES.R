# 30 DAY MAP CHALLENGE
# 2024
# 02 - LINES

# 📦 LOAD PACKAGES --------------------------------------------------------

pacman::p_load(tidyverse, showtext, sf)

# 🔠 IMPORT FONT ----------------------------------------------------------

font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

# ⬇️ IMPORT DATASET -------------------------------------------------------

# OS Data Hub
# https://osdatahub.os.uk/

water <- sf::st_read("2024/00-DATA/oprvrs_essh_gb/data/WatercourseLink.shp")

# 🧹 CLEAN DATASET --------------------------------------------------------

rivers <- water|> 
  filter(form == "inlandRiver")

# 📊 CREATE MAP -----------------------------------------------------------

p <- ggplot() +
  geom_sf(data = rivers, color = "#9ee1ff", linewidth = 0.08) +
  labs(title = "Rivers of Great Britain",
       caption = "Data source: https://osdatahub.os.uk") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#003f5c", colour = "#003f5c"),
        plot.background = element_rect(fill = "#003f5c", colour = "#003f5c"),
        plot.title = element_text(family = "Roboto Condensed",
                                  colour = "#9ee1ff", face = "bold",
                                  hjust = 0.5, size = 60, margin = margin(t = 10, b = 10)),
        plot.caption = element_text(family = "Roboto Condensed",
                                    colour = "#9ee1ff", face = "italic",
                                    hjust = 0.5, size = 25, margin = margin(b = 10)),
        plot.title.position = "plot",
        plot.caption.position = "plot")

# 💾 SAVE MAP -------------------------------------------------------------

ggsave(filename = "2024/02-MAPS/01_FINISHED/02_LINES.png", plot = p, 
       dpi = 320, width = 6, height = 6)
