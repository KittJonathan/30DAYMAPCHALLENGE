# 30 DAY MAP CHALLENGE
# 2024
# 03 - POLYGONS

# 📦 LOAD PACKAGES --------------------------------------------------------

pacman::p_load(tidyverse, showtext, sf)

# 🔠 IMPORT FONT ----------------------------------------------------------

font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

# ⬇️ IMPORT DATASET -------------------------------------------------------

# OS Data Hub
# https://osdatahub.os.uk/

sco_wal <- sf::st_read("2024/00-DATA/bdline_essh_gb/Data/GB/scotland_and_wales_region.shp")

ggplot() +
  geom_sf(data = eng) +
  geom_sf(data = sco_wal)

# 🧹 CLEAN DATASET --------------------------------------------------------

sco <- sco_wal |> 
  filter(AREA_CODE == "SPE") |> 
  mutate(NAME = str_remove_all(NAME, " PER"),
         NAME = fct_relevel(NAME, "Highlands and Islands", "North East Scotland",
                              "Mid Scotland and Fife", "West Scotland", 
                              "Glasgow", "Central Scotland", "Lothian",
                              "South Scotland"))

# 📊 CREATE MAP -----------------------------------------------------------

cols <- c("Central Scotland" = "#56b4e9",
          "Glasgow" = "#000000",
          "Highlands and Islands" = "#e69f00",
          "Lothian" = "#f0e442",
          "South Scotland" = "#009e73",
          "Mid Scotland and Fife" = "#d55e00",
          "North East Scotland" = "#0072b2",
          
          "West Scotland" = "#cc79a7")

p <- ggplot() +
  geom_sf(data = sco,
          aes(fill = NAME),
          col = "white",
          linewidth = 0.05) +
  scale_fill_manual(values = cols) +
  labs(title = "Regions of Scotland",
       caption = "Data source: https://osdatahub.os.uk") +
  theme_void() +
  theme(panel.background = element_rect(fill = "#e7f7ff", colour = "#e7f7ff"),
        plot.background = element_rect(fill = "#e7f7ff", colour = "#e7f7ff"),
        plot.title = element_text(family = "Roboto Condensed",
                                  colour = "#003f5c", face = "bold",
                                  hjust = 0.5, size = 60, margin = margin(t = 10, b = 5)),
        plot.caption = element_text(family = "Roboto Condensed",
                                    colour = "#003f5c", face = "italic",
                                    hjust = 0.5, size = 25, margin = margin(b = 10)),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        legend.title = element_blank(),
        legend.position = "left",
        legend.text = element_text(family = "Roboto Condensed", colour = "#003f5c",
                                   size = 25),
        legend.key.size = unit(0.4, "cm"))

# 💾 SAVE MAP -------------------------------------------------------------

ggsave(filename = "2024/02-MAPS/01_FINISHED/03_POLYGONS.png", plot = p, 
       dpi = 320, width = 6, height = 6)
