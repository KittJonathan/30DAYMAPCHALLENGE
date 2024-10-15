# PACKAGES ----------------------------------------------------------------

library(osmdata)
library(tidyverse)
library(showtext)
library(sf)

# IMPORT FONTS ------------------------------------------------------------

font_add_google("Roboto Condensed", "Roboto Condensed")
showtext_auto()

# CREATE MAP FUNCTION -----------------------------------------------------

create_map <- function(coords, place,
                       line_col, bg_fill, bg_col, border_col,
                       title_col, title_size) {
  
  min_lon <- coords |> 
    filter(city == place) |> 
    pull(min_lon)
  
  max_lon <- coords |> 
    filter(city == place) |> 
    pull(max_lon)
  
  min_lat <- coords |> 
    filter(city == place) |> 
    pull(min_lat)
  
  max_lat <- coords |> 
    filter(city == place) |> 
    pull(max_lat)
  
  bbx <- rbind(x = c(min_lon, max_lon), y = c(min_lat, max_lat))
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
  
  p <- ggplot() +
    geom_sf(data = motorway$osm_lines,
            inherit.aes = FALSE,
            color = line_col,
            alpha = 0.8,
            linewidth = 0.5) +
    geom_sf(data = motorway_link$osm_lines,
            inherit.aes = FALSE,
            color = line_col,
            alpha = 0.8,
            linewidth = 0.3) +
    geom_sf(data = primary$osm_lines,
            inherit.aes = FALSE,
            color = line_col,
            alpha = 0.6,
            linewidth = 0.5) +
    geom_sf(data = primary_link$osm_lines,
            inherit.aes = FALSE,
            color = line_col,
            alpha = 0.6,
            linewidth = 0.3) +
    geom_sf(data = secondary$osm_lines,
            inherit.aes = FALSE,
            color = line_col,
            alpha = 0.6,
            linewidth = 0.3) +
    geom_sf(data = secondary_link$osm_lines,
            inherit.aes = FALSE,
            color = line_col,
            alpha = 0.6,
            size = 0.3) +
    geom_sf(data = tertiary$osm_lines,
            inherit.aes = FALSE,
            color = line_col,
            alpha = 0.5,
            linewidth = 0.2) +
    geom_sf(data = tertiary_link$osm_lines,
            inherit.aes = FALSE,
            color = line_col,
            alpha = 0.5,
            linewidth = 0.2) +
    geom_sf(data = residential$osm_lines,
            inherit.aes = FALSE,
            color = line_col,
            alpha = 0.4,
            linewidth = 0.2) +
    geom_sf(data = living_street$osm_lines,
            inherit.aes = FALSE,
            color = line_col,
            alpha = 0.4,
            linewidth = 0.2) +
    geom_sf(data = unclassified$osm_lines,
            inherit.aes = FALSE,
            color = line_col,
            alpha = 0.4,
            linewidth = 0.2) +
    geom_sf(data = service$osm_lines,
            inherit.aes = FALSE,
            color = line_col,
            alpha = 0.4,
            linewidth = 0.2) +
    geom_sf(data = footway$osm_lines,
            inherit.aes = FALSE,
            color = line_col,
            alpha = 0.4,
            linewidth = 0.2) +
    coord_sf(xlim = c(min_lon, max_lon), 
             ylim = c(min_lat, max_lat),
             expand = FALSE) +
    labs(title = place) +
    theme_void() +
    theme(panel.background = element_rect(fill = bg_fill,
                                          colour = border_col,
                                          linewidth = 0.08),
          plot.background = element_rect(fill = bg_fill, colour = bg_col),
          plot.margin = margin(10, 10, 10, 10),
          plot.title = element_text(family = "Roboto Condensed",
                                    colour = title_col,
                                    size = title_size,
                                    hjust = 0.5,
                                    margin = margin(b = 5)))
  
  p
  
  }

# CREATE COORD TABLE ------------------------------------------------------

# https://fr.wikipedia.org/wiki/Cha%C3%AEne_des_Puys

coords <- tibble(
  name = c("Gour de Tazenat", "Puy de Montiroir", "Puy de Chalard", "Suc de Beaunit",
           "Puy de Monceau", "Puy de Verrières", "Puy du Thiolet", "Puy de Champ Valleix",
           "Puy de Pradet", "Puy de Paugnat", "Puy de la Bannière", "Puy de la Baneyre",
           "Puy des Marais", "Puy de la Gouly", "Puy de l'Espinasse", "Suc de la Louve",
           "Puy de Tressous", "Puy de la Nugère", "Puy de Ténuzet", "Puy de Louchadière",
           "Puy de Jume", "Puy de la Coquille", "Puy de Clermont", "Puy des Gouttes",
           "Puy Chopine", "Puy de Chaumont", "Petit Sarcoui", "Puy de Lemptégy",
           "Grand Sarcoui", "Croix Mory", "Puy des Goulles", "Puy de Fraisse",
           "Puy de Côme", "Puy Pariou", "Puy de Clierzou", "Grand Suchet",
           "Petit Suchet", "Puy Balmet", "Puy Fillu", "Puy Plantas"),
  long = c(2.99099, 2.92679, 2.97418, 2.96742,
           2.98573, 2.95529, 2.96236, 2.99160,
           2.95952, 2.97789, 3.03306, 2.95601,
           2.97586, 2.96017, 2.95553, 2.99016,
           2.95191, 2.98552, 2.96423, 2.95081,
           2.96684, 2.96470, 2.95702, 2.95133,
           2.95503, 2.96972, 2.98359, 2.94799,
           2.98223, 2.96892, 2.97885, 2.96256,
           2.94251, 2.97155, 2.96231, 2.95170,
           2.96135, 2.94000, 2.94243, 3.00570),
  lat = c(45.98054, 45.97013, 45.95888, 45.90087,
          45.89964, 45.89166, 45.89178, 45.89208,
          45.88219, 45.88020, 45.87886, 45.87663,
          45.87412, 45.87296, 45.86987, 45.86395,
          45.86438, 45.86054, 45.85581, 45.85293,
          45.84820, 45.84287, 45.83501, 45.82381,
          45.82821, 45.82443, 45.81908, 45.81767,
          45.81469, 45.81305, 45.80890, 45.80480,
          45.79684, 45.79683, 45.78943, 45.78942,
          45.78985, 45.78797, 45.78377, 45.78201)
)

ggplot(data = coords) +
  geom_point(aes(x = long, y = lat))

# +
#   geom_text(aes(x = long, y = lat, label = name))

# coords <- tibble(
#   city = c("Paisley", "Toulouse & Cugnaux", "Le Havre", "Le Havre 2",
#            "Villefranche-sur-Saône", "Yzeure"),
#   min_lon = c(-4.479, 1.315, 0.04270, 0.04270, 4.66569, 3.31523),
#   max_lon = c(-4.390, 1.505, 0.25480, 0.21, 4.76057, 3.36928),
#   min_lat = c(55.815, 43.521, 49.44673, 49.43, 45.95780, 46.55027),
#   max_lat = c(55.865, 43.652, 49.54325, 49.55, 46.00233, 46.57820))

# coords <- tibble(
#   city = c("Paisley", "Toulouse & Cugnaux", "Le Havre",
#            "Villefranche-sur-Saône", "Yzeure"),
#   min_lon = c(-4.49, 1.31, 0.042, 4.67, 3.3),
#   max_lon = c(-4.40, 1.52, 0.22, 4.77, 3.38),
#   min_lat = c(55.81, 43.52, 49.44, 45.95, 46.53),
#   max_lat = c(55.87, 43.66, 49.55, 46.01, 46.59)
# )

coords <- tibble(
  city = c("Grenoble", "Genève", "Lille", "Besançon"),
  min_lon = c(5.67, 6.10, 3.02, 5.95),
  max_lon = c(5.80, 6.22, 3.10, 6.05),
  min_lat = c(45.11, 46.15, 50.60, 47.20),
  max_lat = c(45.21, 46.25, 50.66, 47.28)
)

coords |> 
  mutate(diff_lon = abs(max_lon - min_lon),
         diff_lat = abs(max_lat - min_lat),
         lon_lat_ratio = diff_lon / diff_lat)

# min_lon <- 3.31523
# max_lon <- 3.36928
# min_lat <- 46.55027
# max_lat <- 46.57820

# 1. PAISLEY --------------------------------------------------------------

paisley <- create_map(coords = coords, place = "Paisley",
  # min_lon = -4.479, max_lon = -4.390, min_lat = 55.815, max_lat = 55.865,
  line_col = "#ffbe7f", bg_fill = "#282828", bg_col = "#282828", border_col = "#fff7ed",
  title_col = "#ffbe7f", title_size = 75) +
  geom_point(aes(x = -4.46669, y = 55.82676),
             col = "#ed5309",
             size = 5) +
  geom_text(aes(x = -4.46669, y = 55.82676),
            col = "#fff7ed",
            label = "1", size = 10,
            family = "Roboto Condensed", hjust = 0.5, fontface = "bold")

ggsave("../../Pictures/20240712_SELECTION/MAPS/01_PAISLEY.png", paisley,
       width = 6, height = 6, scale = 1, dpi = 320)

# 2. TOULOUSE & CUGNAUX ---------------------------------------------------

toulouse_cugnaux <- create_map(coords = coords, place = "Toulouse & Cugnaux",
  # min_lon = 1.315, max_lon = 1.505, min_lat = 43.521, max_lat = 43.652,
  line_col = "#ffbe7f", bg_fill = "#282828", bg_col = "#282828", border_col = "#fff7ed",
  title_col = "#ffbe7f", title_size = 75) +
  geom_point(aes(x = 1.39514, y = 43.56455),
             col = "#ed5309",
             size = 5) +
  geom_text(aes(x = 1.39514, y = 43.56455),
            col = "#fff7ed",
            label = "2", size = 10,
            family = "Roboto Condensed", hjust = 0.5, fontface = "bold") +
  geom_point(aes(x = 1.33729, y = 43.53766),
             col = "#ed5309",
             size = 5) +
  geom_text(aes(x = 1.33729, y = 43.53766),
            col = "#fff7ed",
            label = "3", size = 10,
            family = "Roboto Condensed", hjust = 0.5, fontface = "bold")

ggsave("../../Pictures/20240712_SELECTION/MAPS/02_TOULOUSE_CUGNAUX.png", toulouse_cugnaux,
       width = 6, height = 6, dpi = 320, scale = 1)

# 3. LE HAVRE -------------------------------------------------------------

le_havre <- create_map(coords = coords, place = "Le Havre",
                       # min_lon = 1.315, max_lon = 1.505, min_lat = 43.521, max_lat = 43.652,
                       line_col = "#ffbe7f", bg_fill = "#282828", bg_col = "#282828", border_col = "#fff7ed",
                       title_col = "#ffbe7f", title_size = 75) +
  geom_point(aes(x = 0.11882, y = 49.52876),
             col = "#ed5309",
             size = 5) +
  geom_text(aes(x = 0.11882, y = 49.52876),
            col = "#fff7ed",
            label = "4", size = 10,
            family = "Roboto Condensed", hjust = 0.5, fontface = "bold") +
  geom_point(aes(x = 0.10384, y = 49.49595),
             col = "#ed5309",
             size = 5) +
  geom_text(aes(x = 0.10384, y = 49.49595),
            col = "#fff7ed",
            label = "5", size = 10,
            family = "Roboto Condensed", hjust = 0.5, fontface = "bold") +
  geom_point(aes(x = 0.18167, y = 49.50243),
             col = "#ed5309",
             size = 5) +
  geom_text(aes(x = 0.18167, y = 49.50243),
            col = "#fff7ed",
            label = "7", size = 10,
            family = "Roboto Condensed", hjust = 0.5, fontface = "bold")

ggsave("../../Pictures/20240712_SELECTION/MAPS/03_LE_HAVRE.png", le_havre,
       width = 6, height = 6, scale = 1, dpi = 320)

# 4. VILLEFRANCHE-SUR-SAONE -----------------------------------------------

villefranche_sur_saone <- create_map(coords = coords, place = "Villefranche-sur-Saône",
                      # min_lon = -4.479, max_lon = -4.390, min_lat = 55.815, max_lat = 55.865,
                      line_col = "#ffbe7f", bg_fill = "#282828", bg_col = "#282828", border_col = "#fff7ed",
                      title_col = "#ffbe7f", title_size = 75) +
  geom_point(aes(x = 4.73379, y = 45.97957),
             col = "#ed5309",
             size = 5) +
  geom_text(aes(x = 4.73379, y = 45.97957),
            col = "#fff7ed",
            label = "6", size = 10,
            family = "Roboto Condensed", hjust = 0.5, fontface = "bold")

ggsave("../../Pictures/20240712_SELECTION/MAPS/04_VILLEFRANCHE_SUR_SAONE.png", villefranche_sur_saone,
       width = 6, height = 6, dpi = 320)

# 5. YZEURE ---------------------------------------------------------------

yzeure <- create_map(coords = coords, place = "Yzeure",
                     # min_lon = 1.315, max_lon = 1.505, min_lat = 43.521, max_lat = 43.652,
                     line_col = "#ffbe7f", bg_fill = "#282828", bg_col = "#282828", border_col = "#fff7ed",
                     title_col = "#ffbe7f", title_size = 75) +
  geom_point(aes(x = 3.35740, y = 46.57196),
             col = "#ed5309",
             size = 5) +
  geom_text(aes(x = 3.35740, y = 46.57196),
            col = "#fff7ed",
            label = "8", size = 10,
            family = "Roboto Condensed", hjust = 0.5, fontface = "bold") +
  geom_point(aes(x = 3.36088, y = 46.55972),
             col = "#ed5309",
             size = 5) +
  geom_text(aes(x = 3.36088, y = 46.55972),
            col = "#fff7ed",
            label = "9", size = 10,
            family = "Roboto Condensed", hjust = 0.5, fontface = "bold")

ggsave("../../Pictures/20240712_SELECTION/MAPS/05_YZEURE.png", yzeure,
       width = 6, height = 6, scale = 1, dpi = 320)

# 6. TIMELINE -------------------------------------------------------------

timeline_data <- tibble(
  map_point = 1:9,
  map_point_x = 1974,
  map_point_y = c(45, 40, 35, 30, 25, 20, 15, 10, 5),
  address_x = 1974,
  address_y = c(48, 43, 38, 33, 28, 23, 18, 13, 8),
  address = c("4 Jarvie way, Paisley", "9 passage Louis Pergaud, Toulouse",
              "9 avenue des Pyrénées, Cugnaux", "17 allée Jean Prévost, Le Havre",
              "6 rue Pierre Faure, Le Havre", "1184 route de Riottier, Villefranche-sur-Saône",
              "465 rue de Verdun, Le Havre", "22 rue Clara Malraux, Yzeure",
              "3 rue Flora Tristan, Yzeure"),
  long_seg_start = 1974,
  long_seg_end = 2030,
  short_seg_start = c(1974, 1979, 1981, 1986, 1993, 2000, 2011, 2018, 2021),
  short_seg_end = c(1979, 1981, 1986, 1993, 2000, 2011, 2018, 2021, 2030))

timeline_plot <- timeline_data |> 
  ggplot() +
  geom_point(aes(x = map_point_x, y = map_point_y),
             col = "#ed5309", size = 5) +
  geom_text(aes(x = map_point_x, y = map_point_y, label = map_point),
            col = "#fff7ed", size = 10, hjust = 0.5,
            family = "Roboto Condensed", fontface = "bold") +
  geom_text(aes(x = map_point_x + 2, y = map_point_y, label = address),
            col = "#fff7ed", size = 14, hjust = 0,
            family = "Roboto Condensed", fontface = "bold") + 
  geom_segment(aes(x = long_seg_start, xend = long_seg_end,
                   y = map_point_y - 1.5, yend = map_point_y - 1.5),
               col = "#fff7ed", alpha = 0.2) +
  geom_segment(aes(x = short_seg_start, xend = short_seg_end,
                   y = map_point_y - 1.5, yend = map_point_y - 1.5),
               col = "#ffbe7f") +
  geom_point(aes(x = short_seg_start, y = map_point_y - 1.5),
             shape = 21, size = 2, col = "#ffbe7f", fill = "#282828") +
  geom_text(aes(x = short_seg_start, y = map_point_y - 2.5, label = short_seg_start),
            col = "#ffbe7f", size = 10, family = "Roboto Condensed", hjust = 1) +
  geom_point(data = filter(timeline_data, map_point != 9),
             aes(x = short_seg_end, y = map_point_y - 1.5),
             shape = 21, size = 2, col = "#ffbe7f", fill = "#282828") +
  geom_text(data = filter(timeline_data, map_point != 9),
            aes(x = short_seg_end, y = map_point_y - 2.5, label = short_seg_end),
            col = "#ffbe7f", size = 10, family = "Roboto Condensed", hjust = 0) +
  scale_x_continuous(limits = c(1970, 2040)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#282828", colour = "#282828"),
        plot.background = element_rect(fill = "#282828", colour = "#282828"))

ggsave("../../Pictures/20240712_SELECTION/MAPS/06_TIMELINE.png", timeline_plot,
       width = 6, height = 6, scale = 1, dpi = 320)


timeline_plot <- ggplot(timeline) +
  geom_point(data = filter(timeline, map_point == 1),
             aes(x = start, y = 10),
             shape = 21, size = 4, color = "#ffbe7f", fill = "#282828") +
  geom_point(data = filter(timeline, map_point == 1),
             aes(x = end, y = 10),
             shape = 21, size = 4, color = "#ffbe7f", fill = "#282828") +
  geom_text(data = filter(timeline, map_point == 1),
            aes(x = start, y = 10.5, label = start),
            size = 10, family = "Roboto Condensed", color = "#ffbe7f") +
  geom_text(data = filter(timeline, map_point == 1),
            aes(x = end, y = 10.5, label = end),
            size = 10, family = "Roboto Condensed", color = "#ffbe7f") +
  geom_text(data = filter(timeline, map_point == 1),
            aes(x = start, y = 11.5, label = map_point),
            col = "#fff7ed",
            size = 10,
            family = "Roboto Condensed", hjust = 0.5, fontface = "bold") +
  geom_text(data = filter(timeline, map_point == 1),
            aes(x = start + 3, y = 11.5, label = address),
            col = "#fff7ed",
            size = 14,
            family = "Roboto Condensed", hjust = 0, fontface = "bold") +
  scale_y_continuous(limits = c(0, 15)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#282828", colour = "#282828"),
        plot.background = element_rect(fill = "#282828", colour = "#282828"))

ggsave("../../Pictures/20240712_SELECTION/MAPS/06_TIMELINE.png", timeline_plot,
       width = 6, height = 6, scale = 1, dpi = 320)

d <- tibble(
  x = 0,
  y = 1:50)

dates <- tibble(
  x = 0.5,
  y = c(50, 47, 44, 39, 32, 25, 14, 7, 4, 1),
  label = c(1974, 1979, 1981, 1986, 1993, 2000, 2011, 2018, 2021, 2024))


ggplot() +
  geom_segment(data = d,
               aes(x = x - 0.25, xend = x + 0.25,
                   y = y, yend = y,
                   colour = y),
               linewidth = 1, alpha = 0.4,
               show.legend = FALSE) +
  geom_text(data = dates,
            aes(x = x, y = y, label = label)) +
  scale_colour_gradient2(low = "#fff7ed", mid ="#ffbe7f", high = "#ed5309",
                         midpoint = 25) +
  # geom_rect(aes(xmin = 1, xmax = 10,
  #               ymin = -2, ymax = -1),
  #           color = "#ffbe7f", fill = "#282828") +
  # geom_rect(aes(xmin = 10, xmax = 14,
  #               ymin = -2, ymax = -1),
  #           color = "#ffbe7f", fill = "#282828") +
  # geom_rect(aes(xmin = 14, xmax = 24,
  #               ymin = -2, ymax = -1),
  #           color = "#ffbe7f", fill = "#282828") +
  # geom_rect(aes(xmin = 24, xmax = 38,
  #               ymin = -2, ymax = -1),
  #           color = "#ffbe7f", fill = "#282828") +
  # geom_rect(aes(xmin = 38, xmax = 42,
  #               ymin = -2, ymax = -1),
  #           color = "#ffbe7f", fill = "#282828") +
  # geom_rect(aes(xmin = 42, xmax = 54,
  #               ymin = -2, ymax = -1),
  #           color = "#ffbe7f", fill = "#282828") +
  # geom_rect(aes(xmin = 54, xmax = 68,
  #               ymin = -2, ymax = -1),
  #           color = "#ffbe7f", fill = "#282828") +
  # geom_rect(aes(xmin = 68, xmax = 74,
  #               ymin = -2, ymax = -1),
  #           color = "#ffbe7f", fill = "#282828") +
  # geom_rect(aes(xmin = 74, xmax = 120,
  #               ymin = -2, ymax = -1),
  #           color = "#ffbe7f", fill = "#282828") +
  scale_x_continuous(limits = c(-4, 4)) +
  theme_void() +
  theme(panel.background = element_rect(fill = "#282828", colour = "#282828"),
        plot.background = element_rect(fill = "#282828", colour = "#282828"))

# 7. GRENOBLE -------------------------------------------------------------

grenoble <- create_map(coords = coords, place = "Grenoble",
                      # min_lon = -4.479, max_lon = -4.390, min_lat = 55.815, max_lat = 55.865,
                      line_col = "#ffbe7f", bg_fill = "#282828", bg_col = "#282828", border_col = "#fff7ed",
                      title_col = "#ffbe7f", title_size = 75) +
  geom_point(aes(x = 5.71546, y = 45.17834),
             col = "#ed5309",
             size = 4)
  # geom_text(aes(x = 5.71546, y = 45.17834),
  #           col = "#fff7ed",
  #           label = "1", size = 10,
  #           family = "Roboto Condensed", hjust = 0.5, fontface = "bold")

ggsave("C:/Users/jkitt/Pictures//20240712_SELECTION/MAPS/07_GRENOBLE.png", grenoble,
       width = 6, height = 6, scale = 1, dpi = 320)

# 8. GENEVE ---------------------------------------------------------------

geneve <- create_map(coords = coords, place = "Genève",
                       # min_lon = -4.479, max_lon = -4.390, min_lat = 55.815, max_lat = 55.865,
                       line_col = "#ffbe7f", bg_fill = "#282828", bg_col = "#282828", border_col = "#fff7ed",
                       title_col = "#ffbe7f", title_size = 75) +
  geom_point(aes(x = 6.19651, y = 46.22778),
             col = "#ed5309",
             size = 4)
  # geom_text(aes(x = 6.19651, y = 46.22778),
  #           col = "#fff7ed",
  #           label = "2", size = 10,
  #           family = "Roboto Condensed", hjust = 0.5, fontface = "bold")

ggsave("C:/Users/jkitt/Pictures//20240712_SELECTION/MAPS/08_GENEVE.png", geneve,
       width = 6, height = 6, scale = 1, dpi = 320)

# 9. LILLE ----------------------------------------------------------------

lille <- create_map(coords = coords, place = "Lille",
                     # min_lon = -4.479, max_lon = -4.390, min_lat = 55.815, max_lat = 55.865,
                     line_col = "#ffbe7f", bg_fill = "#282828", bg_col = "#282828", border_col = "#fff7ed",
                     title_col = "#ffbe7f", title_size = 75) +
  geom_point(aes(x = 3.06527, y = 50.63495),
             col = "#ed5309",
             size = 4)
  # geom_text(aes(x = 3.06527, y = 50.63495),
  #           col = "#fff7ed",
  #           label = "3", size = 10,
  #           family = "Roboto Condensed", hjust = 0.5, fontface = "bold")

ggsave("C:/Users/jkitt/Pictures//20240712_SELECTION/MAPS/09_LILLE.png", lille,
       width = 6, height = 6, scale = 1, dpi = 320)

# 10. BESANCON ------------------------------------------------------------

besancon <- create_map(coords = coords, place = "Besançon",
                    # min_lon = -4.479, max_lon = -4.390, min_lat = 55.815, max_lat = 55.865,
                    line_col = "#ffbe7f", bg_fill = "#282828", bg_col = "#282828", border_col = "#fff7ed",
                    title_col = "#ffbe7f", title_size = 75) +
  geom_point(aes(x = 6.00385, y = 47.25539),
             col = "#ed5309",
             size = 4)
  # geom_text(aes(x = 6.00385, y = 47.25539),
  #           col = "#fff7ed",
  #           label = "4", size = 10,
  #           family = "Roboto Condensed", hjust = 0.5, fontface = "bold")

ggsave("C:/Users/jkitt/Pictures//20240712_SELECTION/MAPS/10_BESANCON.png", besancon,
       width = 6, height = 6, scale = 1, dpi = 320)
