# 30 DAY MAP CHALLENGE
# 2024
# 24 - ONLY CIRCULAR SHAPES

# 📦 LOAD PACKAGES --------------------------------------------------------

pacman::p_load(tidyverse, osmdata, showtext, patchwork)

# ⬇️ IMPORT DATASET -------------------------------------------------------

france <- map_data("france") |> 
  mutate(fill_col = case_when(region %in% c("Allier", "Cantal", "Haute-Loire", "Puy-de-Dome") ~ "Auvergne",
                              .default = "Other"))

auvergne <- france |> 
  filter(region %in% c("Allier", "Cantal", "Haute-Loire", "Puy-de-Dome"))

puy_de_dome <- france |> 
  filter(region == "Puy-de-Dome")

lakes <- tibble(
  id = c(1, 2, 3, 4),
  name = c("Lac Chauvet", "Lac d'en Haut", "Lac Pavin", "Gour de Tazenat"),
  long = c(2.83084, 2.91671, 2.88792, 2.99095),
  lat = c(45.45981, 45.38745, 45.79571, 45.98054)
)

p1 <- ggplot() +
  geom_polygon(data = france, aes(long, lat, group = group, fill = fill_col),
               show.legend = FALSE)

p2 <- ggplot() +
  geom_polygon(data = puy_de_dome, aes(long, lat, group = group)) +
  geom_point(data = lakes, aes(x = long, y = lat),
             col = "red")

bbx_lac_chauvet <- rbind(x = c(2.82230, 2.83848), y = c(45.45509, 45.46388))
colnames(bbx_lac_chauvet) <- c("min", "max")

bbx_lac_en_haut <- rbind(x = c(2.91270, 2.92062), y = c(45.38469, 45.38967))
colnames(bbx_lac_en_haut) <- c("min", "max")

bbx_lac_pavin <- rbind(x = c(2.88074, 2.89734), y = c(45.49186, 45.49954))
colnames(bbx_lac_pavin) <- c("min", "max")

bbx_gour_de_tazenat <- rbind(x = c(2.98581, 2.99619), y = c(45.97718, 45.98365))
colnames(bbx_gour_de_tazenat) <- c("min", "max")

lac_chauvet <- bbx_lac_chauvet |> 
  opq() |> 
  add_osm_feature(key = "water",
                  value = "lake") |> 
  osmdata_sf()

lac_en_haut <- bbx_lac_en_haut |> 
  opq() |> 
  add_osm_feature(key = "water",
                  value = "lake") |> 
  osmdata_sf()

lac_pavin <- bbx_lac_pavin |> 
  opq() |> 
  add_osm_feature(key = "water",
                  value = "lake") |> 
  osmdata_sf()

gour_de_tazenat <- bbx_gour_de_tazenat |> 
  opq() |> 
  add_osm_feature(key = "water",
                  value = "lake") |> 
  osmdata_sf()

p <- (plot_spacer() + p1 + p2) | (p3 + p4 + p5) +
  plot_layout(ncol = 3, nrow = 2)

ggsave(filename = "2024/02-MAPS/24_ONLY_CIRCULAR_SHAPES.png", plot = p, 
       dpi = 320, width = 12, height = 6)


(lac_pavin + lac_chauvet + gour_de_tazenat)

p3 <- ggplot() +
  geom_sf(data = lac_chauvet$osm_polygons)

p4 <- ggplot() +
  geom_sf(data = lac_en_haut$osm_polygons)

p5 <- ggplot() +
  geom_sf(data = lac_pavin$osm_polygons)

p6 <- ggplot() +
  geom_sf(data = gour_de_tazenat$osm_polygons)

# 🧹 CLEAN DATASET --------------------------------------------------------

dt <- dt |> 
  filter(Ycoord < 4e6, Country == "S") |> 
  select(x = Xcoord, y = Ycoord, alt = Metres)

# 📊 CREATE MAP -----------------------------------------------------------

cols <- c("0-199" = "#003f5c",
          "200-399" = "#374c80",
          "400-599" = "#7a5195",
          "600-799" = "#bc5090",
          "800-999" = "#ef5675",
          "1000-1199" = "#ff764a",
          "1200-1399" = "#ffa600")

p <- dt |> 
  arrange(alt) |> 
  mutate(cat = case_when(alt < 200 ~ "0-199",
                         alt >= 200 & alt < 400 ~ "200-399",
                         alt >= 400 & alt < 600 ~ "400-599",
                         alt >= 600 & alt < 800 ~ "600-799",
                         alt >= 800 & alt < 1000 ~ "800-999",
                         alt >= 1000 & alt < 1200 ~ "1000-1199",
                         alt >= 1200 & alt < 1400 ~ "1200-1399"),
         cat = fct_inorder(cat)) |> 
  ggplot(aes(x, y, col = cat)) +
  geom_point(size = 0.2, shape = 16) +
  scale_color_manual(values = cols) +
  coord_fixed() +
  theme_void() +
  theme(panel.background = element_rect(fill = "#001017"))

# 💾 SAVE MAP -------------------------------------------------------------

ggsave(filename = "2024/02-MAPS/01_POINTS.png", plot = p, 
       dpi = 320, width = 6, height = 6)

