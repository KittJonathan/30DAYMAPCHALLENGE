# 30 DAY MAP CHALLENGE
# 2024
# 01 - POINTS

# 📦 LOAD PACKAGES --------------------------------------------------------

pacman::p_load(tidyverse, readxl, showtext)

# ⬇️ IMPORT DATASET -------------------------------------------------------

dt <- read_xlsx("2024/00-DATA/DoBIH_v18_1.xlsx")

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

