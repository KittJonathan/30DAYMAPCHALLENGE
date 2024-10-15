# 30 DAY MAP CHALLENGE
# 2024
# 01 - POINTS

# 📦 LOAD PACKAGES --------------------------------------------------------

pacman::p_load(tidyverse, readxl, showtext)

# ⬇️ IMPORT DATASET -------------------------------------------------------

dt <- read_xlsx("2024/00-DATA/DoBIH_v18_1.xlsx")

# 🧹 CLEAN DATASET --------------------------------------------------------

dt <- dt |> 
  filter(Ycoord < 4e6) |> 
  select(x = Xcoord, y = Ycoord, alt = Metres)

# 📊 CREATE MAP -----------------------------------------------------------

p <- dt |> 
  ggplot(aes(x, y, col = alt)) +
  geom_point(size = 0.2)

# 💾 SAVE MAP -------------------------------------------------------------

ggsave(filename = "2024/02-MAPS/01_POINTS.png", plot = p, 
       dpi = 320, width = 6, height = 6)

