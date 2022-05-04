library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(readxl) # Read Excel Files
library(ggtext)
library(purrr)

path <- here::here("makeovermonday/2022/Week04")

clean_data <- read_excel(fs::dir_ls(path, glob = "*.xlsx"), range = "J5:R24", .name_repair = janitor::make_clean_names) |>  
  rename(region = x, null_rate = null_2) |> 
  mutate(
    warehouse_id = as.character(warehouse_id),
    national_avg = sum(errors)/n(),
    region = factor(region, levels = c("Northeast", "West", "South", "Midwest")),
    border_colour = if_else(region %in% c("Northeast", "Midwest"), "black", "white"),
    region_num = case_when(
      region == "Midwest" ~ 1,
      region == "West" ~ 2,
      region == "South" ~ 3,
      region == "Midwest" ~ 4
    )
    )

ggplot(data = clean_data) +
  geom_point(aes(x = forcats::fct_rev(region), y = errors), colour = "black", pch = 21, size = 6.5) +
  geom_point(aes(x = forcats::fct_rev(region), y = errors, fill = region), pch = 21, size = 6) +
  scale_fill_manual(values = c("#ea9d18", "#87CEEB", "yellow", "grey")) +
  coord_flip() +
  annotate(geom = "segment", x = 0.8, xend = 4.2, y = 50.6, yend = 50.6, linetype = "dashed") +
  annotate(geom = "text", x = 4.3, y = 50.6, label = "National average", size = 5) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.x = element_text(size=12),
    axis.text.y = element_text(size=15),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title.position = "plot",
    plot.title = element_markdown(lineheight = 1.2, size = 21)
  ) +
  labs(
    title = "Most of Q4's returned products originated from our warehouses in the<br><strong><span style='color:#ea9d18'>Northeast</span></strong>, but we may have a problem out <strong><span style='color:#87CEEB'>West</span></strong> too",
    caption = "Number of errors recorded per warehouse via ProTip dashboard in Q4 2021"
  )
