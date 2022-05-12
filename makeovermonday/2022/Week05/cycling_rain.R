library(dplyr) # A Grammar of Data Manipulation
library(tidyr) # Tidy Messy Data
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics

path <- here::here("makeovermonday/2022/Week05")

df <- readr::read_csv(fs::dir_ls(path, glob = "*.csv")) |> 
  mutate(num = row_number())

plot <- ggplot(df, aes(x = num)) +
  stat_smooth(aes(y = annual_precipitation/10000), geom = 'area', fill = "#71ABFF") +
  geom_rect(aes(xmin = num-.02, xmax = num+.02, ymin = 0, ymax = cycling_share), fill = "#73716e") +
  geom_point(aes(y = cycling_share), size = 14, colour = "orange") +
  purrr::pmap(
    list(
      c(1:5),
      c(df$cycling_share)
    ),
    ~annotate(geom = "text", x = ..1, y = ..2, label = scales::percent(..2), colour = "black", size = 4.5)
  ) +
  scale_x_continuous(labels = df$city) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text.x = element_text(size=14),
    axis.text.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 22),
    plot.background = element_rect(colour = "white")
  ) +
  labs(
    title = "Annual rainfall doesn't seem to influence bicycle usage in these cities",
    subtitle = "% of trips that are cycling, plotted against annual rainfall"
  )

ggsave(paste0(path, "/cycling_underwater.png"), plot = plot, device = "png")