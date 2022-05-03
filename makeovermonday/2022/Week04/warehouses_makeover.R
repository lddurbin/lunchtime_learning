library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(readxl) # Read Excel Files

path <- here::here("makeovermonday/2022/Week04")

clean_data <- read_excel(fs::dir_ls(path, glob = "*.xlsx"), range = "J5:R24", .name_repair = janitor::make_clean_names) |>  
  rename(region = x, null_rate = null_2) |> 
  mutate(warehouse_id = as.character(warehouse_id))

northeast_or_not <- clean_data |>
  mutate(error_prone_northeast = if_else(warehouse_id %in% c("2", "3", "4", "5", "6"), TRUE, FALSE)) |>
  with_groups(error_prone_northeast, summarise, total_error = sum(errors))

avg_regional_errors <- clean_data |>
  group_by(region) |>
  summarise(total_errors = sum(errors), sites = n(), avg_errors = mean(errors)) |> 
  mutate(regional_avg = sum(total_errors)/sum(sites))

ggplot(data = clean_data) +
  geom_point(aes(x = region, y = errors, colour = region), size = 5) +
  scale_colour_manual(values = c("grey", "orange", "blue", "black")) +
  coord_flip() +
  geom_hline(yintercept = 50.6, linetype = "dashed") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_text(size=14),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(size=18)
  ) +
  labs(
    title = "Most of the errors are coming out our warehouses in the Northeast, but we may have a problem out West too"
  )