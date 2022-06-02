library(dplyr) # A Grammar of Data Manipulation
library(lubridate) # Make Dealing with Dates a Little Easier
library(tidyr) # Tidy Messy Data
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(purrr) # Functional Programming Tools
library(stringr) # Simple, Consistent Wrappers for Common String Operations
library(readr) # Read Rectangular Text Data

path <- here::here("makeovermonday/2022/Week06")

curves <- function(x_start, x_end, y_start, y_end, curve) {
  annotate(
    geom = "curve", x = x_start, y = y_start, xend = x_end, yend = y_end, 
    curvature = curve, arrow = arrow(length = unit(2, "mm")), size = 1
  )
}

labels <- function(x, y, label) {
  annotate(geom = "text", x = x, y = y, label = label, hjust = "left", size = 3.5, fontface = "bold")
}

data <- read_csv(paste0(path, "/oldestpeople_detailed.csv"))

calment <- data |> filter(name == "Jeanne Calment")

# left_join(data, read_csv(paste0(path, "/names_with_urls.csv"))) |> write_csv(file = paste0(path, "/oldestpeople_detailed.csv"))

plot <- data |>
  ggplot(aes(x = died, y = age_years)) +
  geom_smooth(size = 2, colour = "orange2") +
  geom_point(size = 3.75, colour = "white") +
  geom_point(size = 3, colour = "blue") +
  scale_y_continuous(limits = c(108, 124), breaks = seq(108, 124, by = 4)) +
  pmap(
    list(
      calment |> pull(died) + months(6),
      calment |> pull(died),
      calment |> pull(age_years) + 1.4,
      calment |> pull(age_years) + 0.2,
      .3
      ),
    curves
  ) +
  pmap(
    list(calment |> pull(died) - years(4), calment |> pull(age_years) + 2, "Jeanne Calment was the\noldest person who ever lived"),
    labels
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(size = 11),
    plot.title.position = "plot",
    plot.subtitle = element_text(size=12),
    title = element_text(size = 18),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  labs(
    title = "The oldest people are getting older",
    subtitle = "Age at date of death of each person who held the record for oldest person alive since records began "
  )

ggsave(paste0(path, "/oldest_people_makeover.png"), plot = plot, device = "png", bg = "#f8f8ff", width = 10, height = 7)
