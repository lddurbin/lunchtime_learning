library(dplyr) # A Grammar of Data Manipulation
library(ggplot2) # Create Elegant Data Visualisations Using the Grammar of Graphics
library(ggtext) # Improved Text Rendering Support for 'ggplot2'
library(ggforce) # Accelerating 'ggplot2'
library(purrr) # Functional Programming Tools
library(magick) # Advanced Graphics and Image-Processing in R

data <- readr::read_csv("makeovermonday/purchase_intent.csv")

wide_data <- data %>% 
  mutate(
    difference = purchase_intent_2016 - purchase_intent_2015,
    colour = if_else(purchase_intent_2015 > purchase_intent_2016, "decrease", "increase")
    )

plot <- ggplot(data = wide_data) +
  geom_link(aes(
    x = purchase_intent_2015,
    xend = purchase_intent_2016,
    y = reorder(category, -difference),
    yend = reorder(category, -difference),
    size = stat(index),
    color = colour),
    lineend = "round") +
  map(
    c("increase", "decrease"),
    ~geom_point(data = filter(wide_data, colour == .x), aes(x = purchase_intent_2016, y = category), shape = 21, fill = "white", size = 5, stroke = 0)) +
  map2(
    c("decrease", "increase"), c(1.15, -0.08),
    ~geom_label(data = filter(wide_data, colour == ..1), aes(x = purchase_intent_2016, y = category, label = category), hjust = ..2, fill = "white", label.size = NA, size = 5)
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    axis.text.x = element_text(size = 11),
    axis.text.y = element_blank(),
    panel.grid.major.x = element_line(0.3, colour = "#DCDCDC"),
    plot.title.position = "plot",
    plot.title = element_text(size=20),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    plot.margin = margin(10, 10, 20, 10)
  ) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1), breaks = seq(0, .6, by = .1), limits = c(0,.6)) +
  scale_color_manual(values = c("increase" = "orange", "decrease" = "#ADD8E6")) +
  xlab("Physical checkouts, January 2018 vs January 2022") +
  labs(
    title = "Consumers were less interested in purchasing traditional tech in 2016 than they were in 2015",
    caption = "28,000 consumers surveyed in 28 countries | Source: Accenture / Business Insider"
  )

inset_plot <- ggplot(data = tibble(x = seq(1:10), y = seq(1:10))) +
  geom_link(aes(x = 1, xend = 2, y = 1.8, yend = 1.8, size = stat(index)), lineend = "round", colour = "#ADD8E6") +
  geom_point(aes(x = 2, y = 1.8), shape = 21, fill = "white", size = 4.5, stroke = 0) +
  scale_x_continuous(limits = c(0.7,2.3)) +
  scale_y_continuous(limits = c(1.4,3.3)) +
  pmap(list(c(1.5,1,2), c(3,2.3,2.3), c("Key", "2015 purchase\nintent rate", "2016 purchase\nintent rate"), c(7,3.5,3.5)), ~annotate(geom = "text", x = ..1, y = ..2, label = ..3, size = ..4)) +
  theme_void() +
  theme(
    legend.position = "none",
    plot.background = element_rect(size=1, fill = 'floralwhite', color = "black")
  )

ggsave(filename = "makeovermonday/comets.png", plot = plot, height = 8, width = 15, units = "in", dpi = 300)
ggsave(filename = "makeovermonday/comet_key.png",  inset_plot, w = 3, h = 1.5, units = "in", dpi = 200, type = "cairo")

inset <- image_read("makeovermonday/comet_key.png")
chart <- image_read("makeovermonday/comets.png")
image_composite(chart, inset, offset = "+2830+1250") %>% image_write("makeovermonday/comets_composite.png")
