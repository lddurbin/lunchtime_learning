library(ggplot2)
library(ggthemes)

# Create a scatterplot
scatterplot <- ggplot(mtcars, aes(x = mpg, y = hp)) +
  geom_smooth() +
  geom_point(alpha = 0.5) +
  labs(
    title = "Cars with more miles per gallon have less horsepower",
    caption = "Data: 1974 Motor Trend US magazine"
    ) +
  facet_wrap(~gear) +
  theme_wsj()

ggsave("scatterplot.png", scatterplot)

barplot <- ggplot(diamonds, aes(x = clarity)) +
  geom_bar()

ggsave("barplot.png", barplot)
