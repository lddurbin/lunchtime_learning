library(ggplot2)

# Create a scatterplot
ggplot(mtcars, aes(x = mpg, y = hp)) +
  geom_smooth() +
  geom_point(alpha = 0.5) +
  labs(
    title = "Cars with more miles per gallon have less horsepower",
    caption = "Data: 1974 Motor Trend US magazine"
    ) +
  facet_wrap(~gear)