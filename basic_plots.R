library(ggplot2)

ggplot(mtcars) +
  geom_smooth(aes(x = mpg, y = hp)) +
  geom_point(aes(x = mpg, y = hp), alpha = 0.5) +
  labs(
    title = "Cars with more miles per gallon have less horsepower",
    caption = "Data: 1974 Motor Trend US magazine"
    )
