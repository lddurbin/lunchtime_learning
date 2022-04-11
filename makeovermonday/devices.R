library(dplyr)
library(ggplot2)

data <- readr::read_csv("makeovermonday/purchase_intent.csv") %>% 
  tidyr::pivot_longer(cols = 2:3, values_to = "purchase_intent", names_to = "year")

ggplot(data = data) +
  geom_point(aes(x = category, y = purchase_intent)) +
  coord_flip()
