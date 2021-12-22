library(dplyr)
library(ggplot2)

tuesdata <- tidytuesdayR::tt_load('2021-12-21')

starbucks <- tuesdata$starbucks

non_milk_options <- starbucks %>% 
  filter(whip == 0 & size == "short" & milk == 0) %>% 
  distinct(product_name, calories) %>% 
  mutate(product_group = case_when(
    calories == 0 ~ "Tea",
    stringr::str_detect(product_name, "Clover Brewed") ~ "Clover Brewed Coffees",
    calories < 80 ~ "Other coffees",
    TRUE ~ product_name
  ))
  
non_cal_drinks <- non_milk_options %>% 
  ggplot(aes(x = reorder(product_group, calories), y = calories)) +
  geom_col(fill = "#00704A") +
  coord_flip() +
  ggthemes::theme_fivethirtyeight() +
  theme(
    axis.text.y = element_text(size = 12),
    plot.title.position = "plot",
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white")
  ) +
  labs(
    title = "If you can't drink milk and you're watching your calories,\ndon't order the Caramel Apple Spice at Starbucks",
    caption = "Calories (kCal) for drinks at Starbucks with a no-milk option | Source: TidyTuesday (Week 51)"
  )

starbucks %>% 
  filter(milk %in% c(3,4) & whip == 0 & size == "short") %>% 
  mutate(milk = ifelse(milk == 3, "Soy", "Coconut")) %>% 
  ggplot(aes(x = product_name, y = saturated_fat_g)) +
  geom_bar(aes(fill = milk), stat = "identity", position = "dodge") +
  coord_flip()
