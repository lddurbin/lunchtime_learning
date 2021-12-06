library(dplyr)
library(ggplot2)
library(lubridate)

# tuesdata <- tidytuesdayR::tt_load('2021-09-14')

billboard <- tuesdata$billboard
audio_features <- tuesdata$audio_features

join <- left_join(x = billboard, y = audio_features, by = c("song_id"))

join %>% filter(week_position == 1) %>% 
  select(week_id, loudness) %>% 
  mutate(week_id = mdy(week_id)) %>% 
  filter(!is.na(loudness)) %>% 
  ggplot(aes(x = week_id, y = loudness)) +
  geom_point() +
  geom_smooth(method = "lm") +
  ggthemes::theme_fivethirtyeight() +
  theme(
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 12)
    ) +
  labs(
    title = "The hits are getting louder!",
    caption = "Number 1 hits in the Billboard Top 100 by overall loudness of track in decibels"
    )
