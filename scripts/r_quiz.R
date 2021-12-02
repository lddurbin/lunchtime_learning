library(dplyr)
library(tidyr)

answers <- readr::read_csv("data/quiz_answers.csv", col_types = "cc") %>% 
  mutate(comparison = paste(question, correct_answer)) %>% 
  pull(comparison)

files <- fs::dir_ls("data", glob = "*.xlsx")

quiz_data <- files %>% 
  purrr::map_dfr(readxl::read_excel, col_names = c("id", "start_time", "completion_time", "email", "name", "answer"))

clean_quiz_data <- quiz_data %>%
  mutate(question = if_else(id == "ID", answer, NA_character_), answer = stringr::str_replace(answer, ";$", "")) %>% 
  fill(question) %>% 
  filter(email != "smithem1@aklc.govt.nz" & id != "ID") %>% 
  separate_rows(answer, sep = ";")

clean_quiz_data %>% 
  mutate(outcome = if_else(paste(question, answer) %in% answers, TRUE, FALSE)) %>% 
  mutate(score = case_when(
    answer %in% c("TRUE", "FALSE") & outcome ~ 1,
    outcome ~ 2,
    TRUE ~ 0
  )) %>% 
  with_groups(name, summarise, final_score = sum(score))

