library(dplyr)

# read.csv("5000 Records.csv")

raw_data <- readr::read_csv(file = "5000 Records.csv", name_repair = janitor::make_clean_names)

male_employees <- filter(raw_data, gender == "M")

female_employees <- filter(raw_data, gender == "F")

# Joined prior to 2010, not male, middle initial is not either A, B, C, or D.
# Sort by middle initial (ascending order) and last name (descending order)
filtered_and_arranged <- filter(raw_data, year_of_joining < 2010 & gender != "M" & !middle_initial %in% c("A", "B", "C", "D")) %>%
  arrange(middle_initial, desc(last_name))
