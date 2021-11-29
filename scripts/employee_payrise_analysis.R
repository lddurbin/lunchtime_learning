library(dplyr)

employee_data <- readr::read_csv("data/5000 Records.csv", name_repair = janitor::make_clean_names)

pay_rise <- employee_data %>% 
  filter(age_in_company_years > 10 & salary < 100000) %>% 
  mutate(full_name = paste0(first_name, " ", middle_initial, " ", last_name)) %>% 
  select(emp_id, e_mail, full_name, salary)

pay_rise_bands <- pay_rise %>% 
  mutate(salary_band = case_when(
    salary < 50000 ~ "Less than $50,000",
    salary < 75000 ~ "$50,000 to $74,999",
    salary < 100000 ~ "$75,000 to $99,999"
  ))
