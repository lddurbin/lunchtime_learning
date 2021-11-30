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

pay_rise_bands %>% 
  count(salary_band)

pay_rise_bands %>% 
  mutate(pay_rise_1_pct = round(salary*1.01, 0)) %>% 
  group_by(salary_band) %>% 
  summarise(investment_total = sum(pay_rise_1_pct), avg_old_salary = round(mean(salary), 0), avg_new_salary = round(mean(pay_rise_1_pct), 0))

pay_rise_bands %>% 
  mutate(low_pay_shortfall = case_when(
    salary_band == "Less than $50,000" ~ (50000-salary)/salary,
    TRUE ~ 1
  )) %>% 
  mutate(new_pay = case_when(
    salary_band == "Less than $50,000" ~ salary*(1+low_pay_shortfall),
    salary_band == "$50,000 to $74,999" ~ round(salary*1.025, 0),
    salary_band == "$75,000 to $99,999" ~ round(salary*1.01, 0)
  )) %>% 
  group_by(salary_band) %>% 
  summarise(investment_total = sum(new_pay), avg_increase = round(mean(new_pay-salary), 0))
