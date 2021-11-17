# read.csv("5000 Records.csv")

raw_data <- readr::read_csv(file = "5000 Records.csv", name_repair = janitor::make_clean_names)

