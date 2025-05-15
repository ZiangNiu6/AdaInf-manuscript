# This is a Rscript exploring the data analysis
library(readr)
library(dplyr)
baseline_data <- read_csv("realdata-code/raw-data/baseline.csv")
outcome_data <- read_csv("realdata-code/raw-data/outcomes.csv")

# add the age_group based on AGE and rename the variables
combined_data <- baseline_data |>
  merge(outcome_data, by = "MASKID") |>
  mutate(age_group = cut(AGE, 
                         breaks = 3, 
                         labels = c("Middle-aged", "Senior", "Most-Senior")),
         outcome = EVENT_PRIMARY, treatment = INTENSIVE + 1) |>
  dplyr::select(outcome, treatment, age_group, AGE)
  

# save the processed data
path_to_save <- "realdata-code/intermediate-data"
if (!dir.exists(path_to_save)) {
  dir.create(path_to_save)
  cat("Directory created:", path_to_save, "\n")
} else {
  cat("Directory already exists:", path_to_save, "\n")
}
saveRDS(combined_data, file = sprintf("%s/preprocessed_data.rds", path_to_save))
