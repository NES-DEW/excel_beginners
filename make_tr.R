# make_tr.R
library(tidyverse)

source("R/nice_date.R")
source("R/training_sessions.R")

forth <- read_csv("https://raw.githubusercontent.com/NES-DEW/KIND-training/main/data/training_schedule.csv") |>
  filter(start > now())

dates <- read_csv("https://raw.githubusercontent.com/NES-DEW/KIND-training/main/data/KIND_training_sessions.csv") |>
  filter(Level == "beginner", `Platform / area` == "Excel") |>
  inner_join(forth, join_by(Title == `session title`)) |>
  arrange(start) |>
  mutate(start = date(start)) |>
  pull(start) |>
  as.character()

files <- c("excel_first_steps.qmd", "excel_tidy_data.qmd", "excel_references_and_names.qmd", "excel_formatting.qmd", "excel_tables.qmd", "excel_formulas.qmd")

renda <- function(sesh, sesh_date){
  quarto::quarto_render(sesh, execute_params = list(date = sesh_date))
}

walk2(files, dates, renda)

