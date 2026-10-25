library(readr)
library(dplyr)
library(lubridate)

read_quarterly <- function(series, name) {
  read_csv(paste0("datasets/", series, ".csv"), show_col_types = FALSE) |>
    mutate(observation_date = tsibble::yearmonth(observation_date)) |>
    tsibble::as_tsibble(index = observation_date) |>
    filter(month(observation_date) %in% c(3, 6, 9, 12)) |>
    mutate(Quarter = tsibble::yearquarter(observation_date)) |>
    tsibble::as_tsibble(index = Quarter) |>
    transmute(date = as.Date(observation_date), !!name := .data[[series]])
}

quarterly_data <- read_quarterly("CPIAUCSL", "CPI") |>
  inner_join(read_quarterly("UNRATE", "Unemployment"), by = c("Quarter", "date")) |>
  inner_join(read_quarterly("TB3MS",  "Treasury"),     by = c("Quarter", "date")) |>
  mutate(Inflation = 400 * log(CPI / lag(CPI))) |>
  tsibble::filter_index("1960 Q1" ~ .) |>
  select(Quarter, date, CPI, Inflation, Unemployment, Treasury)

initial_data <- quarterly_data |> tsibble::filter_index(~ "1999 Q4")