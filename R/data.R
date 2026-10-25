library(readr)
library(dplyr)
library(lubridate)

read_quarterly <- function(series, name) {
  read_csv(paste0("datasets/", series, ".csv"), show_col_types = FALSE) |>
    mutate(observation_date = tsibble::yearmonth(observation_date)) |>
    tsibble::as_tsibble(index = observation_date) |>
    filter(month(observation_date) %in% c(3, 6, 9, 12)) |>
    as_tibble() |>
    transmute(date = as.Date(observation_date), !!name := .data[[series]])
}

quarterly_data <- read_quarterly("CPIAUCSL", "CPI") |>
  inner_join(read_quarterly("UNRATE", "Unemployment"), by = "date") |>
  inner_join(read_quarterly("TB3MS",  "Treasury"),     by = "date") |>
  arrange(date) |>
  mutate(Inflation = 400 * log(CPI / lag(CPI)),
         Quarter   = paste0(year(date), "Q", quarter(date))) |>
  filter(date >= as.Date("1960-03-01")) |>
  select(date, Quarter, CPI, Inflation, Unemployment, Treasury)

initial_data <- quarterly_data |> filter(date <= as.Date("1999-12-01"))