library(readr)
library(tsibble)
library(dplyr)

# Load data
CPIAUCSL <- read_csv("datasets/CPIAUCSL.csv") |>
  mutate(observation_date = yearmonth(observation_date)) |>
  as_tsibble(index = observation_date) #|>
#  filter_index("1960 Jan" ~ "1999 Dec")
  
TB3MS <- read_csv("datasets/TB3MS.csv") |>
  mutate(observation_date = yearmonth(observation_date)) |>
  as_tsibble(index = observation_date) #|>
  #filter_index("1960 Jan" ~ "1999 Dec")

UNRATE <- read_csv("datasets/UNRATE.csv") |>
  mutate(observation_date = yearmonth(observation_date)) |>
  as_tsibble(index = observation_date) #|>
  #filter_index("1960 Jan" ~ "1999 Dec")

# Verify data quality
for (dataset in list(CPIAUCSL, TB3MS, UNRATE)) {
  print(nrow(dataset)) # 480 for all datasets
  if (has_gaps(dataset) | is_duplicated(dataset)) {
    print("Abnormality detected!")
  }
}

library(lubridate)

CPI.Q <- CPIAUCSL |>
  filter(month(observation_date) %in% c(3, 6, 9, 12)) |>
  mutate(Quarter = yearquarter(observation_date)) |>
  as_tsibble(index = Quarter) |>
  filter_index("1959 Q4" ~ "1999 Q4")

inflation <- CPI.Q |>
  mutate(Inflation = 400 * log(CPIAUCSL / lag(CPIAUCSL))) |>
  filter_index("1960 Q1" ~ .) |> 
  rename(CPI = CPIAUCSL)


# 3.5

library(feasts)
library(tidyr)

inflation |>
  pivot_longer(cols = c(CPI, Inflation),
               names_to = "series",
               values_to = "value") |>
  as_tsibble(index = Quarter, key = series) |>
  autoplot(value) +
  facet_wrap(~ series, ncol = 1, scales = "free_y") +
  labs(y = NULL) +
  theme(legend.position = "none")


library(slider)

inflation <- inflation |>
  mutate(
    roll.mean = slide_dbl(Inflation, mean, .before = 19, .complete = TRUE),
    roll.sd = slide_dbl(Inflation, sd, .before = 19, .complete = TRUE)
  )

inflation |>
  pivot_longer(cols = c(Inflation, roll.mean, roll.sd),
               names_to = "series",
               values_to = "value") |>
  as_tsibble(index = Quarter, key = series) |>
  autoplot(value) +
  facet_wrap(~ series, ncol = 1, scales = "free_y",
             labeller = as_labeller(c(
               "Inflation" = "Inflation",
               "roll.mean" = "20-Quarter Rolling Mean",
               "roll.sd" = "20-Quarter Rolling SD"
             ))) +
  labs(y = NULL) +
  theme(legend.position = "none")


inflation |> ACF(Inflation) |> autoplot() +
  labs(title = "ACF plot of inflation")

inflation |> PACF(Inflation) |> autoplot() +
  labs(title = "PACF plot of inflation")


UNRATE.Q <- UNRATE |>
  filter(month(observation_date) %in% c(3, 6, 9, 12)) |>
  mutate(Quarter = yearquarter(observation_date)) |>
  as_tsibble(index = Quarter) |>
  filter_index("1960 Q1" ~ "1999 Q4")

TB3MS.Q <- TB3MS |>
  filter(month(observation_date) %in% c(3, 6, 9, 12)) |>
  mutate(Quarter = yearquarter(observation_date)) |>
  as_tsibble(index = Quarter) |>
  filter_index("1960 Q1" ~ "1999 Q4")

inflation$Unemployment <- UNRATE.Q$UNRATE
inflation$Treasury <- TB3MS.Q$TB3MS

inflation |>
  pivot_longer(cols = c(Inflation, Unemployment, Treasury),
               names_to = "series",
               values_to = "value") |>
  as_tsibble(index = Quarter, key = series) |>
  autoplot(value) +
  facet_wrap(~ series, ncol = 1, scales = "free_y",
             labeller = as_labeller(c(
               "Inflation" = "Inflation",
               "Unemployment" = "Unemployment rate",
               "Treasury"   = "Treasury bill rate"
             ))) +
  labs(y = NULL) +
  theme(legend.position = "none")


library(ggplot2)

inflation |>
  ggplot(aes(x = Unemployment, y = Inflation)) +
  geom_point() +
  labs(x = "Unemployment rate", y = "Inflation")

cor(inflation$Inflation, inflation$Unemployment)


# 3.6

library(changepoint)

cpt_result <- cpt.mean(inflation$Inflation, method = "PELT", penalty = "BIC")
cpt_locations <- cpts(cpt_result)
cpt_dates <- inflation$Quarter[cpt_locations]

segment_means <- param.est(cpt_result)$mean
seg_start <- c(1, cpt_locations + 1)[1:length(segment_means)]
seg_end   <- c(cpt_locations, nrow(inflation))

segments_tbl <- tibble(
  Segment = seq_along(segment_means),
  Start = inflation$Quarter[seg_start],
  End = inflation$Quarter[seg_end],
  Mean = segment_means
)
