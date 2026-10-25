# Expanding-window forecast experiment
#
# Model interface: every model is a function f(data, H) that returns ONE
# number, the forecast of inflation in quarter T + H made at origin T.
#   data : the team's quarterly tsibble (R/data.R), indexed by Quarter,
#          rows 1960 Q1 through the origin T only (columns include Inflation)
#   H    : forecast horizon, an integer (the framework uses 1, 4 and 8)
# Everything the model needs (lag selection, coefficients, local mean, gap)
# is computed inside f from `data`.
# Usage:
#   source("R/forecast-framework.R")
#   fc <- run_expanding_window(quarterly_data, list("Random walk" = fc_rw))
#   ev <- attach_actuals(fc, quarterly_data)

library(dplyr)

# Returns one row per model, origin and horizon (origin, target, h, model,
# forecast) for every target from the first origin through last_target:
# 80, 77 and 73 forecasts per model at h = 1, 4 and 8.
run_expanding_window <- function(data, models,
                                 first_origin = "1999 Q4",
                                 last_target  = "2019 Q4",
                                 horizons     = c(1, 4, 8)) {
  horizons <- as.integer(horizons)
  quarters <- data[[tsibble::index_var(data)]]   # sorted, one row per quarter
  first <- which(quarters == tsibble::yearquarter(first_origin))
  last  <- which(quarters == tsibble::yearquarter(last_target))
  
  out <- list()
  for (i in first:(last - min(horizons))) {    # i is the row of origin T
    data_T <- data[seq_len(i), ]               # 1960 Q1 through T, nothing later
    for (H in horizons[i + horizons <= last]) {
      for (m in names(models)) {
        fc <- models[[m]](data_T, H)
        if (length(fc) != 1 || !is.finite(fc)) {
          stop(sprintf("'%s' at origin %s, H = %d did not return one finite number.",
                       m, format(quarters[i]), H), call. = FALSE)
        }
        out[[length(out) + 1]] <- tibble(origin   = quarters[i],
                                         target   = quarters[i + H],
                                         h        = H,
                                         model    = m,
                                         forecast = as.numeric(fc))
      }
    }
  }
  bind_rows(out) |> arrange(match(model, names(models)), h, origin)
}

# Attaches realized inflation for each target quarter and the forecast error
# e = actual - forecast (positive = the model underpredicted). Run this only
# after the forecasts are made; actuals never enter their construction.
attach_actuals <- function(forecasts, data) {
  realized <- tibble(target = data[[tsibble::index_var(data)]],
                     actual = data$Inflation)
  forecasts |>
    left_join(realized, by = "target") |>
    mutate(error = actual - forecast)
}