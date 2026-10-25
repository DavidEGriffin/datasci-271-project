# Expanding-window forecast experiment (project guide, Step 2).
#
# Model interface: every model is a function f(data, H) that returns ONE
# number, the forecast of inflation in quarter T + H made at origin T.
#   data : the team's quarterly panel, rows 1960Q1 through the origin T only,
#          ordered by quarter (columns include Quarter and Inflation)
#   H    : forecast horizon, an integer (the framework uses 1, 4 and 8)
# Everything the model needs (lag selection, coefficients, local mean, gap)
# is computed inside f from `data`.
#
# Usage:
#   source("R/forecast-framework.R")
#   fc <- run_expanding_window(ar_quarterly, list("Random walk" = fc_rw))
#   ev <- attach_actuals(fc, ar_quarterly)

library(dplyr)

# Returns one row per model, origin and horizon (origin, target, h, model,
# forecast) for every target from the first origin through last_target:
# 80, 77 and 73 forecasts per model at h = 1, 4 and 8.
run_expanding_window <- function(data, models,
                                 first_origin = "1999Q4",
                                 last_target  = "2019Q4",
                                 horizons     = c(1, 4, 8)) {
  horizons <- as.integer(horizons)
  first <- match(first_origin, data$Quarter)   # row of the first origin
  last  <- match(last_target,  data$Quarter)   # row of the last target
  
  out <- list()
  for (i in first:(last - min(horizons))) {    # i is the row of origin T
    data_T <- data[seq_len(i), ]               # 1960Q1 through T, nothing later
    for (H in horizons[i + horizons <= last]) {
      for (m in names(models)) {
        fc <- models[[m]](data_T, H)
        if (length(fc) != 1 || !is.finite(fc)) {
          stop(sprintf("'%s' at origin %s, H = %d did not return one finite number.",
                       m, data$Quarter[i], H), call. = FALSE)
        }
        out[[length(out) + 1]] <- tibble(origin   = data$Quarter[i],
                                         target   = data$Quarter[i + H],
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
  forecasts |>
    left_join(data |> select(target = Quarter, actual = Inflation),
              by = "target") |>
    mutate(error = actual - forecast)
}