# Stage 2 functions: random walk (4.4.1) and local mean / inflation gap (4.4.5)
#
# Shared interface for every forecasting model
#   fc_<model>(data, H = 8)
#   data : rows 1960Q1 through the forecast origin T, ordered by quarter,
#          with an `Inflation` column. Nothing after T may be included.
#   H    : maximum horizon
#   returns tibble(h = 1:H, forecast)
# The expanding-window loop (4.3) calls each fc_ function once per origin and
# keeps h = 1, 4, 8.

library(tibble)

# 4.4.1 Random walk without drift ------------------------------------------
# Every horizon gets the last observed inflation rate, pi_T.
fc_rw <- function(data, H = 8) {
  tibble(h = seq_len(H), forecast = data$Inflation[nrow(data)])
}

# 4.4.5 Local mean by simple exponential smoothing --------------------------
# tau_1960Q1 = pi_1960Q1, tau_t = (1 - alpha) * tau_{t-1} + alpha * pi_t,
# with alpha = 0.05 fixed by the project (not estimated).
local_mean <- function(pi, alpha = 0.05) {
  stopifnot(length(pi) > 0, !anyNA(pi))
  tau <- numeric(length(pi))
  tau[1] <- pi[1]
  for (t in seq_along(pi)[-1]) {
    tau[t] <- (1 - alpha) * tau[t - 1] + alpha * pi[t]
  }
  tau
}

# Adds `tau` (local mean) and `gap` (pi_t - tau_t) to a sample.
# Call it on the origin-T sample, which must start in 1960Q1 so tau is
# initialized at pi_1960Q1. The last row then holds tau_T and g_T.
#   Gap AR (4.4.6):          fit the AR to data$gap, forecast = tau_T + g-hat
#   Fixed persistence (4.4.7): forecast = tau_T + 0.46^h * g_T
add_local_mean <- function(data, alpha = 0.05) {
  data$tau <- local_mean(data$Inflation, alpha)
  data$gap <- data$Inflation - data$tau
  data
}
