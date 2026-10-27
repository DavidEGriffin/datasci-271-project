# Direct AR (4.4.4) and direct Phillips curve (4.4.8)
#
# fc_<model>(data, H) takes the quarterly data through origin T and returns
# one inflation forecast for T + H (see R/forecast-framework.R)
#
# Direct models fit one equation per horizon instead of iterating [W4S1P24]

# Fit on rows with t + h <= T, same rows for every lag length so BIC is
# comparable [W6S2P12]
# Returns the forecast, chosen p, and lambda when u is given
direct_fit <- function(pi, h, u = NULL, max_p = 4) {
  n_T  <- length(pi)
  rows <- max_p:(n_T - h)          # response observed
  y    <- pi[rows + h]             # pi_{t+h}

  fits <- lapply(seq_len(max_p), function(p) {
    X <- matrix(sapply(0:(p - 1), function(j) pi[rows - j]), ncol = p)  # pi_t, ..., pi_{t-p+1}
    if (!is.null(u)) X <- cbind(X, u[rows])                             # u_t
    lm(y ~ X)
  })

  p <- which.min(vapply(fits, BIC, numeric(1)))   # lowest BIC [W6S2P12]
  b <- coef(fits[[p]])
  # Forecast from values at T [W7S2P33]
  x_T <- c(1, pi[n_T - 0:(p - 1)], if (!is.null(u)) u[n_T])

  list(forecast = sum(b * x_T),
       p        = p,
       lambda   = if (!is.null(u)) unname(b[length(b)]) else NA_real_)
}

# 4.4.4 Direct AR ------------------------------------------------------------
# pi_{t+h} = c_h + sum_{j=0}^{p_h-1} phi_{h,j} pi_{t-j} + e_{t+h}
fc_direct_ar <- function(data, H) {
  direct_fit(data$Inflation, H)$forecast
}

# 4.4.8 Direct Phillips curve ------------------------------------------------
# Direct AR plus u_t [W7S2P6, W7S2P9, W9S2P30]
fc_direct_pc <- function(data, H) {
  direct_fit(data$Inflation, H, u = data$Unemployment)$forecast
}
