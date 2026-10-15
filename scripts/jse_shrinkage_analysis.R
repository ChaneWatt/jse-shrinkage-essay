# =============================================================
# JSE Covariance Shrinkage Analysis
# Financial Economics Essay — Chane
#
# Compares minimum-variance portfolio performance using:
#   1. Sample covariance matrix
#   2. Ledoit-Wolf linear shrinkage (2004, constant-correlation target)
#   3. Nonlinear shrinkage (Ledoit & Wolf, 2012/2017)
#
# Data: Yahoo Finance via tidyquant (.JO tickers)
# =============================================================

# ---- 0. Packages -----------------------------------------------------
# install.packages(c("tidyquant","dplyr","tidyr","purrr","nlshrink","ggplot2"))
library(tidyquant)
library(dplyr)
library(tidyr)
library(purrr)
library(nlshrink)
library(ggplot2)

# ---- 1. Universe & date range -----------------------------------------
# A spread of ~20 large-cap JSE Top 40 constituents across sectors.
# Adjust freely — add/remove tickers as needed.
tickers <- c(
  "NPN.JO", "PRX.JO", "BHP.JO", "AGL.JO", "FSR.JO",
  "SBK.JO", "NED.JO", "ABG.JO", "SOL.JO", "MTN.JO",
  "VOD.JO", "CPI.JO", "SHP.JO", "WHL.JO", "BVT.JO",
  "APN.JO", "REM.JO", "MNP.JO", "GFI.JO", "AMS.JO"
)

start_date <- "2015-01-01"
end_date   <- "2025-01-01"

# ---- 2. Download prices -----------------------------------------------
prices <- tq_get(tickers, from = start_date, to = end_date, get = "stock.prices")

# Quick check: flag any tickers that failed to download / have poor coverage
coverage <- prices %>%
  group_by(symbol) %>%
  summarise(n_obs = n(), .groups = "drop")
print(coverage)

# ---- 3. Compute daily log returns, reshape wide -----------------------
returns_long <- prices %>%
  group_by(symbol) %>%
  tq_transmute(
    select     = adjusted,
    mutate_fun = periodReturn,
    period     = "daily",
    type       = "log",
    col_rename = "ret"
  ) %>%
  ungroup()

returns_wide <- returns_long %>%
  pivot_wider(names_from = symbol, values_from = ret) %>%
  drop_na()                      # keep only dates where ALL tickers have data

dates   <- returns_wide$date
ret_mat <- as.matrix(returns_wide %>% select(-date))

cat("Final sample:", nrow(ret_mat), "trading days,", ncol(ret_mat), "assets\n")

# Save the cleaned dataset as a real artifact in the repo
dir.create("data", showWarnings = FALSE)
write.csv(returns_wide, "data/jse_returns_clean.csv", row.names = FALSE)

# ---- 4. Train / test split ---------------------------------------------
n <- nrow(ret_mat)
split_point <- floor(0.7 * n)

est_returns <- ret_mat[1:split_point, ]          # used to ESTIMATE covariance
oos_returns <- ret_mat[(split_point + 1):n, ]    # held out to TEST performance

cat("Estimation window:", nrow(est_returns), "days |",
    "Out-of-sample window:", nrow(oos_returns), "days\n")

# ---- 5. Covariance estimators ------------------------------------------
sigma_sample <- cov(est_returns)                 # raw sample covariance
sigma_lw     <- linshrink_cov(est_returns)       # Ledoit-Wolf (2004) linear shrinkage
sigma_nl     <- nlshrink_cov(est_returns)        # Ledoit-Wolf (2012/2017) nonlinear shrinkage

# ---- 6. Minimum-variance portfolio weights ------------------------------
min_var_weights <- function(sigma) {
  ones      <- rep(1, ncol(sigma))
  inv_sigma <- solve(sigma)
  w <- inv_sigma %*% ones / as.numeric(t(ones) %*% inv_sigma %*% ones)
  return(as.numeric(w))
}

w_sample <- min_var_weights(sigma_sample)
w_lw     <- min_var_weights(sigma_lw)
w_nl     <- min_var_weights(sigma_nl)

# Optional: inspect weight concentration (sample often produces extreme/
# negative weights — this itself is worth a sentence in your essay)
weights_table <- tibble(
  Ticker = colnames(ret_mat),
  Sample = w_sample,
  LW_Linear = w_lw,
  Nonlinear = w_nl
)
print(weights_table)

# ---- 7. Apply weights out-of-sample -------------------------------------
port_ret_sample <- oos_returns %*% w_sample
port_ret_lw     <- oos_returns %*% w_lw
port_ret_nl     <- oos_returns %*% w_nl

# ---- 8. Performance comparison -------------------------------------------
ann_factor <- 252

perf_table <- tibble(
  Method         = c("Sample Covariance", "Ledoit-Wolf Linear Shrinkage", "Nonlinear Shrinkage"),
  OOS_Volatility = c(sd(port_ret_sample), sd(port_ret_lw), sd(port_ret_nl)) * sqrt(ann_factor),
  OOS_MeanReturn = c(mean(port_ret_sample), mean(port_ret_lw), mean(port_ret_nl)) * ann_factor
) %>%
  mutate(Sharpe = OOS_MeanReturn / OOS_Volatility)

print(perf_table)

# ---- 9. Plot: out-of-sample cumulative returns ---------------------------
cum_df <- tibble(
  date      = dates[(split_point + 1):n],
  Sample    = cumprod(1 + as.numeric(port_ret_sample)) - 1,
  LW_Linear = cumprod(1 + as.numeric(port_ret_lw)) - 1,
  Nonlinear = cumprod(1 + as.numeric(port_ret_nl)) - 1
) %>%
  pivot_longer(-date, names_to = "Method", values_to = "CumReturn")

ggplot(cum_df, aes(x = date, y = CumReturn, color = Method)) +
  geom_line(linewidth = 0.9) +
  labs(
    title = "Out-of-sample performance: minimum-variance portfolios",
    subtitle = "Sample covariance vs. Ledoit-Wolf shrinkage estimators (JSE large caps)",
    x = NULL, y = "Cumulative return"
  ) +
  theme_minimal()

# ---- 10. Save outputs for the essay ----------------------------
dir.create("output", showWarnings = FALSE)
ggsave("output/oos_cumulative_returns.png", width = 8, height = 5)
write.csv(perf_table, "output/performance_comparison.csv", row.names = FALSE)
write.csv(weights_table, "output/portfolio_weights.csv", row.names = FALSE)
