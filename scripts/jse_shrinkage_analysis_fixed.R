# =============================================================
# JSE covariance shrinkage analysis
# Financial Economics Essay — Chane
#
# Compares an unconstrained global minimum-variance portfolio using:
#   1. Sample covariance matrix
#   2. Ledoit-Wolf (2004) linear shrinkage toward a scaled identity
#   3. Ledoit-Wolf nonlinear eigenvalue shrinkage
#
# Data: Yahoo Finance via quantmod (.JO tickers)
# =============================================================

# ---- 0. Reproducible package setup -----------------------------------
required_packages <- c("quantmod", "dplyr", "tidyr", "nlshrink", "ggplot2")
missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0L) {
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  options(repos = c(CRAN = "https://cloud.r-project.org"), timeout = 300)
  install.packages(
    missing_packages,
    dependencies = c("Depends", "Imports", "LinkingTo")
  )
}

still_missing <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]
if (length(still_missing) > 0L) {
  stop(
    "Package installation failed for: ", paste(still_missing, collapse = ", "),
    ". Restart R and run the script again. If nlshrink failed, install its ",
    "dependency first with install.packages('nloptr').",
    call. = FALSE
  )
}

suppressPackageStartupMessages({
  library(quantmod)
  library(dplyr)
  library(tidyr)
  library(nlshrink)
  library(ggplot2)
})

# ---- 1. Universe and date range --------------------------------------
# This is not a historical index-membership dataset, so survivorship and
# selection bias must be acknowledged in the essay.
tickers <- c(
  # BHP is omitted because its JSE ticker/history changes from BIL to BHP to
  # BHG during the sample; splicing those regimes is unnecessary here.
  "NPN.JO", "PRX.JO", "AGL.JO", "FSR.JO",
  "SBK.JO", "NED.JO", "ABG.JO", "SOL.JO", "MTN.JO",
  "VOD.JO", "CPI.JO", "SHP.JO", "WHL.JO", "BVT.JO",
  "APN.JO", "REM.JO", "MNP.JO", "GFI.JO", "AMS.JO"
)

start_date <- as.Date("2015-01-01")
end_date <- as.Date("2025-01-01")
minimum_coverage <- 0.90
annualisation_factor <- 252

# ---- 2. Download adjusted prices ------------------------------------
# Download one symbol at a time. A failed symbol is reported and excluded,
# instead of causing tidyquant's bulk request to return NULL for everything.
download_yahoo_symbol <- function(symbol) {
  failure_message <- NULL

  price_xts <- tryCatch(
    suppressWarnings(
      quantmod::getSymbols(
        Symbols = symbol,
        src = "yahoo",
        from = start_date,
        to = end_date,
        auto.assign = FALSE,
        warnings = FALSE
      )
    ),
    error = function(e) {
      failure_message <<- conditionMessage(e)
      NULL
    }
  )

  if (is.null(price_xts) || NROW(price_xts) == 0L) {
    message(
      "Yahoo download failed for ", symbol,
      if (!is.null(failure_message)) paste0(": ", failure_message) else ""
    )
    return(NULL)
  }

  adjusted_xts <- tryCatch(
    quantmod::Ad(price_xts),
    error = function(e) NULL
  )
  if (is.null(adjusted_xts)) {
    message("No adjusted-price column was returned for ", symbol)
    return(NULL)
  }

  tibble(
    symbol = symbol,
    date = as.Date(zoo::index(adjusted_xts)),
    adjusted = as.numeric(adjusted_xts[, 1])
  )
}

prices <- bind_rows(lapply(tickers, download_yahoo_symbol))

if (is.null(prices) || !is.data.frame(prices) || nrow(prices) == 0L) {
  stop(
    "Yahoo Finance returned no observations for any ticker. Check that R can ",
    "access query1.finance.yahoo.com, then rerun. The individual ticker ",
    "messages above contain the underlying Yahoo/HTTP error.",
    call. = FALSE
  )
}

coverage <- prices %>%
  group_by(symbol) %>%
  summarise(
    first_date = if (all(is.na(adjusted))) as.Date(NA) else min(date[!is.na(adjusted)]),
    last_date = if (all(is.na(adjusted))) as.Date(NA) else max(date[!is.na(adjusted)]),
    n_obs = sum(!is.na(adjusted)),
    .groups = "drop"
  ) %>%
  right_join(tibble(symbol = tickers), by = "symbol") %>%
  arrange(symbol)

print(coverage)

max_observations <- max(coverage$n_obs, na.rm = TRUE)
minimum_observations <- floor(minimum_coverage * max_observations)
kept_tickers <- coverage %>%
  filter(!is.na(n_obs), n_obs >= minimum_observations) %>%
  pull(symbol)
dropped_tickers <- setdiff(tickers, kept_tickers)

if (length(dropped_tickers) > 0L) {
  message(
    "Excluding tickers with failed downloads or less than ",
    minimum_coverage * 100, "% price coverage: ",
    paste(dropped_tickers, collapse = ", ")
  )
}
if (length(kept_tickers) < 5L) {
  stop("Fewer than five assets passed the data-coverage screen.", call. = FALSE)
}

# ---- 3. Compute arithmetic daily returns and reshape wide ------------
# Arithmetic returns are required because portfolio returns are weighted sums
# and are compounded below with cumprod(1 + return).
returns_long <- prices %>%
  filter(symbol %in% kept_tickers, !is.na(adjusted), adjusted > 0) %>%
  select(symbol, date, adjusted) %>%
  group_by(symbol) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(ret = adjusted / lag(adjusted) - 1) %>%
  ungroup() %>%
  filter(is.finite(ret)) %>%
  select(symbol, date, ret)

returns_wide <- returns_long %>%
  pivot_wider(names_from = symbol, values_from = ret) %>%
  arrange(date) %>%
  drop_na()

dates <- returns_wide$date
ret_mat <- as.matrix(select(returns_wide, -date))
storage.mode(ret_mat) <- "double"

if (nrow(ret_mat) <= ncol(ret_mat) + 2L) {
  stop(
    "Too few complete return observations relative to the number of assets.",
    call. = FALSE
  )
}

cat(
  "Final common sample:", format(min(dates)), "to", format(max(dates)), "|",
  nrow(ret_mat), "trading days |", ncol(ret_mat), "assets\n"
)

dir.create("data", showWarnings = FALSE, recursive = TRUE)
write.csv(returns_wide, "data/jse_returns_clean.csv", row.names = FALSE)
write.csv(coverage, "data/jse_price_coverage.csv", row.names = FALSE)

# ---- 4. Chronological train/test split -------------------------------
n <- nrow(ret_mat)
split_point <- floor(0.70 * n)

est_returns <- ret_mat[seq_len(split_point), , drop = FALSE]
oos_index <- seq.int(split_point + 1L, n)
oos_returns <- ret_mat[oos_index, , drop = FALSE]

cat(
  "Estimation window:", nrow(est_returns), "days |",
  "Out-of-sample window:", nrow(oos_returns), "days |",
  "p/n:", round(ncol(est_returns) / nrow(est_returns), 3), "\n"
)

if (ncol(est_returns) / nrow(est_returns) < 0.05) {
  message(
    "Note: p/n is small. Shrinkage benefits may be modest in this design; ",
    "discuss this when interpreting the results."
  )
}

# ---- 5. Covariance estimators ----------------------------------------
sigma_sample <- cov(est_returns)
# nlshrink::linshrink_cov uses a scaled-identity target, not a
# constant-correlation target.
sigma_lw <- nlshrink::linshrink_cov(est_returns)
sigma_nl <- nlshrink::nlshrink_cov(est_returns)

# Remove tiny numerical asymmetries and floor near-zero eigenvalues before
# inversion. This does not materially alter a well-conditioned estimate.
stabilise_covariance <- function(sigma, relative_floor = 1e-10) {
  sigma <- (sigma + t(sigma)) / 2
  eig <- eigen(sigma, symmetric = TRUE)
  eigenvalue_floor <- max(eig$values) * relative_floor
  eig$values <- pmax(eig$values, eigenvalue_floor)
  stabilised <- eig$vectors %*% diag(eig$values) %*% t(eig$vectors)
  dimnames(stabilised) <- dimnames(sigma)
  stabilised
}

sigma_sample <- stabilise_covariance(sigma_sample)
sigma_lw <- stabilise_covariance(sigma_lw)
sigma_nl <- stabilise_covariance(sigma_nl)

# ---- 6. Unconstrained global minimum-variance weights ----------------
min_var_weights <- function(sigma) {
  ones <- rep(1, ncol(sigma))
  sigma_inv_ones <- solve(sigma, ones)
  weights <- sigma_inv_ones / sum(sigma_inv_ones)
  as.numeric(weights)
}

w_sample <- min_var_weights(sigma_sample)
w_lw <- min_var_weights(sigma_lw)
w_nl <- min_var_weights(sigma_nl)

weights_table <- tibble(
  Ticker = colnames(ret_mat),
  Sample = w_sample,
  LW_Linear_Identity = w_lw,
  Nonlinear = w_nl
)
print(weights_table, n = Inf)

# ---- 7. Apply fixed weights out of sample ----------------------------
portfolio_returns <- cbind(
  Sample = as.numeric(oos_returns %*% w_sample),
  LW_Linear_Identity = as.numeric(oos_returns %*% w_lw),
  Nonlinear = as.numeric(oos_returns %*% w_nl)
)

# ---- 8. Performance and estimator diagnostics -----------------------
maximum_drawdown <- function(x) {
  wealth <- cumprod(1 + x)
  min(wealth / cummax(wealth) - 1)
}

method_labels <- c(
  Sample = "Sample covariance",
  LW_Linear_Identity = "LW linear shrinkage (identity target)",
  Nonlinear = "Nonlinear shrinkage"
)

perf_table <- tibble(
  Method = unname(method_labels),
  OOS_Annualised_Return = apply(portfolio_returns, 2, mean) * annualisation_factor,
  OOS_Annualised_Volatility = apply(portfolio_returns, 2, sd) *
    sqrt(annualisation_factor),
  OOS_Max_Drawdown = apply(portfolio_returns, 2, maximum_drawdown)
) %>%
  mutate(Sharpe_Zero_RF = OOS_Annualised_Return / OOS_Annualised_Volatility)

diagnostic_row <- function(method, sigma, weights) {
  tibble(
    Method = method,
    Condition_Number = kappa(sigma, exact = TRUE),
    Minimum_Eigenvalue = min(eigen(sigma, symmetric = TRUE, only.values = TRUE)$values),
    Gross_Leverage = sum(abs(weights)),
    Minimum_Weight = min(weights),
    Maximum_Weight = max(weights)
  )
}

diagnostics_table <- bind_rows(
  diagnostic_row(unname(method_labels["Sample"]), sigma_sample, w_sample),
  diagnostic_row(unname(method_labels["LW_Linear_Identity"]), sigma_lw, w_lw),
  diagnostic_row(unname(method_labels["Nonlinear"]), sigma_nl, w_nl)
)

print(perf_table)
print(diagnostics_table)

# ---- 9. Plot out-of-sample cumulative simple returns ----------------
cum_df <- as_tibble(portfolio_returns) %>%
  mutate(date = dates[oos_index], .before = 1) %>%
  mutate(across(-date, ~ cumprod(1 + .x) - 1)) %>%
  pivot_longer(-date, names_to = "Method", values_to = "Cumulative_Return") %>%
  mutate(Method = recode(Method, !!!method_labels))

cumulative_plot <- ggplot(
  cum_df,
  aes(x = date, y = Cumulative_Return, color = Method)
) +
  geom_line(linewidth = 0.9) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    title = "Out-of-sample global minimum-variance portfolios",
    subtitle = "Sample covariance versus Ledoit-Wolf shrinkage estimators",
    x = NULL,
    y = "Cumulative return",
    color = NULL,
    caption = "Daily adjusted Yahoo Finance data; fixed weights; zero transaction costs"
  ) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

print(cumulative_plot)

# ---- 10. Save essay outputs ------------------------------------------
dir.create("output", showWarnings = FALSE, recursive = TRUE)
ggsave(
  filename = "output/oos_cumulative_returns.png",
  plot = cumulative_plot,
  width = 8,
  height = 5,
  dpi = 300
)
write.csv(perf_table, "output/performance_comparison.csv", row.names = FALSE)
write.csv(diagnostics_table, "output/estimator_diagnostics.csv", row.names = FALSE)
write.csv(weights_table, "output/portfolio_weights.csv", row.names = FALSE)

message("Analysis complete. Results were written to data/ and output/.")
