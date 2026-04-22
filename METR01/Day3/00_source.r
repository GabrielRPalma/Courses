####################################################################################################
###
### File:    00_source.R
### Purpose: Load required packages and helper functions for
###          Day 3: Fundamentals of Time Series Analysis II
### Authors: Gabriel Rodrigues Palma
### Date:    17/06/25
###
####################################################################################################

# packages required ---------------------------

packages <- c("fpp3", "tidyverse", "patchwork", "RColorBrewer",
              "tsibble", "feasts", "fable", "vars", "tseries",
              "rugarch", "EMD", "forecast", "seasonal", "slider")
#install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# Main functions ---------------------------

# print_output: prints a labelled result to the console
# Use this to display intermediate results during teaching
#
# Args:
#   result: any R object to print
#   label:  character label shown above the result
#
# Returns:
#   invisibly returns result
print_output <- function(result, label = "Result") {
  if (!is.character(label)) stop("label must be a character string")
  cat("\n---", label, "---\n")
  print(result)
  cat("\n")
  invisible(result)
}

# make_stationary: iteratively applies first-order differencing until the
# ADF test rejects the unit-root null (p < significance).
#
# Args:
#   x:            numeric vector (the time series)
#   max_diffs:    integer, maximum number of differences to attempt (default 5)
#   significance: numeric, p-value threshold for the ADF test (default 0.05)
#
# Returns:
#   list with elements:
#     $stationary  numeric vector after differencing
#     $ndiffs      integer, number of differences applied
make_stationary <- function(x, max_diffs = 5, significance = 0.05) {
  if (!is.numeric(x)) stop("x must be a numeric vector")
  if (length(x) < 10)  stop("x must have at least 10 observations")
  if (significance <= 0 || significance >= 1) {
    stop("significance must be between 0 and 1")
  }
  d       <- 0
  current <- x
  while (d < max_diffs) {
    adf_p <- tseries::adf.test(na.omit(current))$p.value
    if (adf_p < significance) break
    current <- diff(current)
    d       <- d + 1
  }
  return(list(stationary = current, ndiffs = d))
}

# takens_embed: applies Takens' embedding theorem to reconstruct a time series
# as a multivariate phase-space matrix.
#
# Args:
#   x:   numeric vector, the time series to embed
#   m:   integer, embedding dimension (number of lag columns)
#   tau: integer, time delay between coordinates (default 1)
#
# Returns:
#   data.frame with m columns named lag_0, lag_tau, ..., lag_{(m-1)*tau}
takens_embed <- function(x, m, tau = 1) {
  if (!is.numeric(x))          stop("x must be a numeric vector")
  if (!is.numeric(m) || m < 1) stop("m must be a positive integer")
  if (!is.numeric(tau) || tau < 1) stop("tau must be a positive integer")
  n    <- length(x)
  rows <- n - (m - 1) * tau
  if (rows < 1) stop("Time series too short for the given m and tau")
  mat <- matrix(NA, nrow = rows, ncol = m)
  for (j in 1:m) {
    start    <- 1 + (j - 1) * tau
    mat[, j] <- x[start:(start + rows - 1)]
  }
  df       <- as.data.frame(mat)
  names(df) <- paste0("lag_", (0:(m - 1)) * tau)
  return(df)
}

# maximal_correlation: computes the cross-correlation function (CCF) between
# two series and returns the lag at which the absolute correlation is maximised.
#
# Args:
#   x:       numeric vector, the exogenous (leading) series
#   y:       numeric vector, the target series
#   lag.max: integer, maximum lag to consider (default 20)
#
# Returns:
#   list with elements:
#     $lag         integer, the lag of maximum absolute correlation
#     $correlation numeric, the CCF value at that lag
maximal_correlation <- function(x, y, lag.max = 20) {
  if (!is.numeric(x) || !is.numeric(y)) stop("x and y must be numeric vectors")
  if (length(x) != length(y))           stop("x and y must have the same length")
  if (lag.max < 1)                       stop("lag.max must be at least 1")
  ccf_result <- ccf(x, y, lag.max = lag.max, plot = FALSE)
  best_idx   <- which.max(abs(ccf_result$acf))
  best_lag   <- ccf_result$lag[best_idx]
  best_cor   <- ccf_result$acf[best_idx]
  return(list(lag = best_lag, correlation = best_cor))
}

# compute_garch_persistence: extracts the volatility-persistence parameter
# (alpha1 + beta1) from a fitted rugarch object.
#
# Args:
#   fit: a fitted ugarchfit object from rugarch
#
# Returns:
#   numeric scalar, the persistence value (alpha1 + beta1)
compute_garch_persistence <- function(fit) {
  if (!inherits(fit, "uGARCHfit")) stop("fit must be a ugarchfit object")
  coefs       <- coef(fit)
  alpha1      <- coefs["alpha1"]
  beta1       <- coefs["beta1"]
  persistence <- alpha1 + beta1
  cat("Volatility persistence (alpha1 + beta1):", round(persistence, 4), "\n")
  cat("(Values close to 1 indicate slow-decaying volatility)\n")
  return(invisible(persistence))
}

# reconstruct_time_series: implements Algorithm 1 from Palma et al. (2025)
# to convert a multivariate ecological time series into a tabular ML dataset.
#
# Steps: (1) make Y and each Xi stationary; (2) compute maximal-correlation
# lags via CCF; (3) estimate embedding dimensions via AR models; (4) apply
# Takens' embedding; (5) merge all embedded data frames with MC-lag alignment.
#
# Args:
#   target:          numeric vector, the response time series Y
#   exogenous_list:  named list of numeric vectors, the exogenous series {Xi}
#   max_lag:         integer, maximum CCF lag to consider (default 20)
#   ar_max_order:    integer, maximum AR order for AIC selection (default 15)
#   significance:    numeric, ADF significance threshold (default 0.05)
#
# Returns:
#   list with elements:
#     $data        data.frame, the reconstructed ML-ready dataset
#     $target_col  character, name of the response column ("Y_lag_0")
#     $mc_lags     named numeric vector, MC lags for each exogenous series
#     $embed_dims  named numeric vector, embedding dimension for each series
#     $ndiffs      integer, differences applied to the target
reconstruct_time_series <- function(target,
                                     exogenous_list,
                                     max_lag      = 20,
                                     ar_max_order = 15,
                                     significance = 0.05) {
  if (!is.numeric(target)) stop("target must be a numeric vector")
  if (!is.list(exogenous_list)) stop("exogenous_list must be a named list")
  if (is.null(names(exogenous_list))) stop("exogenous_list must have named elements")

  cat("=== Time Series Reconstruction Algorithm ===\n\n")

  # --- Step 1: Make Y stationary ---
  cat("Step 1: Making target series stationary...\n")
  target_result <- make_stationary(target, significance = significance)
  y_stat        <- target_result$stationary
  cat("  Target:", target_result$ndiffs, "difference(s) applied\n")

  # --- Step 2: Make each Xi stationary ---
  cat("Step 2: Making exogenous series stationary...\n")
  x_stat <- list()
  for (series_name in names(exogenous_list)) {
    result              <- make_stationary(exogenous_list[[series_name]],
                                            significance = significance)
    x_stat[[series_name]] <- result$stationary
    cat(" ", series_name, ":", result$ndiffs, "difference(s) applied\n")
  }

  # Align lengths after differencing
  min_len <- min(length(y_stat), min(sapply(x_stat, length)))
  y_stat  <- tail(y_stat, min_len)
  for (series_name in names(x_stat)) {
    x_stat[[series_name]] <- tail(x_stat[[series_name]], min_len)
  }

  # --- Step 3: Maximal correlation delays ---
  cat("\nStep 3: Computing maximal correlation delays (CCF)...\n")
  mc_lags <- numeric(length(x_stat))
  names(mc_lags) <- names(x_stat)
  for (series_name in names(x_stat)) {
    mc              <- maximal_correlation(x_stat[[series_name]], y_stat,
                                            lag.max = max_lag)
    mc_lags[series_name] <- mc$lag
    cat(" MC(", series_name, ", Y) =", mc$lag,
        " (r =", round(mc$correlation, 3), ")\n")
  }

  # --- Step 4: Estimate embedding dimensions ---
  cat("\nStep 4: Estimating embedding dimensions (AR models)...\n")
  ar_target <- ar(y_stat, aic = TRUE, order.max = ar_max_order)
  m_target  <- max(2, ar_target$order + 1)
  cat("  Target: AR(", ar_target$order, ") → m =", m_target, "\n")

  m_exog <- numeric(length(x_stat))
  names(m_exog) <- names(x_stat)
  for (series_name in names(x_stat)) {
    ar_fit                  <- ar(x_stat[[series_name]], aic = TRUE,
                                   order.max = ar_max_order)
    m_exog[series_name]     <- max(2, ar_fit$order + 1)
    cat(" ", series_name, ": AR(", ar_fit$order, ") → m =", m_exog[series_name], "\n")
  }

  # --- Step 5: Takens' embedding ---
  cat("\nStep 5: Applying Takens' embedding (tau = 1)...\n")
  target_emb            <- takens_embed(y_stat, m = m_target, tau = 1)
  names(target_emb)     <- paste0("Y_lag_", (m_target - 1):0)
  cat("  Target:", ncol(target_emb), "columns,", nrow(target_emb), "rows\n")

  exog_emb <- list()
  for (series_name in names(x_stat)) {
    emb                   <- takens_embed(x_stat[[series_name]],
                                           m = m_exog[series_name], tau = 1)
    names(emb)            <- paste0(series_name, "_lag_",
                                     (m_exog[series_name] - 1):0)
    exog_emb[[series_name]] <- emb
    cat(" ", series_name, ":", ncol(emb), "columns,", nrow(emb), "rows\n")
  }

  # --- Step 6: Merge datasets based on MC lags ---
  cat("\nStep 6: Merging datasets based on MC(Xi, Y)...\n")
  D <- target_emb

  for (series_name in names(exog_emb)) {
    lag_val    <- mc_lags[series_name]
    lag_shift  <- abs(lag_val)
    exog_df    <- exog_emb[[series_name]]
    n_d        <- nrow(D)
    n_exog     <- nrow(exog_df)

    if (lag_val < 0) {
      common_len    <- min(n_d, n_exog - lag_shift)
      D             <- D[1:common_len, , drop = FALSE]
      exog_aligned  <- exog_df[(lag_shift + 1):(lag_shift + common_len), ,
                                drop = FALSE]
    } else if (lag_val > 0) {
      common_len    <- min(n_d - lag_shift, n_exog)
      D             <- D[(lag_shift + 1):(lag_shift + common_len), , drop = FALSE]
      exog_aligned  <- exog_df[1:common_len, , drop = FALSE]
    } else {
      common_len    <- min(n_d, n_exog)
      D             <- D[1:common_len, , drop = FALSE]
      exog_aligned  <- exog_df[1:common_len, , drop = FALSE]
    }

    rownames(exog_aligned) <- rownames(D)
    D <- cbind(D, exog_aligned)
    cat("  Merged", series_name, "(MC lag =", lag_val, "),",
        "dataset now:", nrow(D), "rows x", ncol(D), "cols\n")
  }

  rownames(D) <- NULL

  cat("\n=== Reconstruction Complete ===\n")
  cat("Final dataset:", nrow(D), "rows x", ncol(D), "columns\n")
  cat("Response variable: Y_lag_0\n")
  cat("Feature columns:", ncol(D) - 1, "\n\n")

  return(list(
    data       = D,
    target_col = "Y_lag_0",
    mc_lags    = mc_lags,
    embed_dims = c(target = m_target, m_exog),
    ndiffs     = target_result$ndiffs
  ))
}

# plot settings ---------------------------
pallete = RColorBrewer::brewer.pal(9, "Set1")[ c(3, 1, 9, 6, 8, 5, 2) ]

theme_new <- function(base_size = 15, base_family = "Arial"){
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text = element_text(size = 15, colour = "grey30"),
      legend.key=element_rect(colour=NA, fill =NA),
      axis.line = element_line(colour = 'black'),
      axis.ticks =         element_line(colour = "grey20"),
      plot.title.position = 'plot',
      legend.position = "bottom"
    )
}
