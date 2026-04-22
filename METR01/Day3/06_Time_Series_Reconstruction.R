####################################################################################################
###
### File:    06_Time_Series_Reconstruction.R
### Purpose: Examples and exercises for Time Series Reconstruction
###          (Takens' Embedding + Granger's Causality)
### Authors: Gabriel Rodrigues Palma
### Date:    17/06/25
###
####################################################################################################
# load packages -----
source('00_source.r')

################################################################################
############ Time Series Reconstruction for Machine Learning ###################
################################################################################

# Before feeding time series into a machine learning algorithm, we must
# RECONSTRUCT it — unfold the temporal dependencies into a TABULAR format.
#
# This bridges time series analysis (Days 2-3) and machine learning (Day 4).
# The key question: "How do we convert a temporal sequence into rows and columns
# that a Random Forest or XGBoost can learn from?"
#
# The answer combines two powerful ideas:
#   1. TAKENS' EMBEDDING THEOREM — unfold a single series into a
#      multi-dimensional feature space using lagged observations
#   2. GRANGER'S CAUSALITY — identify which exogenous series (climate
#      variables) influence the target and at what time delay
#
# REFERENCE: Palma, G.R., Mello, R.F., Godoy, W.A.C., et al. (2025).
# "Forecasting insect abundance using time series embedding and machine
# learning." Ecological Informatics, 85, 102934.
# https://doi.org/10.1016/j.ecoinf.2024.102934
#
# The paper's Algorithm 1 provides the complete reconstruction pipeline
# implemented in reconstruct_time_series() in 00_source.r.

# Takens' Embedding Theorem — summary:
# Given a time series X with T observations, each observation x(t)
# is embedded into a phase-space vector:
#   phi_t = (x(t), x(t + tau), ..., x(t + (m-1)*tau))
# where m = embedding dimension, tau = time delay.
#
# We follow Palma et al. (2025): tau = 1, m = AR(p) order + 1.
#
# Granger's Causality via CCF:
#   MC(Xi, Y) = argmax CCF(Xi, Y)
# If MC(X_rainfall, Y) = -5, then rainfall 5 weeks ago has the strongest
# correlation with current aphid abundance.

# Constants -------------------------------------------------------------------
CLIMATE_VARS   <- c("temp_mean", "temp_max", "rainfall", "humidity", "wind_speed")
ADF_SIG_LEVEL  <- 0.05
CCF_LAG_MAX    <- 20
AR_ORDER_MAX   <- 15

################################################################################
############## Example 1: ADF Stationarity Tests on All Variables #############
################################################################################

# STEP 1 of the reconstruction algorithm: make all series stationary.
# ADF test: H0 = unit root (non-stationary). p < 0.05 → stationary.

aphid_data <- read.csv("input_data/aphid_climate_brazil.csv")
cat("Dataset dimensions:", dim(aphid_data), "\n")
head(aphid_data)

# Test stationarity of the target
adf_target <- tseries::adf.test(aphid_data$aphid_abundance)
print_output(adf_target, "ADF Test: Aphid Abundance (Target Y)")

# Test all variables
adf_results <- data.frame(
  variable   = c("aphid_abundance", CLIMATE_VARS),
  adf_stat   = NA,
  p_value    = NA,
  stationary = NA,
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(adf_results))) {
  var_name             <- adf_results$variable[i]
  adf                  <- tseries::adf.test(aphid_data[[var_name]])
  adf_results$adf_stat[i]   <- round(adf$statistic, 3)
  adf_results$p_value[i]    <- round(adf$p.value, 4)
  adf_results$stationary[i] <- ifelse(adf$p.value < ADF_SIG_LEVEL, "Yes", "No")
}

print_output(adf_results, "ADF Stationarity Tests (All Variables)")

################################################################################
############## Example 2: Making Series Stationary ############################
################################################################################

# Apply make_stationary() to each series (from 00_source.r).
# This implements Steps 1-2 of Algorithm 1.

stationary_data  <- list()
ndiffs_tracker   <- data.frame(variable = character(),
                                ndiffs   = integer(),
                                stringsAsFactors = FALSE)

# Target
target_stat <- make_stationary(aphid_data$aphid_abundance)
stationary_data[["aphid_abundance"]] <- target_stat$stationary
ndiffs_tracker <- rbind(ndiffs_tracker,
                        data.frame(variable = "aphid_abundance",
                                   ndiffs   = target_stat$ndiffs))

# Exogenous
for (var in CLIMATE_VARS) {
  result                    <- make_stationary(aphid_data[[var]])
  stationary_data[[var]]    <- result$stationary
  ndiffs_tracker            <- rbind(ndiffs_tracker,
                                      data.frame(variable = var,
                                                 ndiffs   = result$ndiffs))
}

print_output(ndiffs_tracker, "Number of Differences Applied to Achieve Stationarity")

# Verify stationarity after differencing
cat("\n--- Verify stationarity after differencing ---\n")
for (var in names(stationary_data)) {
  p_val <- tseries::adf.test(stationary_data[[var]])$p.value
  cat(var, ": ADF p =", round(p_val, 4),
      ifelse(p_val < ADF_SIG_LEVEL, "(Stationary)", "(Still non-stationary!)"),
      "\n")
}

################################################################################
############## Example 3: Cross-Correlation for Maximal Lag ###################
################################################################################

# STEP 3: Compute CCF between each exogenous variable and the target.
# A negative MC lag means Xi LEADS Y (Xi anticipates Y).
# Example: MC(rainfall, aphids) = -5 → rainfall 5 weeks ago has the
# strongest correlation with current aphid abundance.

# Align series lengths after differencing
min_len <- min(sapply(stationary_data, length))
for (var in names(stationary_data)) {
  stationary_data[[var]] <- tail(stationary_data[[var]], min_len)
}

target_stationary <- stationary_data[["aphid_abundance"]]

mc_results <- data.frame(
  variable    = CLIMATE_VARS,
  maximal_lag = NA,
  correlation = NA,
  stringsAsFactors = FALSE
)

par(mfrow = c(3, 2), mar = c(4, 4, 3, 1))
for (i in seq_along(CLIMATE_VARS)) {
  var <- CLIMATE_VARS[i]
  mc  <- maximal_correlation(stationary_data[[var]], target_stationary,
                               lag.max = CCF_LAG_MAX)
  mc_results$maximal_lag[i]  <- mc$lag
  mc_results$correlation[i]  <- round(mc$correlation, 4)

  ccf(stationary_data[[var]], target_stationary, lag.max = CCF_LAG_MAX,
      main = paste("CCF:", var, "vs Aphid Abundance"),
      ylab = "CCF")
  abline(v = mc$lag, col = "red", lty = 2, lwd = 2)
}
par(mfrow = c(1, 1))

print_output(mc_results, "Maximal Correlation Lags — MC(Xi, Y)")

# Biological interpretation: temperature 3 weeks ago has the strongest
# correlation with current aphid counts — matching aphid generation time
# (~2-3 weeks), since temperature directly drives development rate.

################################################################################
############## Example 4: Estimating Embedding Dimension with AR ##############
################################################################################

# STEP 4: Embedding dimension m for each series = AR(p) order + 1.
# If AR(p) best fits the data, then p past values contain the relevant
# information → m = p + 1 (p lags + the current observation).

embedding_dims <- data.frame(
  variable  = c("aphid_abundance", CLIMATE_VARS),
  ar_order  = NA,
  embed_dim = NA,
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(embedding_dims))) {
  var    <- embedding_dims$variable[i]
  ar_fit <- ar(stationary_data[[var]], aic = TRUE, order.max = AR_ORDER_MAX)
  embedding_dims$ar_order[i]  <- ar_fit$order
  embedding_dims$embed_dim[i] <- ar_fit$order + 1
}

print_output(embedding_dims, "Embedding Dimensions (m = AR order + 1)")

################################################################################
############## Example 5: Takens' Embedding on Individual Series ##############
################################################################################

# STEP 5: Apply takens_embed() (from 00_source.r) to reconstruct each series.
# With tau = 1, each x(t) becomes a row: (x(t), x(t+1), ..., x(t+m-1)).

m_target         <- embedding_dims$embed_dim[1]
target_embedded  <- takens_embed(stationary_data[["aphid_abundance"]],
                                  m = m_target, tau = 1)
names(target_embedded) <- paste0("aphid_lag_", 0:(m_target - 1))

print_output(head(target_embedded, 8),
             paste("Takens' Embedding of Aphid Abundance (m =", m_target, ")"))

# Last column (aphid_lag_0) = x(t) = TARGET for ML.
# Other columns are FEATURES (past values).

# Embed each exogenous series
exog_embedded <- list()
for (var in CLIMATE_VARS) {
  m_var   <- embedding_dims %>%
    filter(variable == var) %>%
    pull(embed_dim)
  embedded        <- takens_embed(stationary_data[[var]], m = m_var, tau = 1)
  names(embedded) <- paste0(var, "_lag_", 0:(m_var - 1))
  exog_embedded[[var]] <- embedded
  cat(var, ": embedded with m =", m_var,
      "→", ncol(embedded), "columns,", nrow(embedded), "rows\n")
}

################################################################################
############## Example 6: Merging Data Frames Based on MC(Xi, Y) #############
################################################################################

# STEP 6: Merge all embedded data frames aligned on maximal correlation lags.
# If MC(X_rainfall, Y) = -5, rainfall columns are shifted 5 steps backward
# relative to the target, aligning the most informative lag with Y.

target_df <- target_embedded
cat("Target data frame:", nrow(target_df), "rows\n")

for (i in seq_along(CLIMATE_VARS)) {
  var       <- CLIMATE_VARS[i]
  mc_lag    <- mc_results$maximal_lag[i]
  lag_shift <- abs(mc_lag)
  exog_df   <- exog_embedded[[var]]

  n_target  <- nrow(target_df)
  n_exog    <- nrow(exog_df)

  if (mc_lag < 0) {
    common_len  <- min(n_target, n_exog - lag_shift)
    target_rows <- 1:common_len
    exog_rows   <- (lag_shift + 1):(lag_shift + common_len)
  } else if (mc_lag > 0) {
    common_len  <- min(n_target - lag_shift, n_exog)
    target_rows <- (lag_shift + 1):(lag_shift + common_len)
    exog_rows   <- 1:common_len
  } else {
    common_len  <- min(n_target, n_exog)
    target_rows <- 1:common_len
    exog_rows   <- 1:common_len
  }

  if (nrow(target_df) > common_len) {
    target_df <- target_df[1:common_len, , drop = FALSE]
  }

  target_df <- cbind(target_df,
                      exog_df[exog_rows[1:nrow(target_df)], , drop = FALSE])

  cat(var, ": MC lag =", mc_lag, "→ shift", lag_shift, "\n")
}

rownames(target_df) <- NULL

print_output(head(target_df, 6),
             "Merged Reconstructed Data Frame (First 6 Rows)")
cat("Final dimensions:", dim(target_df), "\n")
cat("Columns:", paste(names(target_df), collapse = ", "), "\n")

################################################################################
############## Example 7: Full Reconstruction Using reconstruct_time_series() ##
################################################################################

# reconstruct_time_series() in 00_source.r wraps all six steps into a single
# reusable function implementing Algorithm 1 from Palma et al. (2025).

exog_list <- list(
  temp_mean  = aphid_data$temp_mean,
  temp_max   = aphid_data$temp_max,
  rainfall   = aphid_data$rainfall,
  humidity   = aphid_data$humidity,
  wind_speed = aphid_data$wind_speed
)

recon_result <- reconstruct_time_series(
  target         = aphid_data$aphid_abundance,
  exogenous_list = exog_list,
  max_lag        = CCF_LAG_MAX,
  ar_max_order   = AR_ORDER_MAX
)

D <- recon_result$data

print_output(head(D, 8), "Reconstructed Dataset (First 8 Rows)")
cat("Dataset dimensions:", dim(D), "\n")
cat("Target column:", recon_result$target_col, "\n")
cat("Maximal correlation lags:\n")
print(recon_result$mc_lags)
cat("Embedding dimensions:\n")
print(recon_result$embed_dims)

################################################################################
############## Example 8: Feature Correlation Analysis #######################
################################################################################

# Visualise which features have the strongest relationship with the target.

cor_matrix       <- cor(D)
target_cors      <- cor_matrix[, recon_result$target_col]
target_cors_sorted <- sort(abs(target_cors), decreasing = TRUE)

cat("\n--- Feature Correlations with Target (|r|, sorted) ---\n")
print(round(target_cors_sorted, 3))

top_features <- names(target_cors_sorted)[2:min(7, length(target_cors_sorted))]

p_cors <- data.frame(
  feature     = factor(top_features, levels = rev(top_features)),
  correlation = target_cors[top_features]
) %>%
  ggplot(aes(x = feature, y = abs(correlation), fill = correlation > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = pallete[c(1, 2)],
                    labels  = c("Negative", "Positive")) +
  labs(title = "Top Features: Correlation with Target (Y_lag_0)",
       subtitle = "Features selected by time series reconstruction",
       x = "", y = "|Correlation|", fill = "Direction") +
  theme_new()
print(p_cors)

################################################################################
############## Example 9: Summary Statistics of Reconstructed Features ########
################################################################################

cat("\n--- Feature Summary ---\n")
print(summary(D))

# Save the reconstructed dataset for use on Day 4
write.csv(D, "reconstructed_aphid_data.csv", row.names = FALSE)
cat("\nSaved: reconstructed_aphid_data.csv\n")

################################################################################
############## Example 10: Bridge to Day 4 — Ready for ML ####################
################################################################################

cat("\n")
cat("================================================================\n")
cat("       BRIDGE TO DAY 4: Machine Learning for Time Series        \n")
cat("================================================================\n")
cat("\n")
cat("The reconstructed data frame D has:\n")
cat("  -", nrow(D), "observations (rows)\n")
cat("  -", ncol(D), "columns (", ncol(D) - 1, "features + 1 target)\n")
cat("  - Target variable:", recon_result$target_col, "\n")
cat("\n")
cat("Tomorrow, this data frame feeds DIRECTLY into:\n")
cat("  1. Random Forest    → randomForest(Y_lag_0 ~ ., data = D)\n")
cat("  2. XGBoost          → xgb.train(...) on D as matrix\n")
cat("  3. Lasso Regression → glmnet(X, Y, alpha = 1)\n")
cat("\n")
cat("The reconstruction automatically selected:\n")
cat("  - Which climate variables to include (via Granger causality)\n")
cat("  - How many lags of each variable (via AR embedding dimension)\n")
cat("  - The optimal time alignment (via maximal correlation)\n")
cat("\n")
cat("This is the approach from Palma et al. (2025):\n")
cat("  'Forecasting insect abundance using time series embedding\n")
cat("   and machine learning' — Ecological Informatics, 85, 102934.\n")
cat("================================================================\n")

################################################################################
################## Time Series Reconstruction — Exercises #####################
################################################################################

# Exercise 1: ADF Testing Practice
# Using aphid_climate_brazil.csv:
# a) Apply ADF test to ALL variables (target + exogenous)
# b) Which variables are stationary at the 5% level?
# c) For non-stationary variables, how many differences are needed?
# d) After differencing, verify ALL series are stationary
# e) Plot original vs differenced aphid_abundance side by side

# Exercise 2: CCF Interpretation
# Using the stationary versions of the aphid and climate data:
# a) Compute the CCF between temp_mean and aphid_abundance
# b) At what lag is the maximum correlation? Is it positive or negative?
# c) Interpret this lag biologically: why does temperature affect aphid
#    populations with a delay of X weeks?
# d) Compute the CCF between rainfall and aphid_abundance
# e) Which climate variable has a stronger association with aphid dynamics?

# Exercise 3: Embedding Dimension Exploration
# a) Fit AR models to aphid_abundance with max.order = 5, 10, 15, 20
# b) Does the selected order change? Does it stabilise?
# c) Apply Takens' embedding with m = 3, 5, and 8
# d) For each m, how many features and observations are created?
# e) Discuss: more features vs fewer observations — what is the trade-off?

# Exercise 4: Manual Reconstruction (Step by Step)
# Perform the reconstruction steps MANUALLY (without the function):
# a) Make aphid_abundance and temp_mean stationary
# b) Compute CCF and find the maximal correlation lag
# c) Estimate embedding dimensions for both using AR
# d) Apply Takens' embedding to both
# e) Merge the two data frames based on the maximal correlation
# f) Compare your result to the output of reconstruct_time_series()

# Exercise 5: Reconstruction with Subset of Variables
# a) Run reconstruct_time_series() with ONLY temp_mean and rainfall
# b) Run it with ALL 5 climate variables (already done above)
# c) Compare the resulting datasets: dimensions, features, correlations
# d) Does including more exogenous variables always improve reconstruction?
# e) How would you decide which variables to include in practice?

################################################################################
################## Time Series Reconstruction — Answers ######################
################################################################################

# Answer 1: ADF Tests on All Variables
adf_all <- data.frame(
  variable   = c("aphid_abundance", CLIMATE_VARS),
  p_value    = NA,
  stationary = NA,
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(adf_all))) {
  adf                       <- tseries::adf.test(aphid_data[[adf_all$variable[i]]])
  adf_all$p_value[i]        <- round(adf$p.value, 4)
  adf_all$stationary[i]     <- adf$p.value < ADF_SIG_LEVEL
}
print(adf_all)

# c) Differencing for non-stationary variables
non_stationary_vars <- adf_all$variable[!adf_all$stationary]
diff_results <- list()
for (var in non_stationary_vars) {
  result           <- make_stationary(aphid_data[[var]])
  diff_results[[var]] <- result
  cat(var, ": needed", result$ndiffs, "difference(s)\n")
}

# d) Verify
for (var in names(diff_results)) {
  p <- tseries::adf.test(diff_results[[var]]$stationary)$p.value
  cat(var, ": post-differencing ADF p =", round(p, 4), "\n")
}

# e) Plot example — aphid abundance
stat_aphid_ex1 <- make_stationary(aphid_data$aphid_abundance)
par(mfrow = c(1, 2))
plot(aphid_data$aphid_abundance, type = "l",
     main = "Original", ylab = "Count")
plot(stat_aphid_ex1$stationary, type = "l",
     main = "Differenced", ylab = "Diff")
par(mfrow = c(1, 1))

# Answer 2: CCF Interpretation
stat_temp   <- make_stationary(aphid_data$temp_mean)
stat_aphid2 <- make_stationary(aphid_data$aphid_abundance)
min_l       <- min(length(stat_temp$stationary), length(stat_aphid2$stationary))

ccf(tail(stat_temp$stationary, min_l),
    tail(stat_aphid2$stationary, min_l),
    lag.max = CCF_LAG_MAX, plot = TRUE,
    main = "CCF: temp_mean vs aphid_abundance")

mc_temp <- maximal_correlation(tail(stat_temp$stationary, min_l),
                                tail(stat_aphid2$stationary, min_l))
cat("Max correlation at lag:", mc_temp$lag,
    " r =", round(mc_temp$correlation, 3), "\n")

# c) Temperature affects aphid development rate: warm periods accelerate
#    reproduction, leading to higher counts 2-4 weeks later (generation time).

stat_rain <- make_stationary(aphid_data$rainfall)
min_l2    <- min(length(stat_rain$stationary), length(stat_aphid2$stationary))
mc_rain   <- maximal_correlation(tail(stat_rain$stationary, min_l2),
                                  tail(stat_aphid2$stationary, min_l2))
cat("Rainfall: max correlation at lag:", mc_rain$lag,
    " r =", round(mc_rain$correlation, 3), "\n")

# e) Temperature typically has stronger correlation because aphids are
#    poikilothermic — their development is directly temperature-dependent.
#    Rainfall has an indirect effect (affects plant growth → food supply).

# Answer 3: Embedding Dimension Sensitivity
for (max_p in c(5, 10, 15, 20)) {
  ar_fit <- ar(stat_aphid2$stationary, aic = TRUE, order.max = max_p)
  cat("max.order =", max_p, "→ selected order:", ar_fit$order, "\n")
}
# Order typically stabilises around 3-5 for ecological data.

for (m in c(3, 5, 8)) {
  emb <- takens_embed(stat_aphid2$stationary, m = m, tau = 1)
  cat("m =", m, "→", ncol(emb), "features,", nrow(emb), "observations\n")
}
# Higher m → more features but fewer rows. For small ecological datasets,
# large m can lead to overfitting.

# Answer 4: Manual Reconstruction
s_aphid <- make_stationary(aphid_data$aphid_abundance)
s_temp  <- make_stationary(aphid_data$temp_mean)

min_l4 <- min(length(s_aphid$stationary), length(s_temp$stationary))
mc4    <- maximal_correlation(tail(s_temp$stationary, min_l4),
                               tail(s_aphid$stationary, min_l4))
cat("MC lag:", mc4$lag, "\n")

ar_a4 <- ar(tail(s_aphid$stationary, min_l4), aic = TRUE)
ar_t4 <- ar(tail(s_temp$stationary,  min_l4), aic = TRUE)
m_a4  <- max(2, ar_a4$order + 1)
m_t4  <- max(2, ar_t4$order + 1)

emb_a4       <- takens_embed(tail(s_aphid$stationary, min_l4), m = m_a4, tau = 1)
emb_t4       <- takens_embed(tail(s_temp$stationary,  min_l4), m = m_t4, tau = 1)
names(emb_a4) <- paste0("aphid_lag_", (m_a4-1):0)
names(emb_t4) <- paste0("temp_lag_",  (m_t4-1):0)

lag_shift4 <- abs(mc4$lag)
n_a4       <- nrow(emb_a4)
n_t4       <- nrow(emb_t4)

if (mc4$lag < 0) {
  common4   <- min(n_a4, n_t4 - lag_shift4)
  d_manual  <- cbind(emb_a4[1:common4, ],
                      emb_t4[(lag_shift4 + 1):(lag_shift4 + common4), ])
} else {
  common4   <- min(n_a4, n_t4)
  d_manual  <- cbind(emb_a4[1:common4, ], emb_t4[1:common4, ])
}

print_output(head(d_manual), "Manually Reconstructed Data Frame")

# f) Compare with function output
recon_manual <- reconstruct_time_series(
  target         = aphid_data$aphid_abundance,
  exogenous_list = list(temp_mean = aphid_data$temp_mean)
)
cat("Function output dimensions:", dim(recon_manual$data), "\n")
cat("Manual output dimensions:",   dim(d_manual), "\n")

# Answer 5: Reconstruction with Subset vs Full Variable Set
recon_subset <- reconstruct_time_series(
  target         = aphid_data$aphid_abundance,
  exogenous_list = list(
    temp_mean = aphid_data$temp_mean,
    rainfall  = aphid_data$rainfall
  )
)

cat("Subset dimensions:", dim(recon_subset$data), "\n")
cat("Full dimensions:",   dim(recon_result$data), "\n")

# d) More variables = more features, but:
#    - Risk of overfitting with small sample sizes
#    - Irrelevant features add noise
#    - Curse of dimensionality
# e) Include variables with known ecological relationships and validate
#    using CCF/Granger tests. Domain knowledge + statistical testing = best.

################################################################################
############## Time Series Reconstruction — Advanced Examples #################
################################################################################

# Advanced Example 1: Reconstruction Quality Metric
# Compare how well the reconstructed features predict the target by fitting
# a simple linear model and computing R-squared.

d_temp <- recon_result$data
target_col <- recon_result$target_col
feature_cols <- setdiff(names(d_temp), target_col)

lm_fit <- lm(reformulate(feature_cols, target_col), data = d_temp)
cat("Reconstruction linear model R-squared:",
    round(summary(lm_fit)$r.squared, 3), "\n")
cat("This measures how much variance in aphid abundance is explained by\n")
cat("the reconstructed features.\n")

# Advanced Example 2: Sensitivity to CCF Lag Maximum
# How does the choice of lag.max affect the reconstructed dataset?

recon_lag10 <- reconstruct_time_series(
  target         = aphid_data$aphid_abundance,
  exogenous_list = exog_list,
  max_lag        = 10
)

recon_lag30 <- reconstruct_time_series(
  target         = aphid_data$aphid_abundance,
  exogenous_list = exog_list,
  max_lag        = 30
)

cat("Reconstruction with lag.max = 10 — MC lags:\n")
print(recon_lag10$mc_lags)

cat("Reconstruction with lag.max = 30 — MC lags:\n")
print(recon_lag30$mc_lags)

# Advanced Example 3: Visualising the Phase Space
# Takens' embedding creates a multi-dimensional phase space.
# We can visualise the 2D projection (lag_0 vs lag_1).

target_phase <- takens_embed(
  stationary_data[["aphid_abundance"]],
  m = 2, tau = 1
)

p_phase <- ggplot(target_phase, aes(x = lag_0, y = lag_1)) +
  geom_point(alpha = 0.4, colour = pallete[1], size = 1.5) +
  geom_path(alpha = 0.2, colour = pallete[2]) +
  labs(title = "Phase-Space Reconstruction: Aphid Abundance",
       subtitle = "x(t) vs x(t+1) — Takens' embedding with m=2, tau=1",
       x = "x(t)", y = "x(t+1)") +
  theme_new()
print(p_phase)

# Advanced Example 4: Comparing Reconstruction with Different AR Order Bounds
# The AR order determines the embedding dimension. Try different bounds.

for (max_ar in c(5, 10, 15)) {
  recon_test <- reconstruct_time_series(
    target         = aphid_data$aphid_abundance,
    exogenous_list = list(temp_mean = aphid_data$temp_mean),
    ar_max_order   = max_ar
  )
  cat("AR order bound =", max_ar,
      "→ target m =", recon_test$embed_dims["target"],
      "→ dataset dims:", dim(recon_test$data), "\n")
}

# Advanced Example 5: Multi-Target Reconstruction
# What if we want to predict BOTH aphid abundance AND ladybird counts?
# Run separate reconstructions for each target and merge the feature matrices.

ladybird_data <- read.csv("ladybird_aphid_dynamics.csv")

recon_aphid_la <- reconstruct_time_series(
  target         = ladybird_data$aphid_count,
  exogenous_list = list(ladybird = ladybird_data$ladybird_count),
  max_lag        = CCF_LAG_MAX,
  ar_max_order   = 10
)

recon_lady_la <- reconstruct_time_series(
  target         = ladybird_data$ladybird_count,
  exogenous_list = list(aphid = ladybird_data$aphid_count),
  max_lag        = CCF_LAG_MAX,
  ar_max_order   = 10
)

cat("Aphid prediction dataset:", dim(recon_aphid_la$data), "\n")
cat("Ladybird prediction dataset:", dim(recon_lady_la$data), "\n")

################################################################################
############# Time Series Reconstruction — Advanced Exercises #################
################################################################################

# Advanced Exercise 1: Reconstruction with EMD Features
# Combine the EMD decomposition (Script 05) with the reconstruction pipeline:
# a) Apply EMD to aphid_abundance and extract the 4 frequency bands
# b) Apply EMD to temp_mean and extract the 4 frequency bands
# c) Use the band values as exogenous_list in reconstruct_time_series()
# d) Compare this "EMD-enhanced reconstruction" to the standard reconstruction
# e) Does using EMD bands as inputs improve the linear model R-squared?

# Advanced Exercise 2: Leave-One-Out Cross-Validation
# a) For the reconstructed dataset D, implement LOO cross-validation
# b) For each fold, fit a linear model on the training rows and predict the
#    left-out row using the fitted model
# c) Compute mean squared prediction error across all folds
# d) Compare to the in-sample R-squared — is the model overfit?

# Advanced Exercise 3: Sensitivity to Differencing
# a) Run reconstruct_time_series() with all series left UNDIFFERENCED
#    (set significance = 1 to skip ADF filtering)
# b) Compare the resulting feature matrix to the standard reconstruction
# c) Does non-stationarity inflate the CCF? Do the MC lags change?
# d) Fit a linear model to both datasets — does R-squared change?

# Advanced Exercise 4: Reconstruction for Waterbird Forecasting
# a) Load waterbird_migration.csv and ladybird_aphid_dynamics.csv
# b) Use waterbird$count as the target and monthly rainfall/temperature
#    as exogenous (simulate with rnorm if climate data is unavailable)
# c) Run reconstruct_time_series() to create the ML feature matrix
# d) Fit a linear model and evaluate on the last 24 months

# Advanced Exercise 5: Bootstrap Confidence for MC Lags
# The maximal correlation lag is a sample statistic. Assess its uncertainty:
# a) Bootstrap the aphid and temperature CCF 200 times (resample rows with
#    replacement from the stationary data)
# b) For each bootstrap sample, compute the MC lag
# c) Plot the distribution of bootstrap MC lags
# d) How wide is the 95% CI for the MC lag?
# e) If the CI is wide, does this affect the reliability of the reconstruction?
