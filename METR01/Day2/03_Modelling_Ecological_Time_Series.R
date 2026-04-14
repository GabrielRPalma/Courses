####################################################################################################
###
### File:    03_Modelling_Ecological_Time_Series.R
### Purpose: Examples and exercises for Modelling Ecological Time Series
### Authors: Gabriel Rodrigues Palma
### Date:    17/06/25
###
####################################################################################################
# load packages -----
source('00_source.r')

################################################################################
################## Modelling Ecological Time Series ############################
################################################################################
# The Box-Jenkins approach provides a systematic four-step framework for
# building forecasting models:
#   1. IDENTIFY — examine the data, check stationarity, select candidate models
#   2. ESTIMATE — fit the model parameters to the training data
#   3. DIAGNOSE — check residuals (are they white noise after model fitting?)
#   4. FORECAST — generate predictions with uncertainty intervals
#
# Two dominant families of models exist in the fable/fpp3 ecosystem:
#   ETS (Exponential Smoothing) — models the level, trend, and seasonality
#     components directly and is well-suited to strongly seasonal ecological data.
#   ARIMA — models autocorrelation structure after differencing to stationarity;
#     more flexible for complex autocorrelation and able to incorporate predictors.
#
# Before fitting any model, we: (1) TRANSFORM data to stabilise variance if
# necessary (log, Box-Cox), and (2) BENCHMARK against simple naive methods to
# establish the minimum bar our model must beat to be useful.

################################################################################
############### Example 1: The Two Dominant Modelling Approaches ###############
################################################################################

# Load butterfly abundance data and think about which approach to use.
# The visual inspection step is always the first modelling decision.

butterfly_ts <- load_ecological_tsibble(
  file_path  = "input_data/butterfly_meadow_monitoring.csv",
  index_col  = "date",
  index_type = "yearmonth"
)

# Visual inspection — what structural features must our model capture?
autoplot(butterfly_ts, abundance) +
  theme_new() +
  labs(title    = "Butterfly Abundance — What Model Should We Use?",
       subtitle = "Trend + Seasonality → ETS or ARIMA with seasonal component",
       y        = "Abundance (count)")

# ETS (Error-Trend-Seasonality) approach:
# - Directly models level (l_t), trend (b_t), and seasonal (s_t) components
# - Automatically handles additive vs multiplicative seasonality
# - Good when seasonal patterns are the dominant feature
# - Simple to interpret: forecast = (level) + (trend) + (seasonal)

# ARIMA approach:
# - Models autocorrelation structure after differencing to achieve stationarity
# - More flexible for complex autocorrelation patterns
# - Can incorporate external predictors (ARIMAX) — see script 05
# - Requires careful diagnosis of ACF/PACF patterns

# In practice, we always try BOTH and compare using accuracy metrics on
# a held-out test set — let the data decide.

################################################################################
############### Example 2: Log and Box-Cox Transformations #####################
################################################################################

# When the seasonal variation grows proportionally with the trend level,
# we need to stabilise the variance BEFORE modelling. Failing to transform
# leads to heteroscedastic residuals (violating model assumptions) and
# prediction intervals that are too wide at high levels and too narrow at low.

# Box-Cox transformation:
#   w_t = log(y_t)                   if lambda = 0
#   w_t = (y_t^lambda - 1) / lambda  otherwise
# The Guerrero method automatically selects the optimal lambda.

# Australian beer production — classic example with slight growing amplitude
beer_ts <- aus_production %>%
  dplyr::filter(year(Quarter) >= 1992) %>%
  dplyr::select(Quarter, Beer)

# Original data
plot_beer_raw <- autoplot(beer_ts, Beer) +
  theme_new() +
  labs(title = "Beer Production — Original",
       y     = "Megalitres")

# Log transformation — useful when variance grows with the level
plot_beer_log <- beer_ts %>%
  dplyr::mutate(log_beer = log(Beer)) %>%
  autoplot(log_beer) +
  theme_new() +
  labs(title = "Beer Production — Log Transformed",
       y     = "log(Megalitres)")

plot_beer_raw / plot_beer_log

# Automatic Box-Cox lambda selection using Guerrero's method
lambda_beer <- beer_ts %>%
  features(Beer, features = guerrero) %>%
  dplyr::pull(lambda_guerrero)

cat("Optimal Box-Cox lambda for beer:", round(lambda_beer, 3), "\n")

# Apply Box-Cox transformation with the optimal lambda
plot_beer_boxcox <- beer_ts %>%
  dplyr::mutate(bc_beer = box_cox(Beer, lambda_beer)) %>%
  autoplot(bc_beer) +
  theme_new() +
  labs(title = paste0("Beer Production — Box-Cox (lambda = ",
                      round(lambda_beer, 2), ")"),
       y     = "Transformed value")

print(plot_beer_boxcox)

# Ecological application: Box-Cox on butterfly abundance
lambda_butterfly <- butterfly_ts %>%
  features(abundance, features = guerrero) %>%
  dplyr::pull(lambda_guerrero)

cat("Optimal Box-Cox lambda for butterfly abundance:",
    round(lambda_butterfly, 3), "\n")
# Interpretation:
# lambda near 0   → log transform is appropriate (exponential growth)
# lambda near 0.5 → square root transform (count data with Poisson-like variance)
# lambda near 1   → no transformation needed (additive noise structure)

################################################################################
############### Example 3: Benchmark Models ####################################
################################################################################

# Before building complex models, we must establish benchmarks.
# A model that cannot beat NAIVE forecasts is not worth deploying.
# Benchmarks also provide important context: if SNAIVE already achieves
# MASE < 1, the seasonal naive method captures most of the predictable signal.

# MEAN()         — forecast = historical average (constant level)
# NAIVE()        — forecast = last observed value (random walk)
# SNAIVE()       — forecast = last observed value from same season
# RW(~ drift())  — random walk with drift (random walk + linear trend)

# Split butterfly data into training (2015-2022) and test (2023-2024)
train_butterfly_ts <- butterfly_ts %>%
  dplyr::filter(year(date) <= 2022)

test_butterfly_ts <- butterfly_ts %>%
  dplyr::filter(year(date) > 2022)

# Fit all four benchmark models to the training data
benchmark_fit <- train_butterfly_ts %>%
  model(
    Mean           = MEAN(abundance),
    Naive          = NAIVE(abundance),
    `Seasonal Naive` = SNAIVE(abundance),
    Drift          = RW(abundance ~ drift())
  )

print_output(benchmark_fit, "Benchmark models fitted to training data")

# Generate forecasts for the test period (24 months)
benchmark_forecasts <- benchmark_fit %>%
  forecast(h = FORECAST_HORIZON_LONG)

# Plot forecasts vs actual observations
benchmark_forecasts %>%
  autoplot(butterfly_ts, level = NULL) +
  theme_new() +
  scale_colour_manual(values = pallete) +
  labs(title    = "Benchmark Forecasts vs Actual Butterfly Abundance",
       subtitle = "SNAIVE should best capture the seasonal pattern",
       y        = "Abundance (count)",
       colour   = "Method")

# Interpretation: SNAIVE (Seasonal Naive) performs best because the series
# has a strong seasonal pattern. MEAN produces a flat line ignoring all
# temporal structure. NAIVE just repeats the last value. Drift extrapolates
# the linear trend but ignores seasonality.

################################################################################
############### Example 4: Benchmark Models on Beer Production #################
################################################################################

# Apply the same benchmark exercise to quarterly data, demonstrating that
# the SNAIVE advantage is consistent across different ecological time scales
# and data sources whenever a strong seasonal pattern dominates.

train_beer_ts <- aus_production %>%
  filter_index("1992 Q1" ~ "2006 Q4")

beer_benchmark_fit <- train_beer_ts %>%
  model(
    Mean           = MEAN(Beer),
    Naive          = NAIVE(Beer),
    `Seasonal Naive` = SNAIVE(Beer),
    Drift          = RW(Beer ~ drift())
  )

beer_benchmark_forecasts <- beer_benchmark_fit %>%
  forecast(h = 14)

# Plot with the true values overlaid as a dashed line
beer_benchmark_forecasts %>%
  autoplot(data = train_beer_ts, level = NULL) +
  autolayer(filter_index(aus_production, "2007 Q1" ~ .),
            colour = "black", linetype = "dashed") +
  theme_new() +
  scale_colour_manual(values = pallete) +
  labs(y        = "Megalitres",
       title    = "Forecasts for Quarterly Beer Production",
       subtitle = "Dashed line = actual values",
       colour   = "Method")

################################################################################
############### Example 5: Residual Diagnostics ################################
################################################################################

# A model is only adequate if its residuals are white noise (unpredictable
# random variation with no remaining structure). Good residuals must be:
#   1. Uncorrelated (all ACF spikes within the blue bounds)
#   2. Zero mean (no systematic bias)
#   3. Constant variance (homoscedastic)
#   4. Approximately normally distributed (for valid prediction intervals)
#
# gg_tsresiduals() provides a 3-panel diagnostic in one function call:
#   Top panel:     residual time plot (check for patterns and changing variance)
#   Bottom-left:   ACF of residuals (check for autocorrelation)
#   Bottom-right:  histogram of residuals (check for normality)

# Diagnostics for Seasonal Naive on butterfly data
train_butterfly_ts %>%
  model(SNAIVE(abundance)) %>%
  gg_tsresiduals() +
  theme_new() +
  labs(title = "Residual Diagnostics: SNAIVE on Butterfly Abundance")

# Interpretation: If significant spikes appear in the ACF, the residuals are
# autocorrelated → the model has missed some temporal structure → try a
# more complex model. A non-normal histogram suggests the model may
# underestimate the probability of extreme abundance events.

# Formal Ljung-Box test for residual autocorrelation
# H0: residuals are white noise (no autocorrelation up to lag h)
# Small p-value → residuals are autocorrelated → model is inadequate
snaive_augmented <- train_butterfly_ts %>%
  model(SNAIVE(abundance)) %>%
  augment()

snaive_augmented %>%
  features(.innov, ljung_box, lag = 24, dof = 0) %>%
  print_output("Ljung-Box test: SNAIVE residuals")

# For an adequate model, we want p-value > STATIONARITY_ALPHA (0.05).

################################################################################
############### Example 6: Accuracy Metrics ####################################
################################################################################

# How do we compare models objectively? Using accuracy metrics on the
# held-out TEST set (never the training set — overfitting is real!):
#   RMSE = sqrt(mean((y - yhat)^2))      — penalises large errors more
#   MAE  = mean(|y - yhat|)              — all errors weighted equally
#   MAPE = mean(|y - yhat| / |y|) * 100  — percentage error (problematic when y ≈ 0)
#   MASE = MAE / MAE_naive               — scaled error: MASE < 1 beats naive
#
# MASE is strongly recommended for ecological time series because it is
# scale-free and interpretable: MASE > 1 means the model is worse than naive.

# Compare all benchmark models on the butterfly test data
benchmark_accuracy <- accuracy(benchmark_forecasts, butterfly_ts)

print_output(benchmark_accuracy %>%
               dplyr::select(.model, RMSE, MAE, MAPE, MASE) %>%
               dplyr::arrange(RMSE),
             "Benchmark Accuracy: Butterfly Abundance (test set)")

# Compare benchmark models on beer test data
beer_recent_ts <- aus_production %>%
  dplyr::filter(year(Quarter) >= 1992)

beer_benchmark_accuracy <- accuracy(beer_benchmark_forecasts, beer_recent_ts)

print_output(beer_benchmark_accuracy %>%
               dplyr::select(.model, RMSE, MAE, MAPE, MASE) %>%
               dplyr::arrange(RMSE),
             "Benchmark Accuracy: Beer Production (test set)")

################################################################################
############### Example 7: Fitted Values and Cross-Validation ##################
################################################################################

# We can also examine in-sample fit using augment() to extract fitted values.
# However, in-sample accuracy is optimistic — the model saw this data during fitting.
# Cross-validation (stretch_tsibble) is more reliable because it evaluates the
# model at many different time points using expanding training windows.

# Extract fitted values from two benchmark models
augmented_butterfly <- train_butterfly_ts %>%
  model(
    `Seasonal Naive` = SNAIVE(abundance),
    Drift            = RW(abundance ~ drift())
  ) %>%
  augment()

# Plot fitted values vs actual
augmented_butterfly %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = abundance), colour = "black") +
  geom_line(aes(y = .fitted, colour = .model), alpha = 0.7) +
  scale_colour_manual(values = pallete) +
  theme_new() +
  labs(title  = "Fitted Values vs Actual — Training Data",
       y      = "Abundance (count)",
       colour = "Model")

# Time series cross-validation using stretch_tsibble
# Creates expanding training windows: we start with CV_INIT_WINDOW (48) months
# of data and then evaluate 1-step-ahead forecasts for every subsequent month.
butterfly_cv_forecasts <- butterfly_ts %>%
  stretch_tsibble(.init = CV_INIT_WINDOW) %>%
  model(
    SNAIVE = SNAIVE(abundance),
    Naive  = NAIVE(abundance)
  ) %>%
  forecast(h = 1)

butterfly_cv_forecasts %>%
  accuracy(butterfly_ts) %>%
  dplyr::select(.model, RMSE, MAE, MASE) %>%
  dplyr::arrange(RMSE) %>%
  print_output("Cross-Validated Accuracy: 1-step ahead")

# Cross-validation is more trustworthy than a single train/test split
# because it averages performance over many time points rather than
# depending on one particular (possibly easy or difficult) test window.

################################################################################
############### Example 8: Complete Box-Jenkins Workflow ########################
################################################################################

# Let's walk through the full Box-Jenkins workflow on Australian beer data.
# This is the systematic process to follow for any new ecological time series.

# Step 1: VISUALISE — always start by looking at the series
autoplot(beer_ts, Beer) +
  theme_new() +
  labs(title = "Step 1: Visualise — Beer Production")

# Step 2: CHECK STATIONARITY — determine what transformations are needed
beer_ts %>%
  features(Beer, unitroot_kpss) %>%
  print_output("Step 2: KPSS test")

beer_ts %>%
  features(Beer, unitroot_nsdiffs) %>%
  print_output("Step 2: Seasonal differences needed")

# Step 3: TRANSFORM if needed — use Guerrero to select lambda
lambda_beer_workflow <- beer_ts %>%
  features(Beer, guerrero) %>%
  dplyr::pull(lambda_guerrero)
cat("Step 3: Optimal lambda =", round(lambda_beer_workflow, 3), "\n")

# Step 4: FIT benchmark models — establish the baseline to beat
beer_benchmark_workflow <- beer_ts %>%
  model(
    SNAIVE = SNAIVE(Beer),
    Mean   = MEAN(Beer)
  )

# Step 5: DIAGNOSE residuals — check if a better model is needed
beer_benchmark_workflow %>%
  dplyr::select(SNAIVE) %>%
  gg_tsresiduals() +
  labs(title = "Step 5: SNAIVE Residual Diagnostics")

# Step 6: CHECK accuracy — use in-sample metrics as a starting reference
accuracy(beer_benchmark_workflow) %>%
  dplyr::select(.model, RMSE, MAE, MASE) %>%
  print_output("Step 6: Training accuracy")

# In the next scripts, we replace these benchmarks with ARIMA and ETS models
# and test whether they can achieve lower RMSE on a held-out test set.

################################################################################
############# Modelling Ecological Time Series Exercises ########################
################################################################################

# Exercise 1: Box-Cox transformation selection for wetland birds
# Using "wetland_bird_counts.csv" (total_waterbirds column):
# (a) Find the optimal Box-Cox lambda using Guerrero's method.
# (b) Plot the original and transformed series side by side.
# (c) Based on the lambda value, which transformation is recommended?
#     (near 0 → log; near 0.5 → sqrt; near 1 → no transform)
# (d) Does the transformation visually stabilise the seasonal variance?

# Exercise 2: Benchmark models for river dissolved oxygen
# Using "river_water_quality.csv":
# (a) Split into training (year <= 2023) and test (year > 2023).
# (b) Fit MEAN, NAIVE, and RW with drift benchmark models.
# (c) Forecast for the test period and plot with actual values overlaid.
# (d) Evaluate accuracy. Why might SNAIVE be inappropriate for daily data
#     without explicitly specifying the seasonal period?

# Exercise 3: Residual diagnostics comparison across benchmarks
# Using the butterfly data (training set: year <= 2022):
# (a) Fit MEAN, NAIVE, SNAIVE, and Drift models.
# (b) Run gg_tsresiduals() on each model (four separate plots).
# (c) Perform the Ljung-Box test on each model's residuals (lag = 24).
# (d) Which model has the best residuals (closest to white noise)?
#     What does this imply about which model to develop further?

# Exercise 4: Accuracy metrics on forest canopy cover
# Using "forest_canopy_cover.csv" for "Sitka_Spruce" only:
# (a) Create a quarterly tsibble and split (train: year <= 2022, test: > 2022).
# (b) Fit all four benchmark models.
# (c) Compute RMSE, MAE, and MASE on the test set.
# (d) Which model wins? Why might NAIVE or MEAN perform well for Sitka Spruce
#     given its ecological characteristics (stable evergreen canopy)?

# Exercise 5: Cross-validation versus a single train/test split
# Using the butterfly abundance data:
# (a) Compute SNAIVE accuracy using a single split (train: <= 2020, test: > 2020).
# (b) Compute SNAIVE accuracy using stretch_tsibble (.init = 36 months).
# (c) Compare the RMSE from both approaches.
# (d) Which estimate is more reliable and why? (Consider: what if the test
#     period happened to be unusually easy or difficult to forecast?)

################################################################################
############# Modelling Ecological Time Series — Advanced #####################
################################################################################
# Advanced examples extend the Box-Jenkins workflow with time series
# cross-validation, multi-model dashboards, and information-theoretic
# model selection applied to real ecological monitoring data.

# Advanced Example 1: Comprehensive benchmark comparison on river data
river_adv_ts <- load_ecological_tsibble(
  file_path  = "input_data/river_water_quality.csv",
  index_col  = "date",
  index_type = "date"
)
train_river_adv <- river_adv_ts %>% dplyr::filter(year(date) <= 2023)
test_river_adv  <- river_adv_ts %>% dplyr::filter(year(date) > 2023)

river_bench_fit <- train_river_adv %>%
  model(
    Mean  = MEAN(dissolved_oxygen_mgl),
    Naive = NAIVE(dissolved_oxygen_mgl),
    Drift = RW(dissolved_oxygen_mgl ~ drift())
  )

accuracy(river_bench_fit %>%
           forecast(h = nrow(test_river_adv)),
         river_adv_ts) %>%
  dplyr::select(.model, RMSE, MAE, MASE) %>%
  dplyr::arrange(RMSE) %>%
  print_output("Advanced Example 1: River DO Benchmark Accuracy")

# Advanced Example 2: Residual correlation heatmap across models
bf_adv_fit <- butterfly_ts %>%
  dplyr::filter(year(date) <= 2022) %>%
  model(
    Mean   = MEAN(abundance),
    Naive  = NAIVE(abundance),
    SNAIVE = SNAIVE(abundance),
    Drift  = RW(abundance ~ drift())
  )

augment(bf_adv_fit) %>%
  dplyr::group_by(.model) %>%
  features(.innov, ljung_box, lag = 24) %>%
  dplyr::arrange(lb_pvalue) %>%
  print_output("Advanced Example 2: Ljung-Box p-values sorted by adequacy")

# Advanced Example 3: Time series CV with multiple horizons
bf_cv_multi <- butterfly_ts %>%
  stretch_tsibble(.init = CV_INIT_WINDOW) %>%
  model(SNAIVE = SNAIVE(abundance)) %>%
  forecast(h = c(1, 6, 12))

bf_cv_multi %>%
  accuracy(butterfly_ts) %>%
  dplyr::select(.model, .h, RMSE, MAE) %>%
  dplyr::arrange(.h) %>%
  print_output("Advanced Example 3: CV Accuracy at Horizons 1, 6, 12 months")

# Advanced Example 4: Box-Cox impact on residual diagnostics
lambda_bf_adv <- butterfly_ts %>%
  features(abundance, guerrero) %>%
  dplyr::pull(lambda_guerrero)

bf_bc_fit <- butterfly_ts %>%
  dplyr::filter(year(date) <= 2022) %>%
  model(
    SNAIVE_raw = SNAIVE(abundance),
    SNAIVE_bc  = SNAIVE(box_cox(abundance, lambda_bf_adv))
  )

bf_bc_fit %>% dplyr::select(SNAIVE_raw) %>%
  gg_tsresiduals() + labs(title = "Advanced Example 4a: SNAIVE Raw")
bf_bc_fit %>% dplyr::select(SNAIVE_bc) %>%
  gg_tsresiduals() + labs(title = "Advanced Example 4b: SNAIVE Box-Cox")

# Advanced Example 5: Systematic model selection pipeline
run_benchmark_pipeline <- function(training_ts, test_ts, full_ts,
                                   value_col, horizon) {
  # Args:
  #   training_ts : tsibble — training data
  #   test_ts     : tsibble — test data
  #   full_ts     : tsibble — full series for accuracy evaluation
  #   value_col   : character — column to forecast
  #   horizon     : integer — forecast horizon
  # Returns: accuracy table sorted by RMSE

  bench_fit <- training_ts %>%
    model(
      Mean   = MEAN(!!rlang::sym(value_col)),
      Naive  = NAIVE(!!rlang::sym(value_col)),
      SNAIVE = SNAIVE(!!rlang::sym(value_col)),
      Drift  = RW(!!rlang::sym(value_col) ~ drift())
    )

  bench_fc <- bench_fit %>% forecast(h = horizon)

  accuracy(bench_fc, full_ts) %>%
    dplyr::select(.model, RMSE, MAE, MASE) %>%
    dplyr::arrange(RMSE)
}

run_benchmark_pipeline(
  training_ts = butterfly_ts %>% dplyr::filter(year(date) <= 2022),
  test_ts     = butterfly_ts %>% dplyr::filter(year(date) > 2022),
  full_ts     = butterfly_ts,
  value_col   = "abundance",
  horizon     = 24
) %>%
  print_output("Advanced Example 5: Systematic Benchmark Pipeline")

################################################################################
########### Modelling Ecological Time Series — Advanced Exercises ##############
################################################################################

# Advanced Exercise 1: Red deer population — complete workflow
# Using the red_deer_population tsibble from script 01:
# (a) Check stationarity and determine differencing needed.
# (b) Find the optimal Box-Cox lambda.
# (c) Fit MEAN, NAIVE, and Drift benchmarks.
# (d) Which benchmark is most appropriate for a slowly growing population?

# Advanced Exercise 2: Ljung-Box degrees of freedom
# When performing the Ljung-Box test on ARIMA residuals, we subtract the
# number of estimated parameters from the degrees of freedom (dof argument).
# Using the beer SNAIVE benchmark:
# (a) Run the test with dof = 0 (SNAIVE has no estimated parameters).
# (b) Run the test with dof = 1 (hypothetically, if the model had one parameter).
# (c) How does the p-value change? Why does accounting for dof matter?

# Advanced Exercise 3: Cross-validation horizons for forest canopy
# Using the Oak_Woodland forest canopy series:
# (a) Run stretch_tsibble CV (.init = 20 quarters) for SNAIVE.
# (b) Forecast at horizons h = 1, 2, 4 (1 quarter, 2 quarters, 1 year).
# (c) How does RMSE change with increasing horizon?
# (d) Is the 1-year ahead forecast much worse than the 1-quarter ahead?

# Advanced Exercise 4: apply run_benchmark_pipeline() to wetland data
# Use the run_benchmark_pipeline() function from Advanced Example 5:
# (a) Apply it to wetland total_waterbirds with horizon = 52 (1 year).
# (b) Which benchmark wins? Is this the same winner as for butterfly data?
# (c) What does this consistency (or difference) tell us about the
#     predictability of different ecological time series?

# Advanced Exercise 5: Residual normality testing
# For the butterfly SNAIVE model:
# (a) Extract the innovation residuals using augment().
# (b) Perform a Shapiro-Wilk normality test on the residuals.
# (c) Create a Q-Q plot of the residuals.
# (d) Are the residuals normal? If not, what implication does this have
#     for the coverage of the 95% prediction intervals?

################################################################################
############# Modelling Ecological Time Series — Answers ########################
################################################################################

# Answer 1:
wetland_ts <- load_ecological_tsibble(
  file_path  = "wetland_bird_counts.csv",
  index_col  = "date",
  index_type = "yearweek"
)

# (a) Optimal lambda
lambda_wetland <- wetland_ts %>%
  features(total_waterbirds, guerrero) %>%
  dplyr::pull(lambda_guerrero)
cat("Optimal lambda for wetland birds:", round(lambda_wetland, 3), "\n")

# (b) Side-by-side plots
plot_wetland_raw <- autoplot(wetland_ts, total_waterbirds) +
  theme_new() +
  labs(title = "Original", y = "Count")

plot_wetland_bc <- wetland_ts %>%
  dplyr::mutate(bc_count = box_cox(total_waterbirds, lambda_wetland)) %>%
  autoplot(bc_count) +
  theme_new() +
  labs(title = paste0("Box-Cox (lambda = ", round(lambda_wetland, 2), ")"),
       y     = "Transformed")

plot_wetland_raw / plot_wetland_bc

# (c) Interpretation based on lambda value
if (lambda_wetland < BOXCOX_NEAR_ZERO) {
  cat("Recommendation: log transform (lambda near 0)\n")
} else if (lambda_wetland < BOXCOX_NEAR_HALF) {
  cat("Recommendation: square root transform (lambda near 0.5)\n")
} else {
  cat("Recommendation: no transformation needed (lambda near 1)\n")
}

# (d) The transformation should make seasonal amplitude more uniform
# across high-count (winter) and low-count (summer) periods.

# Answer 2:
river_ts <- load_ecological_tsibble(
  file_path  = "river_water_quality.csv",
  index_col  = "date",
  index_type = "date"
)

train_river_ts <- river_ts %>% dplyr::filter(year(date) <= 2023)
test_river_ts  <- river_ts %>% dplyr::filter(year(date) > 2023)

# (b) Fit benchmarks
river_benchmark_fit <- train_river_ts %>%
  model(
    Mean  = MEAN(dissolved_oxygen_mgl),
    Naive = NAIVE(dissolved_oxygen_mgl),
    Drift = RW(dissolved_oxygen_mgl ~ drift())
  )

# (c) Forecast and plot
river_benchmark_forecasts <- river_benchmark_fit %>%
  forecast(h = nrow(test_river_ts))

river_benchmark_forecasts %>%
  autoplot(river_ts, level = NULL) +
  theme_new() +
  scale_colour_manual(values = pallete) +
  labs(title  = "Benchmark Forecasts — River DO",
       y      = "DO (mg/L)",
       colour = "Method")

# (d) Accuracy
accuracy(river_benchmark_forecasts, river_ts) %>%
  dplyr::select(.model, RMSE, MAE, MASE) %>%
  dplyr::arrange(RMSE) %>%
  print_output("River DO benchmark accuracy")
# SNAIVE is inappropriate here because the tsibble uses a Date index (period = 1 day).
# The fable software cannot automatically determine the annual period = 365 days.
# We would need SNAIVE(dissolved_oxygen_mgl ~ lag(365)) to specify it manually.

# Answer 3:
train_bf_ts <- butterfly_ts %>% dplyr::filter(year(date) <= 2022)

bf_benchmark_fit <- train_bf_ts %>%
  model(
    Mean           = MEAN(abundance),
    Naive          = NAIVE(abundance),
    SNAIVE         = SNAIVE(abundance),
    Drift          = RW(abundance ~ drift())
  )

# (b) Residual diagnostics (examine each model separately)
bf_benchmark_fit %>% dplyr::select(Mean)   %>% gg_tsresiduals()
bf_benchmark_fit %>% dplyr::select(Naive)  %>% gg_tsresiduals()
bf_benchmark_fit %>% dplyr::select(SNAIVE) %>% gg_tsresiduals()
bf_benchmark_fit %>% dplyr::select(Drift)  %>% gg_tsresiduals()

# (c) Ljung-Box tests for all benchmark models
augment(bf_benchmark_fit) %>%
  dplyr::group_by(.model) %>%
  features(.innov, ljung_box, lag = 24) %>%
  print_output("Ljung-Box tests for all benchmark models")

# (d) SNAIVE typically has the best (least autocorrelated) residuals for seasonal
# data because it accounts for the seasonal pattern, leaving less structure.
# If even SNAIVE residuals are autocorrelated, an ARIMA or ETS model is needed.

# Answer 4:
spruce_ts <- load_ecological_tsibble(
  file_path  = "forest_canopy_cover.csv",
  index_col  = "date",
  index_type = "yearquarter",
  key_col    = "forest_type"
) %>%
  dplyr::filter(forest_type == "Sitka_Spruce")

train_spruce_ts <- spruce_ts %>% dplyr::filter(year(date) <= 2022)
test_spruce_ts  <- spruce_ts %>% dplyr::filter(year(date) > 2022)

# (b) Fit all four benchmarks
spruce_benchmark_fit <- train_spruce_ts %>%
  model(
    Mean           = MEAN(canopy_cover_pct),
    Naive          = NAIVE(canopy_cover_pct),
    `Seasonal Naive` = SNAIVE(canopy_cover_pct),
    Drift          = RW(canopy_cover_pct ~ drift())
  )

# (c) Forecast accuracy on test set
spruce_benchmark_forecasts <- spruce_benchmark_fit %>%
  forecast(h = nrow(test_spruce_ts))

accuracy(spruce_benchmark_forecasts, spruce_ts) %>%
  dplyr::select(.model, RMSE, MAE, MASE) %>%
  dplyr::arrange(RMSE) %>%
  print_output("Sitka Spruce benchmark accuracy")

# (d) NAIVE or MEAN may perform well because Sitka Spruce (an evergreen conifer)
# has minimal seasonal canopy change and slow, steady growth. The "best"
# forecast is simply "about the same as now" — which NAIVE captures perfectly.

# Answer 5:
train_single_ts <- butterfly_ts %>% dplyr::filter(year(date) <= 2020)
test_single_ts  <- butterfly_ts %>% dplyr::filter(year(date) > 2020)

# (a) Single split
fc_single_split <- train_single_ts %>%
  model(SNAIVE = SNAIVE(abundance)) %>%
  forecast(h = nrow(test_single_ts))

accuracy(fc_single_split, butterfly_ts) %>%
  dplyr::select(.model, RMSE) %>%
  print_output("Single Split RMSE")

# (b) Cross-validation
fc_cross_valid <- butterfly_ts %>%
  stretch_tsibble(.init = 36) %>%
  model(SNAIVE = SNAIVE(abundance)) %>%
  forecast(h = 1)

fc_cross_valid %>%
  accuracy(butterfly_ts) %>%
  dplyr::select(.model, RMSE) %>%
  print_output("Cross-Validated RMSE")

# (d) The cross-validated RMSE is more reliable because it averages across
# many different starting points, reducing the chance that the result is
# biased by a particularly easy or hard test window.

# Answer 6:
red_deer_ts <- tsibble(
  Year  = 2010:2024,
  Count = c(120, 135, 142, 155, 148, 163, 171, 180, 175, 190,
            198, 205, 215, 220, 228),
  index = Year
)

# (a) Check stationarity
red_deer_ts %>%
  features(Count, unitroot_kpss) %>%
  print_output("KPSS: Red Deer Population")

red_deer_ts %>%
  features(Count, unitroot_ndiffs) %>%
  print_output("Differences needed: Red Deer")

# (b) Box-Cox lambda
lambda_deer <- red_deer_ts %>%
  features(Count, guerrero) %>%
  dplyr::pull(lambda_guerrero)
cat("Optimal lambda for red deer:", round(lambda_deer, 3), "\n")

# (c) Fit benchmarks
red_deer_benchmark_fit <- red_deer_ts %>%
  model(
    Mean  = MEAN(Count),
    Naive = NAIVE(Count),
    Drift = RW(Count ~ drift())
  )

# (d) For a steadily growing population, Drift (random walk with trend)
# is most appropriate because it extrapolates the historical growth rate.

# Answer 7:
# (a) SNAIVE with dof = 0 (no estimated parameters)
train_bf_ts %>%
  model(SNAIVE = SNAIVE(abundance)) %>%
  augment() %>%
  features(.innov, ljung_box, lag = 24, dof = 0) %>%
  print_output("Ljung-Box SNAIVE: dof = 0")

# (b) Hypothetical dof = 1
train_bf_ts %>%
  model(SNAIVE = SNAIVE(abundance)) %>%
  augment() %>%
  features(.innov, ljung_box, lag = 24, dof = 1) %>%
  print_output("Ljung-Box SNAIVE: dof = 1")

# (c) Adjusting for degrees of freedom increases the p-value slightly, because
# we are being more conservative about calling residuals non-white-noise when
# the model already used some degrees of freedom to estimate parameters.
# For ARIMA models with p+q parameters, always set dof = p + q.
