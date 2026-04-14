####################################################################################################
###
### File:    04_Classical_Models.R
### Purpose: Examples and exercises for Classical Models (AR, MA, ARIMA)
### Authors: Gabriel Rodrigues Palma
### Date:    17/06/25
###
####################################################################################################
# load packages -----
source('00_source.r')

################################################################################
######################## Classical Models (AR, MA, ARIMA) ######################
################################################################################
# "AR models say the future depends on the past.
#  MA models say the future depends on past surprises.
#  ARIMA combines both with differencing to handle non-stationarity."
#
# The three fundamental building blocks:
#   AR(p):   y_t = c + phi_1*y_{t-1} + ... + phi_p*y_{t-p} + eps_t
#   MA(q):   y_t = c + eps_t + theta_1*eps_{t-1} + ... + theta_q*eps_{t-q}
#   ARIMA(p,d,q): differenced d times, then ARMA(p,q) on the stationary result
#
# Backshift operator notation (B = backward shift):
#   B*y_t = y_{t-1},  B^k*y_t = y_{t-k}
#   (1 - phi_1*B - ... - phi_p*B^p)(1-B)^d * y_t =
#     c + (1 + theta_1*B + ... + theta_q*B^q) * eps_t
#
# ACF/PACF pattern recognition (the core identification skill):
#   AR(p): ACF decays exponentially; PACF cuts off sharply after lag p
#   MA(q): ACF cuts off sharply after lag q; PACF decays exponentially
#   ARMA:  Both ACF and PACF decay exponentially (no sharp cutoff)

################################################################################
############### Example 1: Simulating an AR(1) Process #########################
################################################################################

# AR(1) model: y_t = c + phi * y_{t-1} + eps_t
# The parameter phi controls the "memory" of the process:
#   |phi| < 1 required for stationarity
#   phi > 0: positive autocorrelation (smooth, persistent process — like
#     a population that stays high when it was recently high)
#   phi < 0: alternating sign pattern (each observation reverses direction)

set.seed(2026)
n_sim <- 500
innovations <- rnorm(n_sim, sd = 1)
ar1_values <- numeric(n_sim)

# Simulate AR(1) with phi = 0.7 (ecological context: monthly abundance with
# strong persistence — a good month tends to be followed by a good month)
phi_ar1    <- 0.7
intercept_ar1 <- 5
ar1_values[1] <- innovations[1]
for (time_step in 2:n_sim) {
  ar1_values[time_step] <- intercept_ar1 + phi_ar1 * ar1_values[time_step - 1] +
    innovations[time_step]
}

ar1_ts <- tsibble(Time = 1:n_sim, Value = ar1_values, index = Time)

# Time plot — smooth persistence is visible
plot_ar1 <- autoplot(ar1_ts, Value) +
  theme_new() +
  labs(title    = "Simulated AR(1) Process (phi = 0.7)",
       subtitle = "Ecological analogy: population with strong month-to-month persistence",
       y        = "Simulated Abundance Index")

print(plot_ar1)

# ACF and PACF — the KEY diagnostic tool for model identification
# AR(p) signature:
#   - ACF: exponential decay (gradually decreasing bars)
#   - PACF: sharp cutoff after lag p (only 1 significant bar for AR(1))
par(mfrow = c(2, 1))
acf(ar1_values, lag.max = 20, main = "ACF of AR(1) — Exponential Decay")
pacf(ar1_values, lag.max = 20, main = "PACF of AR(1) — Cuts Off After Lag 1")
par(mfrow = c(1, 1))

# Interpretation: The ACF decays exponentially — classic AR(1) signature.
# The PACF has exactly one significant spike (lag 1) then cuts off → AR(1).
# This is how we identify the AR order from real ecological time series data.

################################################################################
############### Example 2: Simulating an AR(2) Process #########################
################################################################################

# AR(2): y_t = c + phi_1*y_{t-1} + phi_2*y_{t-2} + eps_t
# Ecological context: predator-prey dynamics where abundance depends on
# the previous TWO time steps — prey builds up and predator follows with a lag.
# The PACF will show exactly 2 significant spikes, then cut off.

ar2_values <- numeric(n_sim)
ar2_values[1:2] <- innovations[1:2]
for (time_step in 3:n_sim) {
  ar2_values[time_step] <- 5 + 0.3 * ar2_values[time_step - 1] +
    0.5 * ar2_values[time_step - 2] + innovations[time_step]
}

par(mfrow = c(2, 1))
acf(ar2_values, lag.max = 20, main = "ACF of AR(2) — Exponential Decay")
pacf(ar2_values, lag.max = 20, main = "PACF of AR(2) — Cuts Off After Lag 2")
par(mfrow = c(1, 1))

# Interpretation: PACF has two significant spikes (lags 1 and 2), then cuts off
# → AR(2). The ACF decays more slowly than AR(1) because there are two lag
# dependencies contributing to persistence.

################################################################################
############### Example 3: Simulating an MA(1) Process #########################
################################################################################

# MA(1): y_t = c + eps_t + theta*eps_{t-1}
# The current value depends on the CURRENT shock AND the PREVIOUS shock.
# Ecological analogy: a sudden cold snap (shock) affects this month's
# butterfly count (eps_t) AND next month's count (theta * eps_{t-1}).
# After that, the effect is completely gone — finite memory!

ma1_values <- numeric(n_sim)
theta_ma1  <- 0.8
ma1_values[1] <- innovations[1]
for (time_step in 2:n_sim) {
  ma1_values[time_step] <- 5 + innovations[time_step] +
    theta_ma1 * innovations[time_step - 1]
}

# MA(q) signature — THE OPPOSITE of AR:
#   - ACF: SHARP cutoff after lag q (only 1 bar for MA(1))
#   - PACF: exponential decay (gradually decreasing bars)
par(mfrow = c(2, 1))
acf(ma1_values, lag.max = 20, main = "ACF of MA(1) — Cuts Off After Lag 1")
pacf(ma1_values, lag.max = 20, main = "PACF of MA(1) — Exponential Decay")
par(mfrow = c(1, 1))

# Interpretation: The ACF has exactly one significant spike at lag 1, then
# cuts off completely → MA(1). The PACF shows exponential decay. Contrast
# this with AR(1) where the roles of ACF and PACF are swapped.

################################################################################
############### Example 4: Simulating an MA(2) Process #########################
################################################################################

# MA(2): y_t = c + eps_t + theta_1*eps_{t-1} + theta_2*eps_{t-2}
# Two consecutive shocks affect the current value, after which the series
# has no memory. This is appropriate for series where events have a brief
# 2-month lingering effect (e.g., a pest outbreak's impact on vegetation).

ma2_values <- numeric(n_sim)
ma2_values[1:2] <- innovations[1:2]
for (time_step in 3:n_sim) {
  ma2_values[time_step] <- 5 + innovations[time_step] +
    0.8 * innovations[time_step - 1] + 0.5 * innovations[time_step - 2]
}

par(mfrow = c(2, 1))
acf(ma2_values, lag.max = 20, main = "ACF of MA(2) — Cuts Off After Lag 2")
pacf(ma2_values, lag.max = 20, main = "PACF of MA(2) — Exponential Decay")
par(mfrow = c(1, 1))

# Summary of ACF/PACF identification table:
# +-----------+-----------------------------+---------------------------+
# | Model     | ACF                         | PACF                      |
# +-----------+-----------------------------+---------------------------+
# | AR(p)     | Exponential decay           | Cutoff after lag p        |
# | MA(q)     | Cutoff after lag q          | Exponential decay         |
# | ARMA(p,q) | Exponential decay           | Exponential decay         |
# | I(1)      | Very slow decay (near 1)    | One large spike at lag 1  |
# +-----------+-----------------------------+---------------------------+

################################################################################
############### Example 5: Random Walk — I(1) ##################################
################################################################################

# A random walk is an I(1) process: y_t = y_{t-1} + eps_t
# This is a non-stationary process where the best forecast is always the
# last observed value. In ecology, relevant for:
# - Populations under pure genetic/demographic drift (neutral theory)
# - Species range boundaries shifting randomly under climate variability
# - Contaminant concentrations with no systematic restoring force
# The ACF of a random walk decays VERY slowly — nearly all bars are significant.

rw_values <- numeric(n_sim)
rw_values[1] <- innovations[1]
for (time_step in 2:n_sim) {
  rw_values[time_step] <- rw_values[time_step - 1] + innovations[time_step]
}

rw_ts <- tsibble(Time = 1:n_sim, Value = rw_values, index = Time)

autoplot(rw_ts, Value) +
  theme_new() +
  labs(title    = "Random Walk — Non-Stationary (I(1))",
       subtitle = "Differencing once makes it stationary (white noise)",
       y        = "Simulated Population Index")

par(mfrow = c(2, 1))
acf(rw_values, lag.max = 20, main = "ACF of Random Walk — Very Slow Decay")
pacf(rw_values, lag.max = 20, main = "PACF of Random Walk — One Large Spike")
par(mfrow = c(1, 1))

# After differencing once, we recover white noise — confirming I(1)
par(mfrow = c(2, 1))
acf(diff(rw_values), lag.max = 20,
    main = "ACF of Differenced Random Walk — White Noise")
pacf(diff(rw_values), lag.max = 20,
     main = "PACF of Differenced Random Walk — White Noise")
par(mfrow = c(1, 1))

################################################################################
############### Example 6: Fitting ARIMA to Irish Exports ######################
################################################################################

# Now we apply ARIMA to real data using the fable package.
# fable's ARIMA() function can:
#   (a) Automatically select the best (p,d,q) order — stepwise or exhaustive search
#   (b) Allow manual specification using pdq() notation
# We compare manual specifications against the automatic selection
# using AICc (information criterion: lower = better, penalises complexity).

ireland_exports_ts <- global_economy %>%
  dplyr::filter(Code == "IRL")

# Visualise the series to motivate the differencing choice
autoplot(ireland_exports_ts, Exports) +
  theme_new() +
  labs(title = "Irish Exports (% of GDP, 1960-2017)",
       y     = "% of GDP")

# ACF/PACF of the first-differenced series to identify AR/MA orders
ireland_exports_ts %>%
  gg_tsdisplay(difference(Exports), plot_type = "partial") +
  theme_new() +
  labs(title = "ACF/PACF of First-Differenced Irish Exports")

# Fit multiple manual ARIMA specifications + automatic selection
ireland_arima_fit <- ireland_exports_ts %>%
  model(
    arima110   = ARIMA(Exports ~ 0 + pdq(1, 1, 0)),  # AR(1) after first-diff
    arima011   = ARIMA(Exports ~ 0 + pdq(0, 1, 1)),  # MA(1) after first-diff
    stepwise   = ARIMA(Exports),                       # automatic (fast)
    search     = ARIMA(Exports, stepwise = FALSE)      # automatic (exhaustive)
  )

# Compare using AICc — lower AICc = better model (penalises complexity)
# AICc = AIC + 2k(k+1) / (T - k - 1), corrected for small sample sizes
glance(ireland_arima_fit) %>%
  dplyr::arrange(AICc) %>%
  dplyr::select(.model, AICc, AIC, BIC) %>%
  print_output("Model comparison by AICc: Irish Exports")

# Report the best model (from exhaustive search)
ireland_arima_fit %>%
  dplyr::select(search) %>%
  report()

# Residual diagnostics on the best model
ireland_arima_fit %>%
  dplyr::select(search) %>%
  gg_tsresiduals() +
  labs(title = "Residual Diagnostics: Best ARIMA for Irish Exports")

# Ljung-Box test on residuals (dof = number of ARMA parameters estimated)
augment(ireland_arima_fit) %>%
  dplyr::filter(.model == "search") %>%
  features(.innov, ljung_box, lag = 10, dof = 1) %>%
  print_output("Ljung-Box test: best ARIMA residuals")

# Forecast 10 years ahead with prediction intervals
ireland_arima_fit %>%
  dplyr::select(search) %>%
  forecast(h = 10) %>%
  autoplot(ireland_exports_ts) +
  theme_new() +
  labs(title = "ARIMA Forecast — Irish Exports",
       y     = "% of GDP")

################################################################################
############### Example 7: ARIMA on Ecological Butterfly Data ####################
################################################################################

# Apply ARIMA to our main ecological dataset — butterfly abundance.
# The automatic ARIMA will detect and handle seasonality by selecting an
# appropriate SARIMA(p,d,q)(P,D,Q)_12 specification.

butterfly_ts <- load_ecological_tsibble(
  file_path  = "input_data/butterfly_meadow_monitoring.csv",
  index_col  = "date",
  index_type = "yearmonth"
)

train_butterfly_ts <- butterfly_ts %>% dplyr::filter(year(date) <= 2022)
test_butterfly_ts  <- butterfly_ts %>% dplyr::filter(year(date) > 2022)

# Fit automatic ARIMA — with and without log transformation
butterfly_arima_fit <- train_butterfly_ts %>%
  model(
    auto_arima = ARIMA(abundance),
    arima_log  = ARIMA(log(abundance + 1))  # +1 avoids log(0) in winter months
  )

print_output(butterfly_arima_fit, "ARIMA models for butterfly abundance")

# Report the untransformed model
butterfly_arima_fit %>%
  dplyr::select(auto_arima) %>%
  report()

# Residual diagnostics
butterfly_arima_fit %>%
  dplyr::select(auto_arima) %>%
  gg_tsresiduals() +
  labs(title = "ARIMA Residuals — Butterfly Abundance")

# Compare ARIMA with SNAIVE and NAIVE benchmarks
butterfly_comparison_fc <- train_butterfly_ts %>%
  model(
    ARIMA          = ARIMA(abundance),
    `Seasonal Naive` = SNAIVE(abundance),
    Naive          = NAIVE(abundance)
  ) %>%
  forecast(h = 6)

butterfly_comparison_fc %>%
  autoplot(butterfly_ts, level = NULL) +
  theme_new() +
  scale_colour_manual(values = pallete) +
  labs(title    = "ARIMA vs Benchmarks — Butterfly Abundance Forecast",
       y        = "Abundance (count)",
       colour   = "Model")

# Accuracy comparison on test set
accuracy(butterfly_comparison_fc, butterfly_ts) %>%
  dplyr::select(.model, RMSE, MAE, MASE) %>%
  dplyr::arrange(RMSE) %>%
  print_output("ARIMA vs Benchmarks: Butterfly Forecast Accuracy")

################################################################################
############### Example 8: Seasonal ARIMA Introduction ########################
################################################################################

# Seasonal ARIMA notation: ARIMA(p,d,q)(P,D,Q)_m
# The seasonal part (P,D,Q)_m operates at the seasonal lag m:
#   p,d,q = non-seasonal AR order, differencing order, MA order
#   P,D,Q = seasonal AR order, seasonal differencing, seasonal MA order
#   m     = seasonal period (12 for monthly, 4 for quarterly, 52 for weekly)
#
# For monthly data with annual seasonality:
#   B^12 is the seasonal backshift operator
#   D = 1 means one seasonal difference: y_t - y_{t-12}

# US leisure and hospitality employment — strong monthly seasonality
leisure_employment_ts <- us_employment %>%
  dplyr::filter(Title == "Leisure and Hospitality",
                year(Month) > 2000) %>%
  dplyr::mutate(Employed = Employed / 1000) %>%
  dplyr::select(Month, Employed)

autoplot(leisure_employment_ts, Employed) +
  theme_new() +
  labs(title    = "US Leisure & Hospitality Employment",
       subtitle = "Strong monthly seasonality — needs seasonal ARIMA",
       y        = "People (millions)")

# ACF/PACF after seasonal differencing — to identify SARIMA orders
leisure_employment_ts %>%
  gg_tsdisplay(difference(Employed, 12),
               plot_type = "partial", lag = 36) +
  labs(title    = "Seasonally Differenced Employment",
       subtitle = "Use ACF/PACF to identify seasonal ARIMA orders")

# Fit multiple seasonal ARIMA specifications (manual + automatic)
leisure_arima_fit <- leisure_employment_ts %>%
  model(
    arima012011 = ARIMA(Employed ~ pdq(0, 1, 2) + PDQ(0, 1, 1)),
    arima210110 = ARIMA(Employed ~ pdq(2, 1, 0) + PDQ(1, 1, 0)),
    auto        = ARIMA(Employed, stepwise = FALSE, approx = FALSE)
  )

# Compare by AICc
glance(leisure_arima_fit) %>%
  dplyr::arrange(AICc) %>%
  dplyr::select(.model, AICc, AIC, BIC) %>%
  print_output("Seasonal ARIMA model comparison: Employment")

# Diagnostics on best model
leisure_arima_fit %>%
  dplyr::select(auto) %>%
  gg_tsresiduals(lag = 36) +
  labs(title = "Residual Diagnostics: Best Seasonal ARIMA")

# Forecast 3 years ahead
leisure_arima_fit %>%
  dplyr::select(auto) %>%
  forecast(h = 36) %>%
  autoplot(leisure_employment_ts) +
  theme_new() +
  labs(title = "Seasonal ARIMA Forecast — US Leisure Employment",
       y     = "People (millions)")

################################################################################
############### Example 9: Model Selection — AIC vs BIC ########################
################################################################################

# Information criteria for ARIMA model selection:
#   AIC  = -2*log(L) + 2k          — tends to select more complex models
#   AICc = AIC + 2k(k+1)/(T-k-1)  — corrected for small samples (use this!)
#   BIC  = -2*log(L) + k*log(T)    — stronger complexity penalty, more parsimonious
#
# where L = maximised likelihood, k = number of parameters, T = sample size
#
# In practice for ecological forecasting:
# - Use AICc as the primary criterion (corrects for small-sample bias)
# - BIC can be useful when you prefer simpler, more interpretable models
# - As T → infinity, AICc → AIC; BIC penalty grows with sample size

# Compare AIC, AICc, BIC for butterfly ARIMA models of varying complexity
butterfly_multi_fit <- train_butterfly_ts %>%
  model(
    arima100 = ARIMA(abundance ~ pdq(1, 0, 0) + PDQ(0, 0, 0)),
    arima010 = ARIMA(abundance ~ pdq(0, 1, 0) + PDQ(0, 0, 0)),
    arima110 = ARIMA(abundance ~ pdq(1, 1, 0) + PDQ(0, 0, 0)),
    auto     = ARIMA(abundance)
  )

glance(butterfly_multi_fit) %>%
  dplyr::arrange(AICc) %>%
  dplyr::select(.model, AICc, AIC, BIC, sigma2) %>%
  print_output("AIC/AICc/BIC comparison for butterfly ARIMA models")

# Note: the automatic ARIMA typically includes seasonal PDQ terms which
# dramatically improve fit for seasonal data — the non-seasonal models
# above will have much higher AICc because they ignore the seasonal pattern.

################################################################################
#################### Classical Models Exercises ################################
################################################################################

# Exercise 1: Simulate and identify an AR(3) process
# (a) Simulate 500 observations: y_t = 3 + 0.4*y_{t-1} + 0.2*y_{t-2} + 0.15*y_{t-3} + eps_t
# (b) Plot the ACF and PACF. How many significant PACF spikes are there?
# (c) Based on the ACF/PACF patterns, identify the model order.
# (d) Fit an automatic ARIMA model. Does it recover AR(3) as expected?

# Exercise 2: Simulate and identify an MA(2) process
# (a) Simulate 500 obs: y_t = 2 + eps_t + 0.6*eps_{t-1} + 0.3*eps_{t-2}
# (b) Plot ACF and PACF. How does the ACF cutoff differ from an AR(2)?
# (c) How does this pattern differ from Example 2 (AR(2))?

# Exercise 3: ARIMA on wetland bird data
# Using "wetland_bird_counts.csv" (total_waterbirds column):
# (a) Create a weekly tsibble and plot the series.
# (b) Fit an automatic ARIMA model.
# (c) Run residual diagnostics. Are the residuals white noise (Ljung-Box)?
# (d) Forecast 26 weeks ahead and plot with prediction intervals.

# Exercise 4: Manual vs automatic ARIMA on atmospheric CO2
# Using the built-in co2 dataset (as_tsibble(co2)):
# (a) Plot ACF/PACF of the seasonally differenced series (lag = 12).
# (b) Based on the ACF/PACF, propose a manual SARIMA specification.
# (c) Fit both your manual model and an automatic ARIMA (stepwise = FALSE).
# (d) Compare AICc values. Which model is preferred?

# Exercise 5: Complete ARIMA workflow on forest canopy cover
# Using "forest_canopy_cover.csv" for "Native_Mixed" forest:
# (a) Create a quarterly tsibble, split (2010-2022 train / 2023-2024 test).
# (b) Check stationarity (KPSS) and determine seasonal + non-seasonal diffs needed.
# (c) Fit automatic ARIMA + SNAIVE benchmark.
# (d) Compare forecast accuracy on the test set (RMSE, MAE, MASE).
# (e) Which model wins? Discuss why ARIMA may or may not beat SNAIVE here.

################################################################################
#################### Classical Models — Advanced ##############################
################################################################################
# Advanced examples extend the ARIMA framework with transfer function models,
# ensemble approaches, and systematic specification searches to demonstrate
# best-practice model building for complex ecological time series.

# Advanced Example 1: Systematic ARIMA order search with AICc grid
# Compare a grid of non-seasonal ARIMA(p,1,q) models for Irish exports
ireland_adv_ts <- global_economy %>% dplyr::filter(Code == "IRL")

arima_grid_fit <- ireland_adv_ts %>%
  model(
    arima100 = ARIMA(Exports ~ pdq(1, 0, 0)),
    arima011 = ARIMA(Exports ~ pdq(0, 1, 1)),
    arima110 = ARIMA(Exports ~ pdq(1, 1, 0)),
    arima111 = ARIMA(Exports ~ pdq(1, 1, 1)),
    arima210 = ARIMA(Exports ~ pdq(2, 1, 0)),
    auto     = ARIMA(Exports)
  )

glance(arima_grid_fit) %>%
  dplyr::arrange(AICc) %>%
  dplyr::select(.model, AICc, AIC, BIC) %>%
  print_output("Advanced Example 1: AICc grid for Irish Exports ARIMA")

# Advanced Example 2: Forecast uncertainty comparison
bf_adv_train <- butterfly_ts %>% dplyr::filter(year(date) <= 2022)
bf_adv_fit <- bf_adv_train %>%
  model(
    ARIMA  = ARIMA(abundance),
    SNAIVE = SNAIVE(abundance)
  )

bf_adv_fc <- bf_adv_fit %>% forecast(h = 24)

# Plot WITH prediction intervals to show uncertainty quantification
bf_adv_fc %>%
  autoplot(butterfly_ts) +
  theme_new() +
  scale_fill_manual(values = pallete[1:2]) +
  scale_colour_manual(values = pallete[1:2]) +
  labs(title    = "Advanced Example 2: Forecast Uncertainty — ARIMA vs SNAIVE",
       subtitle = "Shaded regions = 80% and 95% prediction intervals",
       y        = "Abundance (count)")

# Advanced Example 3: Ljung-Box test at multiple lags
bf_arima_adv <- bf_adv_train %>%
  model(auto = ARIMA(abundance))

# Test at three different lag choices
for (test_lag in c(12, 24, 36)) {
  augment(bf_arima_adv) %>%
    features(.innov, ljung_box, lag = test_lag, dof = 2) %>%
    dplyr::mutate(lag_tested = test_lag) %>%
    print_output(paste("Advanced Example 3: Ljung-Box at lag", test_lag))
}

# Advanced Example 4: ARIMA forecast vs historical average baseline
naive_rmse <- accuracy(bf_adv_fit %>%
                         dplyr::select(SNAIVE) %>%
                         forecast(h = 24),
                       butterfly_ts) %>%
  dplyr::pull(RMSE)

arima_rmse <- accuracy(bf_adv_fit %>%
                         dplyr::select(ARIMA) %>%
                         forecast(h = 24),
                       butterfly_ts) %>%
  dplyr::pull(RMSE)

cat("Advanced Example 4: ARIMA RMSE improvement over SNAIVE:",
    round((naive_rmse - arima_rmse) / naive_rmse * 100, 1), "%\n")

# Advanced Example 5: ARIMA on all three forest types simultaneously
forest_adv_ts <- load_ecological_tsibble(
  file_path  = "forest_canopy_cover.csv",
  index_col  = "date",
  index_type = "yearquarter",
  key_col    = "forest_type"
)

# The model() function automatically fits a separate ARIMA to each key series
forest_arima_fit <- forest_adv_ts %>%
  dplyr::filter(year(date) <= 2022) %>%
  model(auto = ARIMA(canopy_cover_pct))

forest_arima_fit %>%
  forecast(h = 8) %>%
  autoplot(forest_adv_ts) +
  theme_new() +
  scale_colour_manual(values = pallete) +
  labs(title    = "Advanced Example 5: ARIMA Forecast for All Forest Types",
       subtitle = "One ARIMA model fitted per forest type automatically",
       y        = "Canopy Cover (%)")

################################################################################
################## Classical Models — Advanced Exercises #######################
################################################################################

# Advanced Exercise 1: AICc model selection for wetland birds
# Using wetland_ts:
# (a) Fit ARIMA(1,0,0), ARIMA(0,0,1), ARIMA(1,0,1), and automatic ARIMA.
# (b) Compare all four using AICc.
# (c) Select the best model and run residual diagnostics.
# (d) Forecast 26 weeks ahead from the best model.

# Advanced Exercise 2: ARIMA with transformation on butterfly data
# (a) Fit ARIMA(abundance) (no transform).
# (b) Fit ARIMA(log(abundance + 1)) (log transform).
# (c) Compare residual normality using a Q-Q plot or Shapiro-Wilk test.
# (d) Which model produces more normally distributed residuals?

# Advanced Exercise 3: Multi-horizon forecast accuracy
# For butterfly abundance, fit automatic ARIMA on years <= 2020.
# (a) Forecast at horizons h = 1, 6, 12, 24.
# (b) Compare RMSE at each horizon against the test data (2021-2024).
# (c) Does ARIMA accuracy degrade quickly with increasing horizon?
# (d) At what horizon does SNAIVE outperform ARIMA?

# Advanced Exercise 4: Seasonal ARIMA manual specification challenge
# Using beer production (quarterly, m = 4):
# (a) After seasonal differencing (lag=4), examine ACF/PACF.
# (b) Propose three candidate SARIMA(p,d,q)(P,1,Q)_4 specifications.
# (c) Fit all three plus automatic ARIMA.
# (d) Which manual specification comes closest to the automatic selection?

# Advanced Exercise 5: ARIMA ensemble forecasting
# Fit three ARIMA models to butterfly abundance:
#   Model A: ARIMA(abundance)
#   Model B: ARIMA(log(abundance + 1))
#   Model C: SNAIVE(abundance)
# (a) Generate forecasts from each model.
# (b) Compute the equal-weight ensemble forecast (average of .mean values).
# (c) Compare ensemble RMSE to individual model RMSE on the test set.
# (d) Does averaging reduce forecast error? Why or why not?

################################################################################
#################### Classical Models — Answers ################################
################################################################################

# Answer 1:
set.seed(123)
n_ans <- 500
eps_ans <- rnorm(n_ans, sd = 1)
ar3_values <- numeric(n_ans)
ar3_values[1:3] <- eps_ans[1:3]

for (time_step in 4:n_ans) {
  ar3_values[time_step] <- 3 +
    0.4 * ar3_values[time_step - 1] +
    0.2 * ar3_values[time_step - 2] +
    0.15 * ar3_values[time_step - 3] +
    eps_ans[time_step]
}

# (b) ACF/PACF
par(mfrow = c(2, 1))
acf(ar3_values, lag.max = 20, main = "ACF of Simulated AR(3)")
pacf(ar3_values, lag.max = 20, main = "PACF of Simulated AR(3)")
par(mfrow = c(1, 1))

# (c) PACF should show 3 significant spikes (lags 1, 2, 3) then cut off.
# ACF should show exponential decay. This identifies AR(3).

# (d) Automatic ARIMA
ar3_ts <- tsibble(Time = 1:n_ans, Value = ar3_values, index = Time)
ar3_fit <- ar3_ts %>%
  model(ARIMA(Value))
report(ar3_fit)
# The automatic ARIMA should recover an AR(3) or close equivalent.

# Answer 2:
set.seed(456)
eps_ans2 <- rnorm(n_ans, sd = 1)
ma2_sim_values <- numeric(n_ans)
ma2_sim_values[1:2] <- eps_ans2[1:2]

for (time_step in 3:n_ans) {
  ma2_sim_values[time_step] <- 2 + eps_ans2[time_step] +
    0.6 * eps_ans2[time_step - 1] +
    0.3 * eps_ans2[time_step - 2]
}

# (b) ACF and PACF
par(mfrow = c(2, 1))
acf(ma2_sim_values, lag.max = 20, main = "ACF of Simulated MA(2)")
pacf(ma2_sim_values, lag.max = 20, main = "PACF of Simulated MA(2)")
par(mfrow = c(1, 1))

# (c) MA(2): ACF cuts off after lag 2; PACF decays exponentially.
# This is the OPPOSITE of AR(2): AR(2) has PACF cutoff after lag 2 and
# ACF decay. Knowing this distinction is the core of ARIMA identification.

# Answer 3:
wetland_ts <- load_ecological_tsibble(
  file_path  = "wetland_bird_counts.csv",
  index_col  = "date",
  index_type = "yearweek"
)

# (a) Plot
autoplot(wetland_ts, total_waterbirds) +
  theme_new() +
  labs(title = "Weekly Waterbird Counts", y = "Count")

# (b) Automatic ARIMA
wetland_arima_fit <- wetland_ts %>%
  model(auto = ARIMA(total_waterbirds))

report(wetland_arima_fit)

# (c) Diagnostics
wetland_arima_fit %>%
  gg_tsresiduals() +
  labs(title = "ARIMA Residuals — Wetland Birds")

augment(wetland_arima_fit) %>%
  features(.innov, ljung_box, lag = 24) %>%
  print_output("Ljung-Box: wetland ARIMA residuals")

# (d) Forecast 26 weeks ahead
wetland_arima_fit %>%
  forecast(h = 26) %>%
  autoplot(wetland_ts) +
  theme_new() +
  labs(title = "ARIMA Forecast — Wetland Birds (26 weeks ahead)",
       y     = "Total Waterbird Count")

# Answer 4:
co2_ts <- as_tsibble(co2)

# (a) ACF/PACF of seasonally differenced CO2
co2_ts %>%
  gg_tsdisplay(difference(value, lag = 12),
               plot_type = "partial", lag_max = 36) +
  labs(title = "Seasonally Differenced CO2 — ACF/PACF")

# (b) From ACF: significant spike at lag 1 → MA(1) term
# From seasonal ACF: spike at lag 12 → SMA(1) term
# Proposed: ARIMA(0,0,1)(0,1,1)_12 or ARIMA(1,0,0)(0,1,1)_12

# (c) Fit both manual and automatic
co2_arima_fit <- co2_ts %>%
  model(
    manual1 = ARIMA(value ~ pdq(0, 0, 1) + PDQ(0, 1, 1)),
    manual2 = ARIMA(value ~ pdq(1, 0, 0) + PDQ(0, 1, 1)),
    auto    = ARIMA(value, stepwise = FALSE)
  )

# (d) Compare AICc
glance(co2_arima_fit) %>%
  dplyr::arrange(AICc) %>%
  dplyr::select(.model, AICc, BIC) %>%
  print_output("CO2 ARIMA model comparison")

# Answer 5:
mixed_forest_ts <- load_ecological_tsibble(
  file_path  = "forest_canopy_cover.csv",
  index_col  = "date",
  index_type = "yearquarter",
  key_col    = "forest_type"
) %>%
  dplyr::filter(forest_type == "Native_Mixed")

train_mixed_ts <- mixed_forest_ts %>% dplyr::filter(year(date) <= 2022)
test_mixed_ts  <- mixed_forest_ts %>% dplyr::filter(year(date) > 2022)

# (b) KPSS test
train_mixed_ts %>%
  features(canopy_cover_pct, unitroot_kpss) %>%
  print_output("KPSS: Native Mixed canopy")

train_mixed_ts %>%
  features(canopy_cover_pct, unitroot_ndiffs) %>%
  print_output("First differences needed")

# (c) Fit ARIMA and SNAIVE
mixed_arima_fit <- train_mixed_ts %>%
  model(
    ARIMA  = ARIMA(canopy_cover_pct),
    SNAIVE = SNAIVE(canopy_cover_pct)
  )

# (d) Forecast accuracy
mixed_arima_fc <- mixed_arima_fit %>%
  forecast(h = nrow(test_mixed_ts))

mixed_arima_fc %>%
  autoplot(mixed_forest_ts, level = NULL) +
  theme_new() +
  scale_colour_manual(values = pallete) +
  labs(title  = "ARIMA vs SNAIVE — Native Mixed Forest Canopy",
       y      = "Canopy Cover (%)",
       colour = "Model")

accuracy(mixed_arima_fc, mixed_forest_ts) %>%
  dplyr::select(.model, RMSE, MAE, MASE) %>%
  dplyr::arrange(RMSE) %>%
  print_output("ARIMA vs SNAIVE: Native Mixed Forest accuracy")

# (e) ARIMA should outperform SNAIVE because it can model the
# autocorrelation structure and the trend simultaneously. SNAIVE only
# repeats last year's pattern, missing trend and inter-seasonal correlations.

# Answer 6:
wetland_arima_comparison <- wetland_ts %>%
  model(
    arima100 = ARIMA(total_waterbirds ~ pdq(1, 0, 0)),
    arima001 = ARIMA(total_waterbirds ~ pdq(0, 0, 1)),
    arima101 = ARIMA(total_waterbirds ~ pdq(1, 0, 1)),
    auto     = ARIMA(total_waterbirds)
  )

glance(wetland_arima_comparison) %>%
  dplyr::arrange(AICc) %>%
  dplyr::select(.model, AICc, AIC, BIC) %>%
  print_output("Wetland ARIMA model comparison by AICc")

# Select best model and diagnose
wetland_arima_comparison %>%
  dplyr::select(auto) %>%
  gg_tsresiduals() +
  labs(title = "Best ARIMA Residuals — Wetland Birds")

wetland_arima_comparison %>%
  dplyr::select(auto) %>%
  forecast(h = 26) %>%
  autoplot(wetland_ts) +
  theme_new() +
  labs(title = "Best ARIMA Forecast — Wetland Birds (26 weeks)",
       y     = "Total Waterbird Count")

# Answer 7:
butterfly_transform_fit <- train_butterfly_ts %>%
  model(
    no_transform = ARIMA(abundance),
    log_transform = ARIMA(log(abundance + 1))
  )

glance(butterfly_transform_fit) %>%
  dplyr::arrange(AICc) %>%
  dplyr::select(.model, AICc) %>%
  print_output("Butterfly ARIMA: transform comparison by AICc")

# The AICc values are on different scales (log-transformed vs original),
# so direct comparison requires care. Examine residual normality instead:
butterfly_transform_fit %>%
  dplyr::select(no_transform) %>%
  gg_tsresiduals() +
  labs(title = "Residuals: No Transform")

butterfly_transform_fit %>%
  dplyr::select(log_transform) %>%
  gg_tsresiduals() +
  labs(title = "Residuals: Log Transform")
# If the log-transformed residuals are more normally distributed and their
# ACF is cleaner, the transformation is justified.
