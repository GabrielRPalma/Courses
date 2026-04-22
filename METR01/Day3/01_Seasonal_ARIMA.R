####################################################################################################
###
### File:    01_Seasonal_ARIMA.R
### Purpose: Examples and exercises for Seasonal ARIMA (SARIMA)
### Authors: Gabriel Rodrigues Palma
### Date:    17/06/25
###
####################################################################################################
# load packages -----
source('00_source.r')

################################################################################
########################## Seasonal ARIMA (SARIMA) #############################
################################################################################

# Ecological time series often contain both a TREND and a clear SEASONAL
# pattern — more waterbirds in winter, fewer in summer; higher beer
# production in summer quarters.
#
# SARIMA extends ARIMA to capture both NON-SEASONAL and SEASONAL dynamics
# in one unified model.
#
# SARIMA notation: ARIMA(p,d,q)(P,D,Q)_m
#   (p,d,q)   = non-seasonal AR order, differencing, MA order
#   (P,D,Q)_m = seasonal AR order, seasonal differencing, seasonal MA order
#   m         = seasonal period (12 for monthly, 4 for quarterly)
#
# The seasonal difference removes the seasonal component:
#   (1 - B^m) y_t = y_t - y_{t-m}
# Combined with first differencing:
#   (1 - B)(1 - B^12) y_t  removes both trend and monthly seasonality

################################################################################
###################### Example 1: Beer Seasonality Overview ###################
################################################################################

# Australian quarterly beer production is a textbook example of strong
# seasonality: Q4 peaks (summer drinking), Q2 troughs (winter).
# This dataset lives in fpp3 as aus_production.

beer_data <- aus_production %>%
  filter(year(Quarter) >= 1992) %>%
  dplyr::select(Quarter, Beer)

# Time plot: clear seasonal cycle with slight downward trend
p1 <- beer_data %>%
  autoplot(Beer) +
  labs(title = "Australian Quarterly Beer Production",
       subtitle = "Seasonal cycle: peak Q4 (summer), trough Q2 (winter)",
       y = "Megalitres", x = "Quarter") +
  theme_new()
print(p1)

# Seasonal plot: each year overlaid to reveal the consistent seasonal shape
p2 <- beer_data %>%
  gg_season(Beer, labels = "both") +
  labs(title = "Seasonal Plot: Beer Production",
       subtitle = "Q4 consistently highest, Q2 consistently lowest",
       y = "Megalitres") +
  theme_new()
print(p2)

# Subseries plot: each quarter shown separately over time
p3 <- beer_data %>%
  gg_subseries(Beer) +
  labs(title = "Subseries Plot: Beer Production by Quarter",
       y = "Megalitres") +
  theme_new()
print(p3)

################################################################################
################## Example 2: Seasonal Patterns in Waterbird Counts ###########
################################################################################

# Waterbird migration follows a strong autumn peak (Oct-Nov arrivals from
# breeding grounds) and summer trough (Jun-Aug, few migrants).
# This is a classic ecological time series requiring a seasonal model.

waterbird <- read.csv("input_data/waterbird_migration.csv")

waterbird_ts <- waterbird %>%
  mutate(Month = yearmonth(date)) %>%
  as_tsibble(index = Month)

# Time plot: clear migration-driven seasonal peaks
p4 <- waterbird_ts %>%
  autoplot(count) +
  labs(title = "Monthly Waterbird Counts at Shannon Wetland",
       subtitle = "Strong autumn peaks (Oct-Nov) from migratory arrivals",
       y = "Count", x = "Month") +
  theme_new()
print(p4)

# Seasonal plot: the repeating autumn migration pattern
p5 <- waterbird_ts %>%
  gg_season(count, labels = "both") +
  labs(title = "Seasonal Plot: Waterbird Counts",
       subtitle = "Peak Oct-Dec (autumn migration), trough Jun-Aug (summer)",
       y = "Count") +
  theme_new()
print(p5)

################################################################################
##################### Example 3: Seasonal Differencing ########################
################################################################################

# Before fitting SARIMA, we need stationarity.
# Seasonal differencing removes the seasonal component:
#   y'_t = y_t - y_{t-m}  (monthly data: m = 12)
# This asks: "How does this January compare to last January?"

# How many seasonal differences does the beer series need?
beer_data %>%
  features(Beer, unitroot_nsdiffs)
# Expect nsdiffs = 1

# Apply seasonal differencing (quarterly: lag = 4)
beer_sdiff <- beer_data %>%
  mutate(beer_sdiff = difference(Beer, lag = 4))

p6 <- beer_sdiff %>%
  autoplot(beer_sdiff) +
  labs(title = "Seasonally Differenced Beer Production",
       subtitle = "Seasonal pattern removed; may still need first differencing",
       y = "Difference (lag 4)") +
  theme_new()
print(p6)

# Check if we also need non-seasonal first differencing
beer_sdiff %>%
  features(beer_sdiff, unitroot_ndiffs)
# If ndiffs = 0, seasonal differencing alone sufficed

# Seasonal differencing for waterbird data (monthly: lag = 12)
waterbird_sdiff <- waterbird_ts %>%
  mutate(count_sdiff = difference(count, lag = 12))

p7 <- waterbird_sdiff %>%
  autoplot(count_sdiff) +
  labs(title = "Seasonally Differenced Waterbird Counts",
       subtitle = "Seasonal migration pattern removed (lag = 12)",
       y = "Difference (lag 12)") +
  theme_new()
print(p7)

################################################################################
############# Example 4: Seasonal ACF/PACF for Model Identification ###########
################################################################################

# The ACF and PACF of the seasonally differenced series guide SARIMA order
# selection. Look for spikes at seasonal lags (4, 8, 12...) and at lags 1-3.
#
# Seasonal ACF/PACF signatures:
#   Spike at lag m in ACF only           → seasonal MA(1): Q = 1
#   Spike at lag m in PACF only          → seasonal AR(1): P = 1
#   Exponential decay at seasonal lags   → seasonal AR component

beer_sdiff %>%
  ACF(beer_sdiff, lag_max = 24) %>%
  autoplot() +
  labs(title = "ACF of Seasonally Differenced Beer Production",
       subtitle = "Look at lags 4, 8, 12... for seasonal MA/AR patterns") +
  theme_new()

beer_sdiff %>%
  PACF(beer_sdiff, lag_max = 24) %>%
  autoplot() +
  labs(title = "PACF of Seasonally Differenced Beer Production",
       subtitle = "Seasonal spikes help determine P and Q") +
  theme_new()

waterbird_sdiff %>%
  ACF(count_sdiff, lag_max = 36) %>%
  autoplot() +
  labs(title = "ACF of Seasonally Differenced Waterbird Counts",
       subtitle = "Seasonal lags: 12, 24, 36...") +
  theme_new()

################################################################################
################ Example 5: Fitting SARIMA with fable::ARIMA() ################
################################################################################

# fable's ARIMA() searches over (p,d,q)(P,D,Q) using AICc.
# Automatic selection finds the best seasonal model without manual specification.

beer_fit <- beer_data %>%
  model(
    sarima_auto     = ARIMA(Beer),
    sarima_011_011  = ARIMA(Beer ~ pdq(0,1,1) + PDQ(0,1,1)),
    arima_no_seasonal = ARIMA(Beer ~ PDQ(0,0,0))
  )

print_output(beer_fit, "SARIMA Models for Beer Production")

# Detailed report for the automatic model
beer_fit %>%
  select(sarima_auto) %>%
  report()
# Expect something like ARIMA(0,1,1)(0,1,1)[4] — the classic airline model

################################################################################
##################### Example 6: SARIMA Residual Diagnostics #################
################################################################################

# Good residuals are white noise: no ACF spikes, normal distribution,
# constant variance.

beer_fit %>%
  select(sarima_auto) %>%
  gg_tsresiduals() +
  labs(title = "SARIMA Residual Diagnostics: Beer Production")

# Ljung-Box test: formal white noise test on residuals
beer_fit %>%
  select(sarima_auto) %>%
  augment() %>%
  features(.innov, ljung_box, lag = 8, dof = 2)
# p-value > 0.05 → residuals are consistent with white noise

################################################################################
##################### Example 7: Forecasting with SARIMA #####################
################################################################################

# Generate forecasts and plot with 80% and 95% prediction intervals

beer_fc <- beer_fit %>%
  forecast(h = "3 years")

p8 <- beer_fc %>%
  autoplot(beer_data, level = c(80, 95)) +
  labs(title = "Beer Production Forecasts: SARIMA vs Non-Seasonal ARIMA",
       subtitle = "SARIMA captures seasonal shape; non-seasonal ARIMA misses it",
       y = "Megalitres", x = "Quarter") +
  facet_wrap(~.model, ncol = 1) +
  theme_new()
print(p8)

################################################################################
################# Advanced Decomposition — SARIMA Exercises #####################
################################################################################

# Exercise 1: Seasonal Differencing Practice
# Using the waterbird_ts dataset:
# a) How many seasonal differences are needed? Use unitroot_nsdiffs()
# b) Apply seasonal differencing with difference(count, lag = 12)
# c) After seasonal differencing, how many first differences are needed?
# d) Plot the ACF of the fully differenced series

# Exercise 2: Manual SARIMA Specification
# Using beer_data:
# a) Fit ARIMA(1,0,0)(1,1,0)[4] manually using pdq() and PDQ()
# b) Fit ARIMA(0,1,1)(0,1,1)[4] — the classic quarterly airline model
# c) Compare both using AICc from glance()
# d) Which model has better diagnostics? Check with gg_tsresiduals()

# Exercise 3: SARIMA Forecasting Challenge
# Load us_employment and filter for "Leisure and Hospitality":
# a) Create a time plot and seasonal plot
# b) Fit SARIMA, ETS, and SNAIVE models
# c) Generate 2-year forecasts and plot them
# d) Which model's forecast looks most reasonable for this sector? Why?

# Exercise 4: Training/Test Accuracy
# Using waterbird_ts:
# a) Split into training (2010-2020) and test (2021-2024) sets
# b) Fit SARIMA, ETS, and SNAIVE on the training set
# c) Forecast the test period and compute accuracy metrics
# d) Which model has the lowest RMSE? Discuss in ecological terms.

# Exercise 5: SARIMA vs Non-Seasonal ARIMA for Gas Production
# Using aus_production Gas:
# a) Plot the time series and identify the seasonal period
# b) Fit ARIMA(p,d,q) with PDQ(0,0,0) and automatic SARIMA
# c) Compare forecasts visually and with accuracy()
# d) Write a comment explaining WHY the seasonal model performs better

################################################################################
################## Advanced Decomposition — Seasonal ARIMA Answers ############
################################################################################

# Answer 1: Seasonal Differencing on Waterbird Data
# a) Seasonal differences needed
waterbird_ts %>%
  features(count, unitroot_nsdiffs)

# b) Apply seasonal differencing
waterbird_ex1 <- waterbird_ts %>%
  mutate(count_sdiff = difference(count, lag = 12))

# c) First differences after seasonal differencing
waterbird_ex1 %>%
  features(count_sdiff, unitroot_ndiffs)

# d) ACF of fully differenced series
waterbird_ex1 %>%
  ACF(count_sdiff, lag_max = 36) %>%
  autoplot() +
  labs(title = "ACF of Seasonally Differenced Waterbird Counts") +
  theme_new()

# Answer 2: Manual SARIMA Specification
# a) and b) Manual specification
beer_manual <- beer_data %>%
  model(
    sarima_100_110 = ARIMA(Beer ~ pdq(1,0,0) + PDQ(1,1,0)),
    sarima_011_011 = ARIMA(Beer ~ pdq(0,1,1) + PDQ(0,1,1))
  )

# c) AICc comparison
beer_manual %>%
  glance() %>%
  select(.model, AICc, BIC)

# d) Residual diagnostics for each
beer_manual %>%
  select(sarima_100_110) %>%
  gg_tsresiduals()

beer_manual %>%
  select(sarima_011_011) %>%
  gg_tsresiduals()

# Answer 3: Leisure & Hospitality Employment
leisure_employment <- us_employment %>%
  filter(Title == "Leisure and Hospitality", year(Month) >= 2000)

# a) Time and seasonal plots
leisure_employment %>%
  autoplot(Employed) +
  theme_new()

leisure_employment %>%
  gg_season(Employed) +
  theme_new()

# b) Fit models
leisure_fit <- leisure_employment %>%
  model(
    SARIMA = ARIMA(Employed),
    ETS    = ETS(Employed),
    SNAIVE = SNAIVE(Employed)
  )

# c) Forecast and plot
leisure_fc <- leisure_fit %>%
  forecast(h = 24)

leisure_fc %>%
  autoplot(leisure_employment %>% filter(year(Month) >= 2015), level = 80) +
  labs(title = "Leisure and Hospitality Employment Forecasts") +
  facet_wrap(~.model) +
  theme_new()
# SARIMA captures the seasonal hospitality hiring pattern better than SNAIVE.

# Answer 4: Training/Test Split for Waterbird
waterbird_train <- waterbird_ts %>%
  filter(year(Month) <= 2020)

waterbird_test <- waterbird_ts %>%
  filter(year(Month) > 2020)

waterbird_fit_train <- waterbird_train %>%
  model(
    SARIMA = ARIMA(count),
    ETS    = ETS(count),
    SNAIVE = SNAIVE(count)
  )

waterbird_fc_train <- waterbird_fit_train %>%
  forecast(h = nrow(waterbird_test))

waterbird_fc_train %>%
  accuracy(waterbird_test) %>%
  select(.model, RMSE, MAE, MAPE) %>%
  arrange(RMSE)
# SARIMA should score low RMSE because it explicitly models the autumn-winter
# migration peaks that dominate waterbird abundance data.

# Answer 5: SARIMA vs Non-Seasonal ARIMA for Gas Production
gas_data <- aus_production %>%
  select(Quarter, Gas) %>%
  filter(!is.na(Gas))

# a) Time plot — seasonal period = 4 (quarterly)
gas_data %>%
  autoplot(Gas) +
  theme_new()

# b) Fit models
gas_fit <- gas_data %>%
  model(
    arima_no_season = ARIMA(Gas ~ PDQ(0,0,0)),
    sarima_auto     = ARIMA(Gas)
  )

# c) Forecasts
gas_fc <- gas_fit %>%
  forecast(h = 12)

gas_fc %>%
  autoplot(gas_data %>% filter(year(Quarter) >= 2000), level = 80) +
  facet_wrap(~.model) +
  theme_new()

# d) Australian gas production has a strong quarterly cycle (higher in winter
#    for heating). The non-seasonal ARIMA cannot capture this recurring pattern
#    and produces flat, uninformative forecasts. The seasonal model is superior.

################################################################################
###################### Seasonal ARIMA — Advanced Examples #####################
################################################################################

# Advanced Example 1: Beer — SARIMA vs ETS Accuracy on Test Data
beer_train <- beer_data %>%
  filter(year(Quarter) <= 2006)

beer_test <- beer_data %>%
  filter(year(Quarter) > 2006)

beer_fit_cv <- beer_train %>%
  model(
    SARIMA = ARIMA(Beer),
    ARIMA_ns = ARIMA(Beer ~ PDQ(0,0,0)),
    SNAIVE = SNAIVE(Beer),
    ETS    = ETS(Beer)
  )

beer_fc_cv <- beer_fit_cv %>%
  forecast(h = nrow(beer_test))

accuracy_tbl <- beer_fc_cv %>%
  accuracy(beer_test)

print_output(accuracy_tbl %>%
               select(.model, RMSE, MAE, MAPE, MASE),
             "Accuracy Comparison: SARIMA vs ARIMA vs SNAIVE vs ETS")

# Advanced Example 2: AICc/BIC Model Selection — US Retail Employment
retail_employment <- us_employment %>%
  filter(Title == "Retail Trade", year(Month) >= 1990)

retail_fit <- retail_employment %>%
  model(
    SARIMA = ARIMA(Employed),
    ETS    = ETS(Employed)
  )

retail_glance <- retail_fit %>%
  glance() %>%
  select(.model, AICc, BIC)

print_output(retail_glance, "Model Comparison: AICc and BIC (US Retail Employment)")

retail_fc <- retail_fit %>%
  forecast(h = 24)

p_retail <- retail_fc %>%
  autoplot(retail_employment %>% filter(year(Month) >= 2015), level = 80) +
  labs(title = "US Retail Employment: SARIMA vs ETS",
       y = "Thousands employed", x = "Month") +
  theme_new()
print(p_retail)

# Advanced Example 3: Full SARIMA on Waterbird with 2-year Forecast
waterbird_fit_full <- waterbird_ts %>%
  model(
    SARIMA = ARIMA(count),
    ETS    = ETS(count),
    SNAIVE = SNAIVE(count)
  )

print_output(waterbird_fit_full, "Models for Waterbird Migration")

waterbird_fit_full %>%
  select(SARIMA) %>%
  report()

waterbird_fit_full %>%
  select(SARIMA) %>%
  gg_tsresiduals() +
  labs(title = "SARIMA Diagnostics: Waterbird Migration")

waterbird_fc_full <- waterbird_fit_full %>%
  forecast(h = 24)

p9 <- waterbird_fc_full %>%
  autoplot(waterbird_ts %>% filter(year(Month) >= 2020), level = c(80, 95)) +
  labs(title = "Waterbird Migration Forecasts (2025-2026)",
       subtitle = "SARIMA captures the autumn-winter migration peaks",
       y = "Count", x = "Month") +
  facet_wrap(~.model, ncol = 1) +
  theme_new()
print(p9)

# Advanced Example 4: Grading the Seasonal Fit with Ljung-Box
# After fitting, extract augmented residuals and run the Ljung-Box test
# on each model to confirm white-noise residuals.
waterbird_fit_full %>%
  augment() %>%
  filter(.model == "SARIMA") %>%
  features(.innov, ljung_box, lag = 12, dof = 2)
# p > 0.05 confirms SARIMA residuals behave as white noise.

# Advanced Example 5: SARIMA with Box-Cox Transformation
# Variance-stabilising transformation before SARIMA fitting
lambda_beer <- beer_data %>%
  features(Beer, features = guerrero) %>%
  pull(lambda_guerrero)

cat("Box-Cox lambda (beer):", round(lambda_beer, 3), "\n")

beer_fit_bc <- beer_data %>%
  model(
    sarima_bc = ARIMA(box_cox(Beer, lambda_beer))
  )

beer_fit_bc %>%
  select(sarima_bc) %>%
  report()

beer_fit_bc %>%
  forecast(h = 12) %>%
  autoplot(beer_data %>% filter(year(Quarter) >= 2005), level = 80) +
  labs(title = "SARIMA with Box-Cox Transformation: Beer Production",
       subtitle = paste("lambda =", round(lambda_beer, 3)),
       y = "Megalitres") +
  theme_new()

################################################################################
################## Seasonal ARIMA — Advanced Exercises ########################
################################################################################

# Advanced Exercise 1: Automated vs Manual SARIMA Comparison
# Using the waterbird migration dataset:
# a) Fit the automatic ARIMA(Beer) — note the selected (p,d,q)(P,D,Q)
# b) Manually fit ARIMA(0,1,1)(0,1,1)[12] and ARIMA(1,1,1)(1,1,0)[12]
# c) Compare all three using glance() — AICc, BIC, log-likelihood
# d) For the best model, run gg_tsresiduals() and the Ljung-Box test
# e) Conclusion: does automatic selection find the parsimonious model?

# Advanced Exercise 2: Cross-Validation with Stretching Windows
# Using beer_data and the fable time-series cross-validation:
# a) Create an expanding-window cross-validation object with stretch_tsibble()
#    (initial = 20, step = 4)
# b) Fit SARIMA, ETS, and SNAIVE on each fold
# c) Evaluate 1-step and 4-step ahead RMSE
# d) Which model is most accurate at short horizons vs long horizons?

# Advanced Exercise 3: Seasonal Spectral Analysis
# Using the waterbird counts:
# a) Compute the periodogram with spectrum()
# b) Identify the dominant frequency peak — what period does it correspond to?
# c) Is the seasonal period exactly 12 months, or does it shift slightly?
# d) How does the periodogram help decide the value of m in SARIMA?

# Advanced Exercise 4: Waterbird SARIMA with Holiday Effects
# Suppose we know that unusual weather events (recorded in a binary "event"
# column) affect waterbird counts:
# a) Load waterbird_migration.csv and add a dummy variable for Oct-Nov
#    (peak migration months)
# b) Fit SARIMA with this dummy as an xreg variable using ARIMA(count ~ event)
# c) Compare to SARIMA without the dummy — does AICc improve?
# d) Discuss: how does adding ecological covariates improve time series models?

# Advanced Exercise 5: Multi-Site SARIMA
# Imagine monitoring waterbirds at 3 sites (North, Central, South).
# a) Simulate three correlated monthly series (2010-2024) with shared
#    seasonal patterns but different trend directions
# b) Create a multi-keyed tsibble and fit SARIMA to each site simultaneously
# c) Plot all three forecasts on the same axis
# d) Discuss: should each site have its own model, or is a common model better?
