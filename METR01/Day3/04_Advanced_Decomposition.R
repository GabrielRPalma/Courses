####################################################################################################
###
### File:    04_Advanced_Decomposition.R
### Purpose: Examples and exercises for Advanced Decomposition Methods
### Authors: Gabriel Rodrigues Palma
### Date:    17/06/25
###
####################################################################################################
# load packages -----
source('00_source.r')

################################################################################
################### Advanced Decomposition Methods #############################
################################################################################

# Classical decomposition and STL (Day 2) work well for data with ONE dominant
# seasonal period. But real ecological data often has MULTIPLE seasonal periods:
#   - Coastal ecosystems: tidal (12h) + daily + seasonal
#   - Energy demand: daily + weekly + yearly
#   - Insect activity: daily + seasonal
#
# This script covers:
#   1. STL with multiple seasonal periods
#   2. TBATS (Trigonometric, Box-Cox, ARMA, Trend, Seasonal)
#   3. Prophet — decomposition as growth + seasonality + holidays
#   4. NNAR — Neural Network AutoRegression
#   5. Decomposed components as engineered features for ML (Day 4 bridge)

# Constants for advanced decomposition ---------------------------
TBATS_DAILY_PERIOD  <- 7       # weekly cycle for daily data
TBATS_YEARLY_PERIOD <- 365.25  # yearly cycle
NNAR_FORECAST_H     <- 24      # default NNAR forecast horizon (months)

################################################################################
############## Example 1: Complex Seasonality — Victorian Electricity ##########
################################################################################

# vic_elec (fpp3): half-hourly electricity demand for Victoria, Australia.
# Contains DAILY seasonality (people wake/cook/heat) and YEARLY seasonality
# (summer air conditioning, winter heating).

# Aggregate to daily total demand
vic_daily <- vic_elec %>%
  index_by(Date = as_date(Time)) %>%
  summarise(
    demand_gwh  = sum(Demand) / 1e3,
    max_temp_c  = max(Temperature)
  )

p1 <- vic_daily %>%
  autoplot(demand_gwh) +
  labs(title = "Victorian Daily Electricity Demand",
       subtitle = "Weekly (weekdays vs weekends) + yearly seasonality",
       y = "Demand (GWh)", x = "Date") +
  theme_new()
print(p1)

# Zoom into a few weeks to reveal the weekly pattern
p2 <- vic_daily %>%
  filter(Date >= "2013-06-01", Date <= "2013-08-01") %>%
  autoplot(demand_gwh) +
  labs(title = "Electricity Demand: Weekly Pattern (Jun-Aug 2013)",
       subtitle = "Lower demand on weekends, higher on weekdays",
       y = "Demand (GWh)", x = "Date") +
  theme_new()
print(p2)

################################################################################
############## Example 2: STL with Multiple Seasonal Periods ###################
################################################################################

# fable's STL() handles multiple seasonal periods when applied to sub-daily data.
# We decompose with daily and yearly seasonality simultaneously.

vic_2year <- vic_elec %>%
  filter(year(Time) %in% c(2012, 2013))

# Specify both daily (period = "day") and yearly (period = "year") seasonality
vic_stl <- vic_2year %>%
  model(
    stl = STL(Demand ~ season(period = "day") + season(period = "year"))
  )

vic_components <- vic_stl %>%
  components()

p3 <- vic_components %>%
  autoplot() +
  labs(title = "STL Decomposition: Multiple Seasonal Periods",
       subtitle = "Daily and yearly seasonality separated simultaneously") +
  theme_new()
print(p3)

################################################################################
############## Example 3: TBATS Model for Complex Seasonality #################
################################################################################

# TBATS: Trigonometric seasonality, Box-Cox transformation,
# ARMA errors, Trend, Seasonal components.
#
# TBATS uses Fourier terms to model each seasonal period — more flexible than
# seasonal dummies. fit via forecast::tbats() (fable has no native TBATS).

# Convert daily demand to msts object with weekly + yearly periods
vic_msts <- forecast::msts(vic_daily$demand_gwh,
                            seasonal.periods = c(TBATS_DAILY_PERIOD,
                                                  TBATS_YEARLY_PERIOD))

tbats_fit <- forecast::tbats(vic_msts)
cat("\n--- TBATS Model Summary ---\n")
print(tbats_fit)

# TBATS decomposition plot
plot(tbats_fit, main = "TBATS Decomposition of Daily Electricity Demand")

# 90-day forecast
tbats_fc <- forecast::forecast(tbats_fit, h = 90)
plot(tbats_fc,
     main = "TBATS Forecast: 90 Days Ahead",
     xlab = "Time", ylab = "Demand (GWh)")

################################################################################
############## Example 4: ETS Decomposition as a Prophet Analogue #############
################################################################################

# Prophet (Facebook/Meta) decomposes time series as:
#   y(t) = g(t) + s(t) + h(t) + epsilon_t
#   g(t) = growth/trend; s(t) = seasonality; h(t) = holiday effects
#
# For ecological data: g(t) = population trend, s(t) = seasonal life-cycle,
# h(t) = disturbance events.
#
# We use the fpp3-built-in aus_retail dataset as a clean demonstration,
# fitting ETS and ARIMA as accessible alternatives to Prophet.

food_retail_vic <- aus_retail %>%
  filter(
    State    == "Victoria",
    Industry == "Food retailing"
  ) %>%
  dplyr::select(Month, Turnover)

# Fit ETS and ARIMA for comparison
retail_fit <- food_retail_vic %>%
  model(
    ETS   = ETS(Turnover),
    ARIMA = ARIMA(Turnover)
  )

# ETS components (analogous to Prophet's decomposition)
retail_ets_components <- retail_fit %>%
  dplyr::select(ETS) %>%
  components()

p4 <- retail_ets_components %>%
  autoplot() +
  labs(title = "ETS Decomposition: Victorian Food Retail",
       subtitle = "Trend, seasonal, and remainder components") +
  theme_new()
print(p4)

# Forecast comparison
retail_fc <- retail_fit %>%
  forecast(h = NNAR_FORECAST_H)

p5 <- retail_fc %>%
  autoplot(food_retail_vic %>% filter(year(Month) >= 2015), level = 80) +
  labs(title = "Forecasts: ETS vs ARIMA for Food Retail",
       y = "Turnover (A$M)", x = "Month") +
  theme_new()
print(p5)

################################################################################
############## Example 5: NNAR — Neural Network AutoRegression ################
################################################################################

# NNAR(p, P, k)_m: a feed-forward neural network using:
#   p lagged inputs (non-seasonal)
#   P lagged inputs at the seasonal period
#   k hidden neurons
#   m = seasonal period
#
# This is a simple nonlinear model — a bridge between classical time series
# methods and deep learning.

beer_data <- aus_production %>%
  filter(year(Quarter) >= 1992) %>%
  dplyr::select(Quarter, Beer)

nnar_fit <- beer_data %>%
  model(
    NNAR  = NNETAR(Beer),
    ARIMA = ARIMA(Beer),
    ETS   = ETS(Beer)
  )

nnar_fit %>%
  dplyr::select(NNAR) %>%
  report()

nnar_fc <- nnar_fit %>%
  forecast(h = 12)

p6 <- nnar_fc %>%
  autoplot(beer_data %>% filter(year(Quarter) >= 2005), level = NULL) +
  labs(title = "NNAR vs ARIMA vs ETS: Beer Production Forecasts",
       subtitle = "NNAR captures nonlinear seasonal patterns",
       y = "Megalitres", x = "Quarter") +
  theme_new()
print(p6)

################################################################################
############## Example 6: Decomposition Components as ML Features #############
################################################################################

# KEY BRIDGE TO DAY 4: Decomposed components (trend, seasonal, remainder) can
# serve as ENGINEERED FEATURES for machine learning models.
#
# Instead of feeding the raw series into RF/XGBoost, we decompose first and
# use the components as separate input columns. This gives the ML model
# pre-structured information about the signal.

waterbird <- read.csv("input_data/waterbird_migration.csv")

waterbird_ts <- waterbird %>%
  mutate(Month = yearmonth(date)) %>%
  as_tsibble(index = Month)

waterbird_stl <- waterbird_ts %>%
  model(STL(count ~ season(window = "periodic"))) %>%
  components()

# Create a feature data frame from decomposition
decomp_features <- waterbird_stl %>%
  as_tibble() %>%
  dplyr::select(Month, count, trend, season_year, remainder) %>%
  mutate(
    trend_momentum    = trend - lag(trend),
    noise_magnitude   = abs(remainder),
    seasonal_strength = abs(season_year) /
                        (abs(season_year) + abs(remainder) + 0.01)
  )

print_output(head(decomp_features, 12),
             "Decomposition-Based Feature Engineering")

cat("\n--- These columns become inputs for ML models on Day 4 ---\n")
cat("Columns:", paste(names(decomp_features), collapse = ", "), "\n")

# Quick correlation check: which features relate to count?
decomp_numeric <- decomp_features %>%
  select(where(is.numeric)) %>%
  na.omit()

cor_matrix <- cor(decomp_numeric)
cat("\n--- Correlation with count ---\n")
print(round(cor_matrix[, "count"], 3))

################################################################################
#################### Advanced Decomposition Exercises ##########################
################################################################################

# Exercise 1: Complex Seasonality Detection
# Using vic_elec (fpp3):
# a) Aggregate to daily totals (as in Example 1)
# b) Plot the ACF up to lag 400 — can you see multiple seasonal peaks?
# c) At what lags do the peaks occur? What periods do they correspond to?
# d) Why is standard single-period STL insufficient for this data?

# Exercise 2: TBATS vs STL on Australian Retail
# Filter aus_retail for "New South Wales" and "Department stores":
# a) Fit both STL decomposition and TBATS
# b) Compare the decomposition components visually
# c) Generate 24-month forecasts from both
# d) Which method captures the seasonal shape better?

# Exercise 3: NNAR Hyperparameter Exploration
# Using waterbird_ts:
# a) Fit NNAR with default settings
# b) Fit NNAR with AR(p = 12, P = 1), size = 10
# c) Fit NNAR with AR(p = 6, P = 1), size = 5
# d) Compare forecasts — how sensitive is NNAR to hyperparameter choices?
# e) Discuss: why might NNAR overfit ecological time series?

# Exercise 4: Feature Engineering from Decomposition
# Using waterbird_ts:
# a) Apply STL decomposition
# b) Create a data frame with trend, seasonal, remainder, plus lag-1 of each
# c) Add the target: next month's count (lead(count, 1))
# d) Compute correlation of each feature with the target
# e) Which decomposition feature has the strongest predictive relationship?

# Exercise 5: Method Comparison Dashboard
# Using aus_production Beer data:
# a) Fit ARIMA, ETS, SNAIVE, and NNAR models
# b) Split into training (pre-2005) and test (2005+) sets
# c) Generate forecasts and compute accuracy for each
# d) Create a comparison table showing RMSE, MAE, and MAPE
# e) Which method wins? Is NNAR's extra complexity justified?

################################################################################
#################### Advanced Decomposition — Answers #########################
################################################################################

# Answer 1: Complex Seasonality in Victorian Electricity
vic_daily_ex <- vic_elec %>%
  index_by(Date = as_date(Time)) %>%
  summarise(demand_gwh = sum(Demand) / 1e3)

# b) ACF up to lag 400
vic_daily_ex %>%
  ACF(demand_gwh, lag_max = 400) %>%
  autoplot() +
  labs(title = "ACF: Daily Electricity Demand (up to 400 lags)") +
  theme_new()

# c) Peaks at lag 7 (weekly) and ~365 (yearly).
# d) Standard STL handles one seasonal period. This data has two distinct
#    seasonal cycles requiring multi-seasonal decomposition.

# Answer 2: TBATS vs STL on NSW Department Stores
nsw_dept <- aus_retail %>%
  filter(State == "New South Wales",
         Industry == "Department stores") %>%
  select(Month, Turnover)

nsw_stl <- nsw_dept %>%
  model(STL(Turnover ~ season(window = "periodic")))

nsw_tbats_ts <- ts(nsw_dept$Turnover, frequency = 12,
                    start = c(1982, 4))
nsw_tbats <- forecast::tbats(nsw_tbats_ts)

nsw_stl %>%
  components() %>%
  autoplot() +
  theme_new()
plot(nsw_tbats)

# c) Forecasts
nsw_stl_fc <- nsw_dept %>%
  model(stl_arima = decomposition_model(
    STL(Turnover ~ season(window = "periodic")),
    ARIMA(season_adjust)
  )) %>%
  forecast(h = 24)

nsw_tbats_fc <- forecast::forecast(nsw_tbats, h = 24)

# d) TBATS handles potential changes in seasonal amplitude through its
#    trigonometric formulation; STL assumes a more rigid seasonal shape.

# Answer 3: NNAR Hyperparameter Sensitivity
nnar_default <- waterbird_ts %>%
  model(nnar_default = NNETAR(count))

nnar_large <- waterbird_ts %>%
  model(nnar_large = NNETAR(count ~ AR(p = 12, P = 1), size = 10))

nnar_small <- waterbird_ts %>%
  model(nnar_small = NNETAR(count ~ AR(p = 6, P = 1), size = 5))

nnar_compare <- waterbird_ts %>%
  model(
    Default = NNETAR(count),
    Large   = NNETAR(count ~ AR(p = 12, P = 1), size = 10),
    Small   = NNETAR(count ~ AR(p = 6, P = 1), size = 5)
  )

nnar_compare %>%
  forecast(h = NNAR_FORECAST_H) %>%
  autoplot(waterbird_ts %>% filter(year(Month) >= 2022), level = NULL) +
  theme_new()

# e) NNAR can overfit because:
#    - Ecological data is noisy with small sample sizes
#    - Neural networks have many parameters relative to data points
#    - Flexibility to fit nonlinear patterns can model noise as signal

# Answer 4: Decomposition Feature Engineering
wb_stl <- waterbird_ts %>%
  model(STL(count ~ season(window = "periodic"))) %>%
  components()

wb_features <- wb_stl %>%
  as_tibble() %>%
  mutate(
    trend_lag1     = lag(trend),
    seasonal_lag1  = lag(season_year),
    remainder_lag1 = lag(remainder),
    count_next     = lead(count)
  ) %>%
  select(count_next, trend, season_year, remainder,
         trend_lag1, seasonal_lag1, remainder_lag1) %>%
  na.omit()

cors <- cor(wb_features)
print(round(cors[, "count_next"], 3))

# e) Trend typically has the strongest correlation: it captures the slow-moving
#    population level. Seasonal also contributes because migration timing is
#    highly predictable.

# Answer 5: Method Comparison for Beer Production
beer_ex5 <- aus_production %>%
  filter(year(Quarter) >= 1992) %>%
  select(Quarter, Beer)

beer_train_ex5 <- beer_ex5 %>% filter(year(Quarter) < 2005)
beer_test_ex5  <- beer_ex5 %>% filter(year(Quarter) >= 2005)

beer_models_ex5 <- beer_train_ex5 %>%
  model(
    ARIMA  = ARIMA(Beer),
    ETS    = ETS(Beer),
    SNAIVE = SNAIVE(Beer),
    NNAR   = NNETAR(Beer)
  )

beer_fc_ex5 <- beer_models_ex5 %>%
  forecast(h = nrow(beer_test_ex5))

beer_fc_ex5 %>%
  accuracy(beer_test_ex5) %>%
  select(.model, RMSE, MAE, MAPE) %>%
  arrange(RMSE)

# e) ARIMA or ETS often wins for this smooth seasonal series.
#    NNAR's extra complexity is not always justified unless the data has
#    strong nonlinear patterns that simpler models miss.

################################################################################
################# Advanced Decomposition — Advanced Examples ##################
################################################################################

# Advanced Example 1: STL Robustness — Outlier in Waterbird Counts
# STL is robust to outliers when robust = TRUE.

waterbird_stl_robust <- waterbird_ts %>%
  model(
    stl_classic = STL(count),
    stl_robust  = STL(count, robust = TRUE)
  )

# Compare the remainder components
comps_robust <- waterbird_stl_robust %>%
  components()

p_robust <- comps_robust %>%
  filter(.model %in% c("stl_classic", "stl_robust")) %>%
  autoplot(remainder) +
  labs(title = "STL Classic vs Robust: Remainder Component",
       subtitle = "Robust STL is less sensitive to outliers") +
  theme_new()
print(p_robust)

# Advanced Example 2: STL + ARIMA vs ETS — Forecast Competition
# Use decomposition_model() to combine STL with ARIMA on the seasonal-adjusted
# component, then compare to standalone ETS.

waterbird_stl_fc <- waterbird_ts %>%
  model(
    stl_arima = decomposition_model(
      STL(count ~ season(window = "periodic")),
      ARIMA(season_adjust)
    ),
    ETS = ETS(count)
  ) %>%
  forecast(h = 24)

p_stl_fc <- waterbird_stl_fc %>%
  autoplot(waterbird_ts %>% filter(year(Month) >= 2020), level = 80) +
  labs(title = "STL+ARIMA vs ETS: Waterbird 2-year Forecast",
       y = "Count", x = "Month") +
  facet_wrap(~.model, ncol = 1) +
  theme_new()
print(p_stl_fc)

# Advanced Example 3: Seasonal Strength Diagnostic
# feasts::feat_stl() computes trend and seasonal strength.
# Values near 1 indicate strong components; near 0 indicate weak.

stl_features <- waterbird_ts %>%
  features(count, feat_stl)

print_output(stl_features, "STL Feature Summary: Trend and Seasonal Strength")

beer_stl_features <- beer_data %>%
  features(Beer, feat_stl)

print_output(beer_stl_features, "STL Feature Summary: Beer Production")

# Advanced Example 4: TBATS on Aphid Weekly Data
# Aphid counts have seasonal + within-year variation. TBATS handles this.
aphid_data <- read.csv("aphid_climate_brazil.csv")

aphid_msts <- forecast::msts(aphid_data$aphid_abundance,
                              seasonal.periods = c(52))

aphid_tbats <- forecast::tbats(aphid_msts)
cat("\n--- Aphid TBATS Model ---\n")
print(aphid_tbats)

aphid_tbats_fc <- forecast::forecast(aphid_tbats, h = 52)
plot(aphid_tbats_fc,
     main = "Aphid Abundance: TBATS 1-Year Forecast",
     ylab = "Aphid count")

# Advanced Example 5: Cross-Validation with decomposition_model()
# Use time-series cross-validation to evaluate STL+ARIMA on waterbird data.

waterbird_cv <- waterbird_ts %>%
  stretch_tsibble(.init = 60, .step = 12) %>%
  model(
    stl_arima = decomposition_model(
      STL(count ~ season(window = "periodic")),
      ARIMA(season_adjust)
    ),
    ETS = ETS(count)
  ) %>%
  forecast(h = 12)

cv_accuracy <- waterbird_cv %>%
  accuracy(waterbird_ts)

print_output(
  cv_accuracy %>% select(.model, RMSE, MAE, MAPE) %>% arrange(RMSE),
  "Cross-Validation Accuracy: STL+ARIMA vs ETS (12-month horizon)"
)

################################################################################
############# Advanced Decomposition — Advanced Exercises #####################
################################################################################

# Advanced Exercise 1: Multiple-Period STL on Half-Hourly Data
# Using vic_elec (half-hourly, fpp3):
# a) Filter to calendar year 2013 only
# b) Fit STL with both daily (period = 48) and weekly (period = 7*48) seasonality
# c) Extract and plot the daily and weekly seasonal components separately
# d) Which component has more variance? What does this imply for scheduling?

# Advanced Exercise 2: Fourier Term Approach to Multiple Seasonality
# TBATS uses Fourier terms internally. Here, fit them manually with fable:
# a) Create Fourier terms with K = 3 terms for each of weekly and yearly periods
#    on vic_daily (hint: fourier_terms() from the forecast package)
# b) Fit ARIMA with the Fourier terms as xreg
# c) Compare the fitted values to the TBATS decomposition
# d) Which approach is more interpretable?

# Advanced Exercise 3: NNAR vs Random Forest for Seasonal Data
# Using beer_data:
# a) Create a feature matrix: month-of-year dummies + lag-1, lag-2, lag-4 of Beer
# b) Fit a Random Forest on the feature matrix (use randomForest package)
# c) Fit NNAR with the default settings
# d) Compare 1-year-ahead RMSE using training/test split
# e) Is NNAR essentially a special case of a shallow neural network?

# Advanced Exercise 4: Decomposition as an Anomaly Detector
# Using pop_vol (monthly insect population index):
# a) Apply STL decomposition with robust = TRUE
# b) Standardise the remainder component: (remainder - mean) / sd
# c) Flag observations where |standardised remainder| > 3 as anomalies
# d) Do these anomalies align with the known disturbance events in the dataset?

# Advanced Exercise 5: Feature Importance of Decomposition Components
# Using waterbird_ts:
# a) Build the decomposition feature matrix from Answer 4 above
# b) Split into training (pre-2022) and test (2022+) sets
# c) Fit a linear model: count_next ~ trend + season_year + remainder
# d) Also fit: count_next ~ trend + seasonal_strength + trend_momentum
# e) Which features are most significant? Compare RMSE on the test set.
