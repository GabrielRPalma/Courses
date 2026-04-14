####################################################################################################
###
### File:    05_Incorporating_Exogenous_Time_Series.R
### Purpose: Examples and exercises for Incorporating Exogenous Time Series (ARIMAX)
### Authors: Gabriel Rodrigues Palma
### Date:    17/06/25
###
####################################################################################################
# load packages -----
source('00_source.r')

################################################################################
############## Incorporating Exogenous Time Series (ARIMAX) ####################
################################################################################
# Butterfly populations don't just depend on their past — they respond to
# temperature, rainfall, and plant phenology. Dynamic regression lets us
# incorporate these external ecological drivers directly into our forecasts,
# producing models that are both more accurate and more interpretable.
#
# The key model structure: REGRESSION with ARIMA ERRORS
#   y_t = beta_0 + beta_1*x_{1,t} + ... + beta_k*x_{k,t} + eta_t
#   where eta_t ~ ARIMA(p, d, q)
#
# This is NOT the same as fitting OLS regression with autocorrelated residuals.
# There are TWO distinct types of error in the model:
#   1. eta_t — the regression residual: variation unexplained by predictors
#   2. eps_t — the ARIMA innovation: white noise after ARIMA modelling of eta_t
#
# The ARIMA component captures temporal autocorrelation that remains AFTER
# accounting for the predictor variables. This separation is crucial: it
# prevents confounding the ecological effect of predictors with their
# shared temporal patterns (e.g., temperature and abundance both peak in summer).

################################################################################
############### Example 1: US Consumption and Income ###########################
################################################################################

# Classic fpp3 textbook example: US quarterly consumption predicted by income.
# This demonstrates the core concept of dynamic regression before moving
# to ecological applications.

us_change %>%
  tidyr::pivot_longer(c(Consumption, Income),
                      names_to  = "Variable",
                      values_to = "Value") %>%
  ggplot(aes(x = Quarter, y = Value)) +
  geom_line() +
  facet_grid(vars(Variable), scales = "free_y") +
  theme_new() +
  labs(y     = "Quarterly % change",
       title = "US Consumption and Income — Both Trending Together")

# Fit dynamic regression: Consumption ~ Income with ARIMA errors
fit_consumption <- us_change %>%
  model(ARIMA(Consumption ~ Income))

report(fit_consumption)

# Interpretation of the report:
# y_t = intercept + beta_income * Income_t + eta_t
# eta_t follows an ARIMA process (captures autocorrelation beyond income effect)
# The income coefficient means: a 1% increase in income is associated with
# approximately a 0.2% increase in consumption in the same quarter.

# Visualise the two distinct error types to understand the model structure
dplyr::bind_rows(
  `Regression residuals (eta)` =
    as_tibble(residuals(fit_consumption, type = "regression")),
  `ARIMA residuals (epsilon)` =
    as_tibble(residuals(fit_consumption, type = "innovation")),
  .id = "type"
) %>%
  dplyr::mutate(type = factor(type, levels = c("Regression residuals (eta)",
                                                "ARIMA residuals (epsilon)"))) %>%
  ggplot(aes(x = Quarter, y = .resid)) +
  geom_line() +
  facet_grid(vars(type), scales = "free_y") +
  theme_new() +
  labs(title    = "Two Types of Residuals in Dynamic Regression",
       subtitle = "eta shows autocorrelation; epsilon should be white noise",
       y        = "Residual")

# Diagnostics — the innovation residuals (epsilon) must be white noise
fit_consumption %>% gg_tsresiduals()

augment(fit_consumption) %>%
  features(.innov, ljung_box, dof = 5, lag = 10) %>%
  print_output("Ljung-Box test: US consumption ARIMA errors")

################################################################################
############### Example 2: Forecasting with Future Scenarios ###################
################################################################################

# A key advantage of dynamic regression: we can explore "what if" scenarios
# by providing hypothetical future predictor values. This is scientifically
# valuable but requires careful ecological interpretation — we are assuming
# the estimated coefficient beta_income is the true causal effect.

# Scenario: Income grows at the historical average
future_income_ts <- new_data(us_change, 8) %>%
  dplyr::mutate(Income = mean(us_change$Income))

forecast(fit_consumption, new_data = future_income_ts) %>%
  autoplot(us_change) +
  theme_new() +
  labs(y     = "Percentage change",
       title = "US Consumption Forecast — Average Income Growth Scenario")

# Key insight: Prediction intervals are narrower than a pure ARIMA forecast
# because the Income predictor explains additional variation. Each predictor
# that we can forecast well reduces the residual uncertainty.

################################################################################
############### Example 3: Climate-Driven Butterfly Forecasting ################
################################################################################

# The primary ecological application: predict butterfly abundance from
# temperature and rainfall using dynamic regression.
# Ecological motivation:
#   - Warmer temperatures → longer activity season → higher abundance
#   - Excessive rainfall → reduced flight activity → lower abundance
# The ARIMA errors will capture any remaining temporal autocorrelation
# not explained by these two climate drivers.

butterfly_ts <- load_ecological_tsibble(
  file_path  = "input_data/butterfly_meadow_monitoring.csv",
  index_col  = "date",
  index_type = "yearmonth"
)

# Visualise the three time series together to motivate the model
butterfly_ts %>%
  tidyr::pivot_longer(c(abundance, temperature_c, rainfall_mm),
                      names_to  = "Variable",
                      values_to = "Value") %>%
  ggplot(aes(x = date, y = Value)) +
  geom_line() +
  facet_grid(Variable ~ ., scales = "free_y") +
  theme_new() +
  labs(title    = "Butterfly Abundance with Climate Drivers",
       subtitle = "Temperature and rainfall as exogenous predictors")

# Scatter plots: abundance vs individual predictors
plot_abundance_temp <- butterfly_ts %>%
  as_tibble() %>%
  ggplot(aes(x = temperature_c, y = abundance)) +
  geom_point(alpha = 0.5, colour = pallete[1]) +
  geom_smooth(method = "loess", colour = pallete[2]) +
  theme_new() +
  labs(title = "Abundance vs Temperature",
       x     = "Temperature (°C)",
       y     = "Abundance")

plot_abundance_rain <- butterfly_ts %>%
  as_tibble() %>%
  ggplot(aes(x = rainfall_mm, y = abundance)) +
  geom_point(alpha = 0.5, colour = pallete[1]) +
  geom_smooth(method = "loess", colour = pallete[2]) +
  theme_new() +
  labs(title = "Abundance vs Rainfall",
       x     = "Rainfall (mm)",
       y     = "Abundance")

plot_abundance_temp | plot_abundance_rain

# Ecological interpretation:
# - Positive relationship with temperature (more butterflies in warm months)
# - Weak negative relationship with excess rainfall (heavy rain reduces activity)

# Split into training (2015-2022) and test (2023-2024) sets
train_butterfly_ts <- butterfly_ts %>% dplyr::filter(year(date) <= 2022)
test_butterfly_ts  <- butterfly_ts %>% dplyr::filter(year(date) > 2022)

# Fit dynamic regression: abundance ~ temperature + rainfall + ARIMA errors
butterfly_arimax_fit <- train_butterfly_ts %>%
  model(
    arima_only = ARIMA(abundance),
    arimax     = ARIMA(abundance ~ temperature_c + rainfall_mm)
  )

# Report the ARIMAX model coefficients
butterfly_arimax_fit %>%
  dplyr::select(arimax) %>%
  report()

# Diagnostics — the innovation residuals of arimax must be white noise
butterfly_arimax_fit %>%
  dplyr::select(arimax) %>%
  gg_tsresiduals() +
  labs(title = "ARIMAX Residual Diagnostics — Butterfly Abundance")

augment(butterfly_arimax_fit) %>%
  dplyr::filter(.model == "arimax") %>%
  features(.innov, ljung_box, lag = 24) %>%
  print_output("Ljung-Box: ARIMAX residuals")

################################################################################
############### Example 4: Scenario Forecasting — Climate Warming ##############
################################################################################

# What happens to butterfly populations under different climate scenarios?
# We create hypothetical future climate conditions using make_climate_scenario()
# (defined in 00_source.r) and compare the model's predictions.
# This is one of the most ecologically valuable uses of dynamic regression:
# it turns a statistical forecasting tool into a policy analysis tool.

# Historical monthly averages (baseline seasonal pattern)
monthly_mean_temperature <- train_butterfly_ts %>%
  as_tibble() %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(avg_temp = mean(temperature_c)) %>%
  dplyr::pull(avg_temp)

monthly_mean_rainfall <- train_butterfly_ts %>%
  as_tibble() %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(avg_rain = mean(rainfall_mm)) %>%
  dplyr::pull(avg_rain)

# Scenario 1: Status quo — repeat historical seasonal pattern
scenario_baseline <- make_climate_scenario(
  training_ts   = train_butterfly_ts,
  horizon       = 3,
  monthly_temp  = monthly_mean_temperature,
  monthly_rain  = monthly_mean_rainfall,
  temp_offset   = 0,
  rain_scale    = 1
)

# Scenario 2: Climate warming — temperature +2°C above historical
scenario_warming_2c <- make_climate_scenario(
  training_ts   = train_butterfly_ts,
  horizon       = 6,
  monthly_temp  = monthly_mean_temperature,
  monthly_rain  = monthly_mean_rainfall,
  temp_offset   = 2,
  rain_scale    = 1
)

# Scenario 3: Drought — rainfall reduced by 30%
scenario_drought <- make_climate_scenario(
  training_ts   = train_butterfly_ts,
  horizon       = 6,
  monthly_temp  = monthly_mean_temperature,
  monthly_rain  = monthly_mean_rainfall,
  temp_offset   = 0,
  rain_scale    = 0.7
)

# Scenario 4: Combined warming + drought (worst case)
scenario_warming_drought <- make_climate_scenario(
  training_ts   = train_butterfly_ts,
  horizon       = 6,
  monthly_temp  = monthly_mean_temperature,
  monthly_rain  = monthly_mean_rainfall,
  temp_offset   = 2,
  rain_scale    = 0.7
)

# Forecast under each scenario
fc_baseline         <- butterfly_arimax_fit %>%
  dplyr::select(arimax) %>%
  forecast(new_data = scenario_baseline) %>%
  dplyr::mutate(Scenario = "Baseline")

fc_warming_2c       <- butterfly_arimax_fit %>%
  dplyr::select(arimax) %>%
  forecast(new_data = scenario_warming_2c) %>%
  dplyr::mutate(Scenario = "Warming +2\u00b0C")

fc_drought          <- butterfly_arimax_fit %>%
  dplyr::select(arimax) %>%
  forecast(new_data = scenario_drought) %>%
  dplyr::mutate(Scenario = "Drought -30%")

fc_warming_drought  <- butterfly_arimax_fit %>%
  dplyr::select(arimax) %>%
  forecast(new_data = scenario_warming_drought) %>%
  dplyr::mutate(Scenario = "Warming + Drought")

# Combine all scenarios for a single comparison plot
all_climate_scenarios <- dplyr::bind_rows(
  fc_baseline, fc_warming_2c, fc_drought, fc_warming_drought
)

all_climate_scenarios %>%
  as_tibble() %>%
  ggplot(aes(x = date, y = .mean, colour = Scenario)) +
  geom_line(linewidth = 1) +
  geom_line(data = as_tibble(butterfly_ts),
            aes(x = date, y = abundance),
            colour = "gray50", alpha = 0.5, inherit.aes = FALSE) +
  scale_colour_manual(values = pallete[1:4]) +
  theme_new() +
  labs(title    = "Butterfly Abundance — Climate Scenario Forecasts",
       subtitle = "Gray = historical data; coloured lines = forecast scenarios",
       y        = "Predicted Abundance (count)",
       x        = "Month",
       colour   = "Climate Scenario")

# Ecological interpretation:
# - Warming +2°C: May increase butterfly abundance (extended activity season)
# - Drought -30%: Effect depends on whether baseline rainfall is beneficial or
#   detrimental; typical Irish conditions (high rainfall) may mean drought helps
# - Combined: Net effect depends on which driver dominates in the estimated model

################################################################################
############### Example 5: ARIMA vs ARIMAX Accuracy Comparison #################
################################################################################

# Does adding climate predictors actually improve out-of-sample forecasts?
# We need to provide the ACTUAL observed predictor values in the test period
# for a fair comparison. This tests whether the ecological signal in
# temperature and rainfall genuinely improves prediction, or whether the
# ARIMA component already captures the shared seasonal pattern implicitly.

# Forecast from ARIMA-only (no predictors, just temporal autocorrelation)
fc_arima_only <- butterfly_arimax_fit %>%
  dplyr::select(arima_only) %>%
  forecast(h = 6)

# Forecast from ARIMAX using the actual test predictor values
fc_arimax_actual <- butterfly_arimax_fit %>%
  dplyr::select(arimax) %>%
  forecast(new_data = test_butterfly_ts)

# SNAIVE benchmark
fc_snaive_bench <- train_butterfly_ts %>%
  model(SNAIVE = SNAIVE(abundance)) %>%
  forecast(h = 6)

# Combine for a comparative plot
dplyr::bind_rows(
  fc_arima_only   %>% dplyr::mutate(.model = "ARIMA only"),
  fc_arimax_actual %>% dplyr::mutate(.model = "ARIMAX (temp + rain)"),
  fc_snaive_bench  %>% dplyr::mutate(.model = "SNAIVE")
) %>%
  autoplot(butterfly_ts, level = NULL) +
  theme_new() +
  scale_colour_manual(values = pallete) +
  labs(title  = "ARIMA vs ARIMAX vs SNAIVE — Butterfly Forecasts",
       y      = "Abundance (count)",
       colour = "Model")

# Accuracy comparison table
acc_arima_only   <- accuracy(fc_arima_only, butterfly_ts) %>%
  dplyr::mutate(.model = "ARIMA only")
acc_arimax       <- accuracy(fc_arimax_actual, butterfly_ts) %>%
  dplyr::mutate(.model = "ARIMAX (temp + rain)")
acc_snaive_bench <- accuracy(fc_snaive_bench, butterfly_ts) %>%
  dplyr::mutate(.model = "SNAIVE")

dplyr::bind_rows(acc_arima_only, acc_arimax, acc_snaive_bench) %>%
  dplyr::select(.model, RMSE, MAE, MASE) %>%
  dplyr::arrange(RMSE) %>%
  print_output("Forecast Accuracy: ARIMA vs ARIMAX vs SNAIVE")

# Interpretation: If ARIMAX beats ARIMA, the climate predictors add genuine
# value. If not, the seasonal ARIMA component may already capture most of
# the temperature/rainfall effects indirectly — because both temperature
# and butterfly abundance share the same annual seasonal cycle.

################################################################################
############### Example 6: Electricity Demand with Climate #####################
################################################################################

# A non-ecological example showing a NONLINEAR predictor relationship.
# Victorian electricity demand is driven by temperature but with a U-shaped
# curve (heating in cold + air conditioning in hot) — this requires a
# quadratic term. The same principle applies in ecology: species may have
# optimal temperature ranges (thermal performance curves).

vic_elec_daily_ts <- vic_elec %>%
  dplyr::filter(year(Time) == 2014) %>%
  index_by(Date = date(Time)) %>%
  dplyr::summarise(
    Demand      = sum(Demand) / 1e3,
    Temperature = max(Temperature),
    Holiday     = any(Holiday)
  ) %>%
  dplyr::mutate(day_type = dplyr::case_when(
    Holiday                    ~ "Holiday",
    wday(Date) %in% 2:6        ~ "Weekday",
    TRUE                       ~ "Weekend"
  ))

# Scatter plot: U-shaped temperature-demand relationship
vic_elec_daily_ts %>%
  ggplot(aes(x = Temperature, y = Demand, colour = day_type)) +
  geom_point(alpha = 0.7) +
  scale_colour_manual(values = pallete) +
  theme_new() +
  labs(y        = "Electricity demand (GW)",
       x        = "Maximum daily temperature (\u00b0C)",
       title    = "Electricity Demand vs Temperature",
       subtitle = "U-shaped: heating in cold + cooling in hot weather")

# Fit dynamic regression with quadratic temperature term
fit_electricity <- vic_elec_daily_ts %>%
  model(ARIMA(Demand ~ Temperature + I(Temperature^2) +
                (day_type == "Weekday")))

report(fit_electricity)

# Diagnostics
fit_electricity %>% gg_tsresiduals()

# Forecast under temperature scenarios: warm spell vs heat wave
elec_future_warm <- new_data(vic_elec_daily_ts, 14) %>%
  dplyr::mutate(
    Temperature = 26,
    Holiday     = c(TRUE, rep(FALSE, 13)),
    day_type    = dplyr::case_when(
      Holiday             ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE                ~ "Weekend"
    )
  )

elec_future_hot <- new_data(vic_elec_daily_ts, 14) %>%
  dplyr::mutate(
    Temperature = 35,
    Holiday     = c(TRUE, rep(FALSE, 13)),
    day_type    = dplyr::case_when(
      Holiday             ~ "Holiday",
      wday(Date) %in% 2:6 ~ "Weekday",
      TRUE                ~ "Weekend"
    )
  )

plot_warm_scenario <- forecast(fit_electricity, elec_future_warm) %>%
  autoplot(vic_elec_daily_ts) +
  theme_new() +
  labs(title = "Warm Scenario (26\u00b0C)", y = "GW")

plot_hot_scenario <- forecast(fit_electricity, elec_future_hot) %>%
  autoplot(vic_elec_daily_ts) +
  theme_new() +
  labs(title = "Heat Wave Scenario (35\u00b0C)", y = "GW")

plot_warm_scenario / plot_hot_scenario

# The heat wave scenario predicts much higher demand due to air conditioning.
# Ecologically, this mirrors thermal performance curve analysis — species show
# optimal activity at intermediate temperatures, with decline at extremes.

################################################################################
############### Example 7: Comparing Single vs Multiple Predictors #############
################################################################################

# Model selection with multiple ecological predictors: should we use
# both temperature and rainfall, or is one driver sufficient?
# AICc comparison lets us test whether adding a predictor genuinely
# improves the model (lower AICc) or just adds noise (higher AICc).

butterfly_predictor_fit <- butterfly_ts %>%
  model(
    both_predictors  = ARIMA(abundance ~ temperature_c + rainfall_mm),
    temperature_only = ARIMA(abundance ~ temperature_c),
    rainfall_only    = ARIMA(abundance ~ rainfall_mm),
    arima_no_covars  = ARIMA(abundance)
  )

glance(butterfly_predictor_fit) %>%
  dplyr::arrange(AICc) %>%
  dplyr::select(.model, AICc, AIC, BIC) %>%
  print_output("Model comparison: single vs multiple predictors")

# The model with the lowest AICc is preferred. If adding rainfall doesn't
# reduce AICc when temperature is already included, then temperature alone
# captures most of the predictive power (temperature and rainfall are
# correlated in the seasonal cycle, so they contain redundant information).

################################################################################
######### Incorporating Exogenous Time Series Exercises #########################
################################################################################

# Exercise 1: River dissolved oxygen predicted by water temperature
# Using "river_water_quality.csv":
# (a) Fit ARIMA(dissolved_oxygen_mgl ~ water_temperature_c).
# (b) Report the model. What is the estimated temperature coefficient?
# (c) Interpret: for each 1°C increase in temperature, how does DO change?
#     Is the sign consistent with Henry's Law (dissolved gas solubility)?
# (d) Run residual diagnostics. Is the model adequate (Ljung-Box test)?

# Exercise 2: Wetland birds with water level as a predictor
# Using "wetland_bird_counts.csv":
# (a) Fit ARIMA(total_waterbirds ~ water_level_m).
# (b) Is the water_level coefficient statistically significant?
#     (Examine the report and check if the coefficient CI excludes zero)
# (c) Compare AICc: ARIMA only vs ARIMAX with water_level.
# (d) Does water level add predictive value beyond the seasonal pattern?

# Exercise 3: Scenario analysis for river dissolved oxygen
# Using the river water quality ARIMAX model from Exercise 1:
# (a) Forecast DO for 90 days with temperature held constant at 15°C (normal summer).
# (b) Forecast DO for 90 days with temperature held constant at 22°C (heat wave).
# (c) Plot both scenarios with a horizontal reference line at CRITICAL_DO_THRESHOLD (5 mg/L).
# (d) At what temperature does the mean DO forecast fall below the critical threshold?

# Exercise 4: Multiple climate predictors for butterfly abundance
# Using "butterfly_meadow_monitoring.csv":
# (a) Fit: ARIMA(abundance ~ temperature_c + rainfall_mm) (both predictors)
# (b) Fit: ARIMA(abundance ~ temperature_c) (temperature only)
# (c) Fit: ARIMA(abundance ~ rainfall_mm) (rainfall only)
# (d) Compare AICc across all three. Which predictor is more important?

# Exercise 5: Warming scenario analysis for butterfly abundance
# Using the full butterfly ARIMAX model (both temperature and rainfall):
# (a) Forecast 24 months under: baseline, +1°C, +2°C, +3°C warming.
# (b) Keep rainfall constant at historical monthly averages for all scenarios.
# (c) Plot all four scenarios on one chart (use pallete[1:4] for colours).
# (d) Calculate peak summer abundance for each scenario and identify
#     at what warming level peak abundance changes by more than 20%.

################################################################################
####### Incorporating Exogenous Time Series — Advanced ########################
################################################################################
# Advanced examples explore lagged predictors, nonlinear effects, multi-predictor
# selection, and scenario analysis at the population level — directly applicable
# to conservation planning and ecological impact assessments.

# Advanced Example 1: Lagged predictor effects in ARIMAX
# Temperature effects on abundance may be lagged — warm conditions this month
# may affect next month's butterfly count (larval development time).
# We create a lagged temperature variable and compare models.
butterfly_lag_ts <- butterfly_ts %>%
  dplyr::mutate(temp_lag1 = dplyr::lag(temperature_c, 1))

bf_lag_fit <- butterfly_lag_ts %>%
  dplyr::filter(year(date) <= 2022) %>%
  model(
    arimax_current = ARIMA(abundance ~ temperature_c),
    arimax_lagged1 = ARIMA(abundance ~ temp_lag1)
  )

glance(bf_lag_fit) %>%
  dplyr::arrange(AICc) %>%
  dplyr::select(.model, AICc) %>%
  print_output("Advanced Example 1: Current vs Lagged Temperature AICc")

# Advanced Example 2: Quadratic temperature effect (thermal performance curve)
# Butterflies may have an optimal temperature range; abundance may decline
# at both very low and very high temperatures.
bf_quad_fit <- butterfly_ts %>%
  dplyr::filter(year(date) <= 2022) %>%
  model(
    linear    = ARIMA(abundance ~ temperature_c),
    quadratic = ARIMA(abundance ~ temperature_c + I(temperature_c^2))
  )

glance(bf_quad_fit) %>%
  dplyr::arrange(AICc) %>%
  dplyr::select(.model, AICc, AIC) %>%
  print_output("Advanced Example 2: Linear vs Quadratic Temperature Effect")

# Advanced Example 3: assess_forecast_accuracy() utility function
# Using the helper function from 00_source.r for clean accuracy comparison
bf_adv_train <- butterfly_ts %>% dplyr::filter(year(date) <= 2022)
bf_adv_test  <- butterfly_ts %>% dplyr::filter(year(date) > 2022)

bf_adv_arimax <- bf_adv_train %>%
  model(arimax = ARIMA(abundance ~ temperature_c + rainfall_mm))

bf_adv_arima <- bf_adv_train %>%
  model(arima = ARIMA(abundance))

bf_adv_snaive <- bf_adv_train %>%
  model(snaive = SNAIVE(abundance))

all_accuracy <- dplyr::bind_rows(
  accuracy(bf_adv_arimax %>%
             forecast(new_data = bf_adv_test), butterfly_ts),
  accuracy(bf_adv_arima %>%
             forecast(h = FORECAST_HORIZON_LONG), butterfly_ts),
  accuracy(bf_adv_snaive %>%
             forecast(h = FORECAST_HORIZON_LONG), butterfly_ts)
) %>%
  dplyr::select(.model, RMSE, MAE, MASE) %>%
  dplyr::arrange(RMSE)

print_output(all_accuracy,
             "Advanced Example 3: All Models Compared via assess_forecast_accuracy()")

# Advanced Example 4: ARIMAX with seasonal dummies as additional predictors
# Adding explicit month dummy variables can help if the ARIMA component
# is not capturing the seasonal pattern adequately.
bf_seasonal_fit <- butterfly_ts %>%
  dplyr::filter(year(date) <= 2022) %>%
  dplyr::mutate(month_num = month(date)) %>%
  model(
    arimax_climate  = ARIMA(abundance ~ temperature_c + rainfall_mm),
    arimax_seasonal = ARIMA(abundance ~ temperature_c + rainfall_mm +
                              fourier(K = 2))
  )

glance(bf_seasonal_fit) %>%
  dplyr::select(.model, AICc) %>%
  dplyr::arrange(AICc) %>%
  print_output("Advanced Example 4: ARIMAX vs ARIMAX + Fourier seasonality")

# Advanced Example 5: Conservation threshold analysis
# At what temperature threshold does the model predict abundance falling
# below MIN_VIABLE_ABUNDANCE (defined in 00_source.r as 5 individuals)?
bf_threshold_fit <- butterfly_ts %>%
  dplyr::filter(year(date) <= 2022) %>%
  model(arimax = ARIMA(abundance ~ temperature_c + rainfall_mm))

monthly_rain_adv <- butterfly_ts %>%
  dplyr::filter(year(date) <= 2022) %>%
  as_tibble() %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(avg_rain = mean(rainfall_mm)) %>%
  dplyr::pull(avg_rain)

monthly_temp_adv <- butterfly_ts %>%
  dplyr::filter(year(date) <= 2022) %>%
  as_tibble() %>%
  dplyr::group_by(month) %>%
  dplyr::summarise(avg_temp = mean(temperature_c)) %>%
  dplyr::pull(avg_temp)

# Test extreme cooling scenario (-3°C)
fc_cooling <- forecast(
  bf_threshold_fit,
  new_data = make_climate_scenario(
    training_ts  = butterfly_ts %>% dplyr::filter(year(date) <= 2022),
    horizon      = FORECAST_HORIZON_LONG,
    monthly_temp = monthly_temp_adv,
    monthly_rain = monthly_rain_adv,
    temp_offset  = -3
  )
) %>%
  dplyr::mutate(Scenario = "Cooling -3°C")

fc_cooling %>%
  as_tibble() %>%
  ggplot(aes(x = date, y = .mean)) +
  geom_line(colour = pallete[1], linewidth = 1) +
  geom_hline(yintercept = MIN_VIABLE_ABUNDANCE,
             linetype = "dashed", colour = "red") +
  annotate("text", x = min(fc_cooling$date) + 2,
           y = MIN_VIABLE_ABUNDANCE + 1,
           label = paste("Viability threshold (", MIN_VIABLE_ABUNDANCE, ")"),
           colour = "red", hjust = 0) +
  theme_new() +
  labs(title    = "Advanced Example 5: Conservation Threshold Analysis",
       subtitle = "Red line = minimum viable abundance threshold",
       y        = "Predicted Abundance")

################################################################################
###### Incorporating Exogenous Time Series — Advanced Exercises ################
################################################################################

# Advanced Exercise 1: Two-predictor scenario — warming AND drought combined
# Using butterfly ARIMAX:
# (a) Create 4 combined scenarios: (+0°C, 100% rain), (+1°C, 90% rain),
#     (+2°C, 80% rain), (+3°C, 70% rain).
# (b) Forecast 24 months under each scenario.
# (c) Plot all four. Under which combined scenario is the species most at risk?
# (d) Compare the combined scenarios to the warming-only scenarios from Exercise 5.

# Advanced Exercise 2: Dynamic regression residuals diagnosis
# Using the butterfly ARIMAX model (abundance ~ temperature_c + rainfall_mm):
# (a) Extract both regression residuals (eta) and innovation residuals (epsilon).
# (b) Plot both residual series and their ACFs.
# (c) Are the regression residuals autocorrelated? Are the innovation residuals?
# (d) What does this tell you about the importance of including ARIMA errors?

# Advanced Exercise 3: Lagged predictors for river dissolved oxygen
# Water temperature on previous days may affect today's DO level.
# (a) Create lag-1 and lag-7 temperature variables in the river dataset.
# (b) Fit ARIMAX models: DO ~ temp (current), DO ~ temp_lag1, DO ~ temp_lag7.
# (c) Compare AICc values. Which lag structure is preferred?
# (d) Forecast 90 days ahead with the best model.

# Advanced Exercise 4: Quadratic temperature in wetland bird model
# Waterbird counts may have an optimal water level (too high = flooded roosts).
# (a) Add I(water_level_m^2) as a predictor to the wetland ARIMAX.
# (b) Compare AICc: linear vs quadratic water level model.
# (c) Plot the fitted relationship between water level and predicted count
#     (holding all other variables at mean values).

# Advanced Exercise 5: Full ecological impact report
# Using the butterfly ARIMAX model (abundance ~ temperature_c + rainfall_mm):
# (a) Create a 2x2 scenario grid: {+0, +2}°C warming x {100%, 70%} rainfall.
# (b) Forecast all four scenarios for 24 months.
# (c) For each scenario, report: mean summer abundance, peak month, and
#     the percentage of months below MIN_VIABLE_ABUNDANCE.
# (d) Which scenario combination represents the greatest conservation risk?

################################################################################
######### Incorporating Exogenous Time Series — Answers #########################
################################################################################

# Answer 1:
river_ts <- load_ecological_tsibble(
  file_path  = "river_water_quality.csv",
  index_col  = "date",
  index_type = "date"
)

# (a) Fit ARIMAX
river_arimax_fit <- river_ts %>%
  model(arimax = ARIMA(dissolved_oxygen_mgl ~ water_temperature_c))

# (b) Report
report(river_arimax_fit)
# The temperature coefficient should be negative: warmer water dissolves less oxygen.

# (c) Interpretation: Each 1°C increase in water temperature decreases
# dissolved oxygen by approximately [coefficient] mg/L — consistent with
# Henry's Law (gas solubility decreases with temperature).

# (d) Diagnostics
river_arimax_fit %>%
  gg_tsresiduals() +
  labs(title = "ARIMAX Diagnostics: River DO ~ Temperature")

augment(river_arimax_fit) %>%
  features(.innov, ljung_box, lag = 14) %>%
  print_output("Ljung-Box: River ARIMAX residuals")

# Answer 2:
wetland_ts <- load_ecological_tsibble(
  file_path  = "wetland_bird_counts.csv",
  index_col  = "date",
  index_type = "yearweek"
)

# (a) Fit ARIMAX
wetland_arimax_fit <- wetland_ts %>%
  model(
    arima_only = ARIMA(total_waterbirds),
    arimax     = ARIMA(total_waterbirds ~ water_level_m)
  )

# (b) Report
wetland_arimax_fit %>%
  dplyr::select(arimax) %>%
  report()

# (c) Compare AICc
glance(wetland_arimax_fit) %>%
  dplyr::select(.model, AICc) %>%
  dplyr::arrange(AICc) %>%
  print_output("ARIMA vs ARIMAX: Wetland birds AICc")

# (d) Accuracy comparison
wetland_train_ts <- wetland_ts %>% dplyr::filter(year(date) <= 2023)
wetland_test_ts  <- wetland_ts %>% dplyr::filter(year(date) > 2023)

wetland_split_fit <- wetland_train_ts %>%
  model(
    arima_only = ARIMA(total_waterbirds),
    arimax     = ARIMA(total_waterbirds ~ water_level_m)
  )

fc_wetland_arima  <- wetland_split_fit %>%
  dplyr::select(arima_only) %>%
  forecast(h = nrow(wetland_test_ts))

fc_wetland_arimax <- wetland_split_fit %>%
  dplyr::select(arimax) %>%
  forecast(new_data = wetland_test_ts)

dplyr::bind_rows(
  accuracy(fc_wetland_arima,  wetland_ts),
  accuracy(fc_wetland_arimax, wetland_ts)
) %>%
  dplyr::select(.model, RMSE, MAE, MASE) %>%
  dplyr::arrange(RMSE) %>%
  print_output("ARIMA vs ARIMAX forecast accuracy: Wetland birds")

# Answer 3:
river_train_ts <- river_ts %>% dplyr::filter(year(date) <= 2023)

river_arimax_train_fit <- river_train_ts %>%
  model(arimax = ARIMA(dissolved_oxygen_mgl ~ water_temperature_c))

# (a) Normal summer scenario (15°C)
future_normal_summer <- new_data(river_train_ts, 90) %>%
  dplyr::mutate(water_temperature_c = 15)

# (b) Heat wave scenario (22°C)
future_heat_wave <- new_data(river_train_ts, 90) %>%
  dplyr::mutate(water_temperature_c = 22)

fc_normal_summer <- forecast(river_arimax_train_fit,
                             new_data = future_normal_summer) %>%
  dplyr::mutate(Scenario = "Normal Summer (15\u00b0C)")

fc_heat_wave <- forecast(river_arimax_train_fit,
                         new_data = future_heat_wave) %>%
  dplyr::mutate(Scenario = "Heat Wave (22\u00b0C)")

# (c) Plot with critical DO threshold
dplyr::bind_rows(as_tibble(fc_normal_summer), as_tibble(fc_heat_wave)) %>%
  ggplot(aes(x = date, y = .mean, colour = Scenario)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = CRITICAL_DO_THRESHOLD,
             linetype = "dashed", colour = "red") +
  annotate("text",
           x     = min(future_normal_summer$date) + 5,
           y     = CRITICAL_DO_THRESHOLD - 0.3,
           label = paste0("Critical DO threshold (", CRITICAL_DO_THRESHOLD, " mg/L)"),
           colour = "red",
           hjust  = 0) +
  scale_colour_manual(values = pallete[1:2]) +
  theme_new() +
  labs(title    = "River DO Forecast — Temperature Scenarios",
       subtitle = "Red dashed line = critical threshold for aquatic life",
       y        = "Dissolved Oxygen (mg/L)",
       colour   = "Scenario")

# Answer 4:
butterfly_multi_predictor_fit <- butterfly_ts %>%
  model(
    both_predictors  = ARIMA(abundance ~ temperature_c + rainfall_mm),
    temperature_only = ARIMA(abundance ~ temperature_c),
    rainfall_only    = ARIMA(abundance ~ rainfall_mm)
  )

glance(butterfly_multi_predictor_fit) %>%
  dplyr::arrange(AICc) %>%
  dplyr::select(.model, AICc, AIC, BIC) %>%
  print_output("Model comparison: single vs multiple predictors")

# The model with lowest AICc is preferred. If temperature_only has lower
# AICc than both_predictors, rainfall adds noise rather than signal.

# Answer 5:
bf_arimax_train_fit <- train_butterfly_ts %>%
  model(arimax = ARIMA(abundance ~ temperature_c + rainfall_mm))

# (a) Create all four warming scenarios using make_climate_scenario()
fc_warm_0c <- forecast(bf_arimax_train_fit,
                       new_data = make_climate_scenario(
                         training_ts  = train_butterfly_ts,
                         horizon      = FORECAST_HORIZON_LONG,
                         monthly_temp = monthly_mean_temperature,
                         monthly_rain = monthly_mean_rainfall,
                         temp_offset  = 0)) %>%
  dplyr::mutate(Scenario = "Baseline")

fc_warm_1c <- forecast(bf_arimax_train_fit,
                       new_data = make_climate_scenario(
                         training_ts  = train_butterfly_ts,
                         horizon      = FORECAST_HORIZON_LONG,
                         monthly_temp = monthly_mean_temperature,
                         monthly_rain = monthly_mean_rainfall,
                         temp_offset  = 1)) %>%
  dplyr::mutate(Scenario = "+1\u00b0C")

fc_warm_2c <- forecast(bf_arimax_train_fit,
                       new_data = make_climate_scenario(
                         training_ts  = train_butterfly_ts,
                         horizon      = FORECAST_HORIZON_LONG,
                         monthly_temp = monthly_mean_temperature,
                         monthly_rain = monthly_mean_rainfall,
                         temp_offset  = 2)) %>%
  dplyr::mutate(Scenario = "+2\u00b0C")

fc_warm_3c <- forecast(bf_arimax_train_fit,
                       new_data = make_climate_scenario(
                         training_ts  = train_butterfly_ts,
                         horizon      = FORECAST_HORIZON_LONG,
                         monthly_temp = monthly_mean_temperature,
                         monthly_rain = monthly_mean_rainfall,
                         temp_offset  = 3)) %>%
  dplyr::mutate(Scenario = "+3\u00b0C")

# (c) Plot all four scenarios
dplyr::bind_rows(
  as_tibble(fc_warm_0c), as_tibble(fc_warm_1c),
  as_tibble(fc_warm_2c), as_tibble(fc_warm_3c)
) %>%
  ggplot(aes(x = date, y = .mean, colour = Scenario)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = pallete[1:4]) +
  theme_new() +
  labs(title    = "Butterfly Abundance Under Climate Warming Scenarios",
       subtitle = "Rainfall held constant at historical monthly averages",
       y        = "Predicted Abundance (count)",
       colour   = "Warming Scenario")

# (d) Compare peak summer abundance across scenarios
peak_abundance_comparison <- dplyr::bind_rows(
  as_tibble(fc_warm_0c), as_tibble(fc_warm_1c),
  as_tibble(fc_warm_2c), as_tibble(fc_warm_3c)
) %>%
  dplyr::group_by(Scenario) %>%
  dplyr::summarise(peak_abundance = max(.mean, na.rm = TRUE)) %>%
  dplyr::mutate(pct_change_from_baseline = (
    peak_abundance / peak_abundance[Scenario == "Baseline"] - 1
  ) * 100)

print_output(peak_abundance_comparison,
             "Peak Summer Abundance Change Under Warming Scenarios")

# Answer 6:
fc_combined_0_100 <- forecast(bf_arimax_train_fit,
                               new_data = make_climate_scenario(
                                 training_ts = train_butterfly_ts,
                                 horizon = FORECAST_HORIZON_LONG,
                                 monthly_temp = monthly_mean_temperature,
                                 monthly_rain = monthly_mean_rainfall,
                                 temp_offset = 0, rain_scale = 1)) %>%
  dplyr::mutate(Scenario = "+0\u00b0C, 100% rain")

fc_combined_1_90  <- forecast(bf_arimax_train_fit,
                               new_data = make_climate_scenario(
                                 training_ts = train_butterfly_ts,
                                 horizon = FORECAST_HORIZON_LONG,
                                 monthly_temp = monthly_mean_temperature,
                                 monthly_rain = monthly_mean_rainfall,
                                 temp_offset = 1, rain_scale = 0.9)) %>%
  dplyr::mutate(Scenario = "+1\u00b0C, 90% rain")

fc_combined_2_80  <- forecast(bf_arimax_train_fit,
                               new_data = make_climate_scenario(
                                 training_ts = train_butterfly_ts,
                                 horizon = FORECAST_HORIZON_LONG,
                                 monthly_temp = monthly_mean_temperature,
                                 monthly_rain = monthly_mean_rainfall,
                                 temp_offset = 2, rain_scale = 0.8)) %>%
  dplyr::mutate(Scenario = "+2\u00b0C, 80% rain")

fc_combined_3_70  <- forecast(bf_arimax_train_fit,
                               new_data = make_climate_scenario(
                                 training_ts = train_butterfly_ts,
                                 horizon = FORECAST_HORIZON_LONG,
                                 monthly_temp = monthly_mean_temperature,
                                 monthly_rain = monthly_mean_rainfall,
                                 temp_offset = 3, rain_scale = 0.7)) %>%
  dplyr::mutate(Scenario = "+3\u00b0C, 70% rain")

dplyr::bind_rows(
  as_tibble(fc_combined_0_100), as_tibble(fc_combined_1_90),
  as_tibble(fc_combined_2_80),  as_tibble(fc_combined_3_70)
) %>%
  ggplot(aes(x = date, y = .mean, colour = Scenario)) +
  geom_line(linewidth = 1) +
  scale_colour_manual(values = pallete[1:4]) +
  theme_new() +
  labs(title    = "Butterfly Abundance — Combined Warming + Drought Scenarios",
       subtitle = "Each scenario combines temperature increase with rainfall reduction",
       y        = "Predicted Abundance (count)",
       colour   = "Combined Scenario")

# Answer 7:
# (a) Extract both types of residuals from the ARIMAX model
both_residuals <- dplyr::bind_rows(
  `Regression residuals (eta)` =
    as_tibble(residuals(butterfly_arimax_fit %>% dplyr::select(arimax),
                        type = "regression")),
  `ARIMA innovations (epsilon)` =
    as_tibble(residuals(butterfly_arimax_fit %>% dplyr::select(arimax),
                        type = "innovation")),
  .id = "type"
)

# (b) Plot both residual series
both_residuals %>%
  ggplot(aes(x = date, y = .resid)) +
  geom_line() +
  facet_grid(vars(type), scales = "free_y") +
  theme_new() +
  labs(title    = "ARIMAX Residuals: Regression (eta) vs Innovations (epsilon)",
       subtitle = "eta should show autocorrelation; epsilon should be white noise",
       y        = "Residual")

# (c) The regression residuals (eta) are autocorrelated because they contain
# the temporal pattern not captured by temperature and rainfall alone.
# The innovation residuals (epsilon) should be white noise if the ARIMA
# component successfully absorbed that remaining autocorrelation.

# (d) This demonstrates WHY we need ARIMA errors (not just OLS):
# OLS residuals would be equivalent to eta — autocorrelated and violating
# the independence assumption. ARIMA errors correct for this, giving valid
# inference on the ecological coefficients.
