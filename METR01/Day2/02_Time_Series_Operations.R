####################################################################################################
###
### File:    02_Time_Series_Operations.R
### Purpose: Examples and exercises for Time Series Operations
### Authors: Gabriel Rodrigues Palma
### Date:    17/06/25
###
####################################################################################################
# load packages -----
source('00_source.r')

################################################################################
######################### Time Series Operations ##############################
################################################################################
# Before we can model a time series, we need to understand its components
# and transform it into a form suitable for analysis. This script covers:
#   1. Stationarity — the mathematical foundation of time series modelling
#   2. Differencing — removing trends and seasonal patterns
#   3. Decomposition — separating trend, seasonal, and remainder components
#   4. Moving averages — smoothing to reveal the underlying trend
#
# A stationary time series has: constant mean, constant variance, and
# autocorrelation that depends only on the lag (not on time itself).
# Most forecasting methods require (or assume) stationarity, so testing
# and achieving it is the critical first step in any analysis pipeline.

################################################################################
############### Example 1: Visual Stationarity Assessment ######################
################################################################################

# The first step is always visual: does the series look stationary?
# We compare a clearly non-stationary ecological series with white noise
# (the canonical stationary series) to calibrate our visual intuition.

# Load butterfly abundance data — a non-stationary ecological series
butterfly_ts <- load_ecological_tsibble(
  file_path  = "input_data/butterfly_meadow_monitoring.csv",
  index_col  = "date",
  index_type = "yearmonth"
)

# Plot 1: Butterfly abundance — clearly non-stationary
# (seasonal pattern + upward trend = changing mean over time)
plot_abundance_raw <- autoplot(butterfly_ts, abundance) +
  theme_new() +
  labs(title    = "Butterfly Abundance — Non-stationary",
       subtitle = "Seasonal pattern + trend: mean changes over time",
       y        = "Abundance (count)")

# Plot 2: White noise — the textbook example of stationarity
# Each value is drawn independently from N(0,1) with no temporal structure.
set.seed(42)
white_noise_ts <- tsibble(
  Time  = 1:200,
  Value = rnorm(200, mean = 0, sd = 1),
  index = Time
)

plot_white_noise <- autoplot(white_noise_ts, Value) +
  theme_new() +
  labs(title    = "White Noise — Stationary",
       subtitle = "Constant mean (0), constant variance, no autocorrelation",
       y        = "Value")

plot_abundance_raw / plot_white_noise

# Interpretation: The butterfly series has a clear seasonal cycle and a rising
# trend — its mean changes with time. White noise fluctuates randomly around
# zero with no pattern — it IS stationary.

################################################################################
############### Example 2: KPSS Test for Stationarity ##########################
################################################################################

# The KPSS test (Kwiatkowski-Phillips-Schmidt-Shin) formally tests
# H0: the series is stationary vs H1: the series has a unit root (non-stationary)
# A small p-value (< STATIONARITY_ALPHA) means we reject stationarity.
# We use the constant STATIONARITY_ALPHA = 0.05 defined in 00_source.r.

# Test butterfly abundance
kpss_butterfly <- butterfly_ts %>%
  features(abundance, unitroot_kpss)

print_output(kpss_butterfly, "KPSS test: Butterfly abundance")

# How many first-order differences are needed to achieve stationarity?
ndiffs_butterfly <- butterfly_ts %>%
  features(abundance, unitroot_ndiffs)

print_output(ndiffs_butterfly, "Number of first differences needed: Butterfly abundance")

# How many seasonal differences are needed?
nsdiffs_butterfly <- butterfly_ts %>%
  features(abundance, unitroot_nsdiffs)

print_output(nsdiffs_butterfly, "Number of seasonal differences needed: Butterfly abundance")

# Test the CO2 series — strongly non-stationary due to global trend
co2_ts <- as_tsibble(co2)

kpss_co2 <- co2_ts %>%
  features(value, unitroot_kpss)

print_output(kpss_co2, "KPSS test: CO2 concentration")
# Expected: p-value < STATIONARITY_ALPHA → strongly non-stationary

################################################################################
############### Example 3: First-Order Differencing ############################
################################################################################

# First differencing computes: y'_t = y_t - y_{t-1}
# This removes a linear trend. If the original series has a constant rate
# of increase, differencing turns it into a stationary series around zero.
# Think of it as replacing "the level" with "the change" — a common
# ecological application is going from population size to growth rate.

# Irish exports — a classic trending series
ireland_exports_ts <- global_economy %>%
  dplyr::filter(Country == "Ireland") %>%
  dplyr::mutate(gdp_per_capita = GDP / Population)

# Original series: clear upward trend → non-stationary
plot_exports_raw <- ireland_exports_ts %>%
  autoplot(Exports) +
  theme_new() +
  labs(y     = "% of GDP",
       title = "Irish Exports — Original (Non-stationary)")

# First difference: trend removed → near-stationary
plot_exports_diff <- ireland_exports_ts %>%
  dplyr::mutate(diff_exports = difference(Exports)) %>%
  autoplot(diff_exports) +
  theme_new() +
  labs(y        = "Change in % of GDP",
       title    = "Irish Exports — First Difference",
       subtitle = "y'_t = y_t - y_{t-1}")

plot_exports_raw / plot_exports_diff

# Verify stationarity after differencing
ireland_exports_ts %>%
  dplyr::mutate(diff_exports = difference(Exports)) %>%
  features(diff_exports, unitroot_kpss) %>%
  print_output("KPSS test after first differencing: Irish Exports")
# Expected: p-value > STATIONARITY_ALPHA → now stationary!

################################################################################
############### Example 4: Seasonal Differencing ###############################
################################################################################

# Seasonal differencing: y'_t = y_t - y_{t-m}
# where m is the seasonal period (e.g., m = 12 for monthly data).
# This removes the periodic seasonal pattern by comparing each observation
# to the same season in the previous year — an intuitive ecological operation
# (e.g., comparing this July's butterfly count to last July's).

# Australian beer production — strong quarterly seasonality (m = 4)
beer_ts <- aus_production %>%
  dplyr::filter(year(Quarter) >= 1992) %>%
  dplyr::select(Quarter, Beer)

# Original series — strong quarterly seasonality
plot_beer_raw <- autoplot(beer_ts, Beer) +
  theme_new() +
  labs(title = "Australian Beer Production — Original",
       y     = "Megalitres")

# Seasonal difference (lag = 4 for quarterly data)
plot_beer_sdiff <- beer_ts %>%
  dplyr::mutate(seasonal_diff_beer = difference(Beer, lag = 4)) %>%
  autoplot(seasonal_diff_beer) +
  theme_new() +
  labs(title    = "Beer Production — Seasonally Differenced",
       subtitle = "y'_t = y_t - y_{t-4} (removes quarterly seasonality)",
       y        = "Change in Megalitres")

# Sometimes we need BOTH seasonal and first-order differencing
# when the series has both trend and seasonality that must be removed.
plot_beer_double_diff <- beer_ts %>%
  dplyr::mutate(double_diff_beer = difference(Beer, lag = 4) %>% difference()) %>%
  autoplot(double_diff_beer) +
  theme_new() +
  labs(title    = "Beer Production — Seasonal + First Difference",
       subtitle = "First seasonal (lag=4), then first-order",
       y        = "Doubly Differenced")

plot_beer_raw / plot_beer_sdiff / plot_beer_double_diff

# Ecological application: monthly butterfly abundance (m = 12)
butterfly_ts %>%
  dplyr::mutate(seasonal_diff_abundance = difference(abundance, lag = 12)) %>%
  autoplot(seasonal_diff_abundance) +
  theme_new() +
  labs(title    = "Butterfly Abundance — Seasonally Differenced (lag = 12)",
       subtitle = "Change from same month last year: removes annual cycle",
       y        = "Change from Same Month Last Year")

################################################################################
############### Example 5: Classical Decomposition #############################
################################################################################

# Decomposition separates a time series into three components:
#   ADDITIVE:       y_t = T_t + S_t + R_t
#   MULTIPLICATIVE: y_t = T_t × S_t × R_t
# where T = trend-cycle, S = seasonal, R = remainder (unexplained variation)
#
# ADDITIVE is appropriate when the amplitude of seasonal fluctuations
# is roughly constant regardless of the trend level.
# MULTIPLICATIVE is appropriate when seasonal amplitude grows proportionally
# with the trend (common in economic data; sometimes in populations).

# US retail employment — the standard fpp3 textbook example
us_retail_employment_ts <- us_employment %>%
  dplyr::filter(year(Month) >= 1990, Title == "Retail Trade") %>%
  dplyr::select(-Series_ID)

# Additive classical decomposition
us_retail_employment_ts %>%
  model(
    classical_decomposition(Employed, type = "additive")
  ) %>%
  components() %>%
  autoplot() +
  theme_new() +
  labs(title = "Classical Additive Decomposition — US Retail Employment")

# Multiplicative classical decomposition
# Use this when seasonal variation grows proportionally with the level
us_retail_employment_ts %>%
  model(
    classical_decomposition(Employed, type = "multiplicative")
  ) %>%
  components() %>%
  autoplot() +
  theme_new() +
  labs(title = "Classical Multiplicative Decomposition — US Retail Employment")

# Key limitation: classical decomposition cannot estimate trend at the start
# and end of the series (moving average window), and assumes the seasonal
# component is constant across the entire observation period.

################################################################################
############### Example 6: STL Decomposition ###################################
################################################################################

# STL = Seasonal and Trend decomposition using Loess
# Advantages over classical decomposition:
# - Handles any type of seasonality (not just monthly/quarterly)
# - The seasonal component can CHANGE over time (important for phenology)
# - Robust to outliers (e.g., single pollution events in water quality data)
# - User controls the smoothness of trend and seasonal components

# STL on US retail employment
dcmp_stl_employment <- us_retail_employment_ts %>%
  model(stl = STL(Employed))

# View all four decomposition components
components(dcmp_stl_employment) %>%
  autoplot() +
  theme_new() +
  labs(title = "STL Decomposition — US Retail Employment")

# Overlay STL trend on original data to show the trend extraction
components(dcmp_stl_employment) %>%
  as_tsibble() %>%
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y = trend), colour = pallete[1], linewidth = 1) +
  theme_new() +
  labs(y     = "Persons (thousands)",
       title = "US Retail Employment with STL Trend (green)")

# Seasonally adjusted series — removes the seasonal component to reveal
# underlying trend and irregular movements. Used in official statistics.
components(dcmp_stl_employment) %>%
  as_tsibble() %>%
  autoplot(Employed, colour = "gray") +
  geom_line(aes(y = season_adjust), colour = pallete[2], linewidth = 1) +
  theme_new() +
  labs(y     = "Persons (thousands)",
       title = "Seasonally Adjusted US Retail Employment (red)")

# Ecological application: STL on butterfly abundance
dcmp_stl_butterfly <- butterfly_ts %>%
  model(stl = STL(abundance))

components(dcmp_stl_butterfly) %>%
  autoplot() +
  theme_new() +
  labs(title    = "STL Decomposition — Butterfly Abundance",
       subtitle = "Trend shows gradual population recovery")

# Interpretation: The trend component reveals a gentle upward trajectory
# in butterfly abundance — possibly reflecting habitat improvement or
# warming temperatures. The seasonal component peaks in summer as expected.
# The remainder shows unexplained variation (stochastic events: unusual weather,
# counting errors, short-term habitat disturbance).

################################################################################
############### Example 7: X-11 and SEATS Decomposition ########################
################################################################################

# X-11 (developed by US Census Bureau) and SEATS (Bank of Spain) are more
# sophisticated decomposition methods using iterative moving averages and
# seasonal adjustment filters. Both are available via the seasonal package.
# They are more flexible than classical decomposition and widely used in
# national statistical offices for economic time series.

# X-11 decomposition
x11_decomp <- us_retail_employment_ts %>%
  model(x11 = X_13ARIMA_SEATS(Employed ~ x11())) %>%
  components()

autoplot(x11_decomp) +
  theme_new() +
  labs(title    = "X-11 Decomposition — US Retail Employment",
       subtitle = "More refined than classical decomposition")

# SEATS decomposition — signal extraction in ARIMA time series
seats_decomp <- us_retail_employment_ts %>%
  model(seats = X_13ARIMA_SEATS(Employed ~ seats())) %>%
  components()

autoplot(seats_decomp) +
  theme_new() +
  labs(title = "SEATS Decomposition — US Retail Employment")

# X-11 allows the seasonal component to vary slowly over time (similar to STL),
# but uses a different estimation approach based on a cascade of moving averages.
# SEATS uses ARIMA models explicitly to estimate each component.

################################################################################
############### Example 8: Moving Averages #####################################
################################################################################

# Moving averages smooth the series to reveal the underlying trend by averaging
# nearby observations. A k-MA takes the mean of k consecutive observations.
# Higher k = smoother trend but more data lost at the endpoints.
# For seasonal data: choose k equal to the seasonal period to eliminate
# the seasonal component and expose the trend-cycle directly.

# Irish exports with various moving average orders
ireland_exports_vector <- ireland_exports_ts %>%
  dplyr::pull(Exports)

ma5_values <- slider::slide_dbl(ireland_exports_vector, mean,
                                .before = 2, .after = 2,
                                .complete = TRUE)

# Compare different MA orders — higher order = smoother trend
ireland_exports_long_ts <- ireland_exports_ts %>%
  dplyr::mutate(
    `3-MA` = forecast::ma(Exports, order = 3),
    `5-MA` = forecast::ma(Exports, order = 5),
    `7-MA` = forecast::ma(Exports, order = 7),
    `9-MA` = forecast::ma(Exports, order = 9)
  ) %>%
  dplyr::select(Year, Exports, `3-MA`, `5-MA`, `7-MA`, `9-MA`) %>%
  tidyr::pivot_longer(3:6,
                      names_to  = "ma_order",
                      values_to = "ma_value")

ireland_exports_long_ts %>%
  ggplot(aes(x = Year, y = Exports)) +
  geom_line() +
  geom_line(aes(y = ma_value), col = pallete[2], linewidth = 1) +
  facet_wrap(~ ma_order) +
  theme_new() +
  labs(y        = "% of GDP",
       title    = "Irish Exports with Moving Averages of Different Orders",
       subtitle = "Higher order = smoother trend estimate")

# Interpretation: Higher-order moving averages produce smoother trends but
# lose more data at the endpoints. For seasonal data, use an MA whose order
# matches the seasonal period (e.g., 12-MA for monthly) to remove seasonality.

# 2×12 MA for monthly seasonal data — the centred moving average
# A 12-MA averages over a full year eliminating the annual seasonal cycle.
# The 2×12-MA re-centres it to avoid the one-month offset of a 12-MA.
us_retail_with_ma <- us_retail_employment_ts %>%
  dplyr::mutate(
    `12-MA`   = slider::slide_dbl(Employed, mean,
                                   .before = 5, .after = 6, .complete = TRUE),
    `2x12-MA` = slider::slide_dbl(`12-MA`, mean,
                                   .before = 1, .after = 0, .complete = TRUE)
  )

us_retail_with_ma %>%
  autoplot(Employed, col = "gray") +
  geom_line(aes(y = `2x12-MA`), colour = pallete[2], linewidth = 1) +
  theme_new() +
  labs(y        = "Persons (thousands)",
       title    = "US Retail Employment with 2×12 Moving Average",
       subtitle = "2×12-MA in red — eliminates monthly seasonality")

################################################################################
#################### Time Series Operations Exercises ##########################
################################################################################

# Exercise 1: Stationarity assessment of river water quality
# Load "river_water_quality.csv" and create a daily tsibble.
# (a) Plot the dissolved_oxygen_mgl series.
# (b) Perform a KPSS test — is it stationary?
# (c) How many differences are needed (unitroot_ndiffs)?
# (d) Apply the required differencing and plot the result.
# (e) Verify stationarity of the differenced series with a second KPSS test.

# Exercise 2: Seasonal differencing on wetland bird data
# Load "wetland_bird_counts.csv" as a weekly tsibble.
# (a) Plot the total_waterbirds series.
# (b) Apply seasonal differencing with lag = 52 (annual season in weekly data).
# (c) Does the differenced series appear stationary? Verify with KPSS.
# (d) If not, apply an additional first-order difference.

# Exercise 3: Classical decomposition of atmospheric CO2
# Using the built-in co2 dataset (as_tsibble(co2)):
# (a) Apply additive classical decomposition.
# (b) Apply multiplicative classical decomposition.
# (c) Which decomposition type is more appropriate? Justify your answer.
# (d) Extract and plot only the trend component from the additive decomposition.

# Exercise 4: STL decomposition on forest canopy cover
# Load "forest_canopy_cover.csv" and filter to "Oak_Woodland" only.
# (a) Create a quarterly tsibble and apply STL decomposition.
# (b) Plot the four decomposition components.
# (c) What percentage of total variance is explained by the seasonal component?
#     (Hint: compare var(seasonal component) / var(original series))
# (d) Plot the seasonally adjusted series overlaid on the original.

# Exercise 5: Moving average comparison on butterfly abundance
# Using the butterfly_ts abundance column:
# (a) Compute 3-MA, 6-MA, and 12-MA smoothed series.
# (b) Plot all three MAs overlaid on the original data.
# (c) Which MA order best reveals the long-term trend? Why?
# (d) What happens to the endpoints as MA order increases?

################################################################################
#################### Time Series Operations — Advanced ########################
################################################################################
# Advanced examples demonstrate the strengths of STL over classical methods,
# combine multiple decomposition approaches for comparison, and explore
# higher-order differencing patterns in complex ecological datasets.

# Advanced Example 1: Detecting a changing seasonal component with STL
# The seasonal component in STL is allowed to evolve. For long ecological
# time series, this can reveal phenological shifts under climate change.
dcmp_adv_butterfly <- butterfly_ts %>%
  model(stl = STL(abundance ~ season(window = "periodic"),
                  robust = TRUE))
components(dcmp_adv_butterfly) %>%
  autoplot() +
  theme_new() +
  labs(title    = "Advanced Example 1: Robust STL on Butterfly Abundance",
       subtitle = "robust = TRUE protects against outlier years")

# Advanced Example 2: Variance stabilisation before decomposition
lambda_adv <- butterfly_ts %>%
  features(abundance, guerrero) %>%
  dplyr::pull(lambda_guerrero)

butterfly_ts %>%
  dplyr::mutate(bc_abundance = box_cox(abundance, lambda_adv)) %>%
  model(stl = STL(bc_abundance)) %>%
  components() %>%
  autoplot() +
  theme_new() +
  labs(title    = "Advanced Example 2: STL after Box-Cox Transformation",
       subtitle = paste0("lambda = ", round(lambda_adv, 2),
                         " stabilises variance before decomposition"))

# Advanced Example 3: KPSS test on multiple ecological series
ecological_series <- list(
  butterfly  = butterfly_ts %>% dplyr::rename(value = abundance),
  co2        = as_tsibble(co2) %>% dplyr::rename(value = value)
)

lapply(names(ecological_series), function(series_name) {
  kpss_result <- ecological_series[[series_name]] %>%
    features(value, unitroot_kpss)
  cat(series_name, ": kpss_stat =",
      round(kpss_result$kpss_stat, 3),
      ", p-value =", round(kpss_result$kpss_pvalue, 3), "\n")
})

# Advanced Example 4: Cascade differencing — when and why
# Some ecological series require d=2 differencing (acceleration)
# This is rare but can occur in strongly accelerating processes
beer_cascade <- aus_production %>%
  dplyr::filter(year(Quarter) >= 1992) %>%
  dplyr::select(Quarter, Beer) %>%
  dplyr::mutate(
    diff1 = difference(Beer),
    diff2 = difference(Beer, differences = 2)
  )

autoplot(beer_cascade, Beer) +
  theme_new() + labs(title = "Advanced Example 4a: Original Beer Production")
autoplot(beer_cascade, diff1) +
  theme_new() + labs(title = "Advanced Example 4b: First Difference")
autoplot(beer_cascade, diff2) +
  theme_new() + labs(title = "Advanced Example 4c: Second Difference")

# Advanced Example 5: Moving average centring for irregular seasons
# For seasonal periods that are odd (e.g., 7 days/week), a centred MA
# requires a 2×k-MA approach. Weekly data with annual seasonality (m=52)
# can be estimated using nested moving averages.
wetland_adv_ts <- load_ecological_tsibble(
  file_path  = "input_data/wetland_bird_counts.csv",
  index_col  = "date",
  index_type = "yearweek"
)

wetland_adv_ts %>%
  dplyr::mutate(
    ma13 = slider::slide_dbl(total_waterbirds, mean,
                              .before = 6, .after = 6, .complete = TRUE)
  ) %>%
  autoplot(total_waterbirds, colour = "gray") +
  geom_line(aes(y = ma13), colour = pallete[2], linewidth = 1) +
  theme_new() +
  labs(title    = "Advanced Example 5: 13-Week Moving Average",
       subtitle = "Smooths over a quarter; reveals seasonal trend",
       y        = "Total Waterbirds")

################################################################################
################ Time Series Operations — Advanced Exercises ####################
################################################################################

# Advanced Exercise 1: KPSS and differencing on red deer population
# Create the red_deer_population tsibble (from script 01 Example 2).
# (a) Test for stationarity using the KPSS test.
# (b) Apply first differencing and re-test.
# (c) Plot the original and differenced series side by side.
# (d) Interpret the differenced values ecologically (what do they represent?).

# Advanced Exercise 2: Comparing STL and classical decomposition on CO2
# Using co2_ts (as_tsibble(co2)):
# (a) Apply both STL and classical additive decomposition.
# (b) Extract the seasonal component from each.
# (c) Plot both seasonal components overlaid on the same axes.
# (d) Do they agree? Where do they differ most? What advantage does STL add?

# Advanced Exercise 3: Optimal Box-Cox lambda comparison
# For all three forest types (Oak_Woodland, Sitka_Spruce, Native_Mixed):
# (a) Compute the Guerrero lambda for each series.
# (b) Apply the appropriate Box-Cox transformation.
# (c) Refit STL decomposition on the transformed series.
# (d) Does transformation improve residual normality in the STL remainder?

# Advanced Exercise 4: Multiple differencing strategies for wetland birds
# For the wetland total_waterbirds series:
# (a) Try seasonal difference lag=52 only.
# (b) Try seasonal difference lag=52 followed by first difference.
# (c) Try first difference only.
# (d) Compare KPSS p-values for all three approaches. Which achieves stationarity
#     with the fewest differences (principle of parsimony)?

# Advanced Exercise 5: Build a complete EDA pipeline function
# Write a function explore_ecological_ts(ts_data, value_col) that:
# (a) Prints summarise_ts_features() output
# (b) Runs KPSS test and prints result
# (c) Creates and prints autoplot() of the series
# (d) Creates and prints the ACF plot (lag_max = 36)
# Test it on butterfly_ts using value_col = "abundance".

################################################################################
#################### Time Series Operations — Answers ##########################
################################################################################

# Answer 1:
river_ts <- load_ecological_tsibble(
  file_path  = "input_data/river_water_quality.csv",
  index_col  = "date",
  index_type = "date"
)

# (a) Plot
autoplot(river_ts, dissolved_oxygen_mgl) +
  theme_new() +
  labs(title = "Dissolved Oxygen — River Liffey",
       y     = "DO (mg/L)")

# (b) KPSS test
river_ts %>%
  features(dissolved_oxygen_mgl, unitroot_kpss) %>%
  print_output("KPSS test: Dissolved Oxygen")

# (c) Number of differences
river_ts %>%
  features(dissolved_oxygen_mgl, unitroot_ndiffs) %>%
  print_output("Number of differences needed: DO")

# (d) Apply differencing
river_ts %>%
  dplyr::mutate(diff_dissolved_oxygen = difference(dissolved_oxygen_mgl)) %>%
  autoplot(diff_dissolved_oxygen) +
  theme_new() +
  labs(title = "Dissolved Oxygen — First Difference",
       y     = "Change in DO (mg/L)")

# (e) Verify stationarity of differenced series
river_ts %>%
  dplyr::mutate(diff_dissolved_oxygen = difference(dissolved_oxygen_mgl)) %>%
  features(diff_dissolved_oxygen, unitroot_kpss) %>%
  print_output("KPSS test after differencing: Dissolved Oxygen")

# Answer 2:
wetland_ts <- load_ecological_tsibble(
  file_path  = "input_data/wetland_bird_counts.csv",
  index_col  = "date",
  index_type = "yearweek"
)

# (a) Plot
autoplot(wetland_ts, total_waterbirds) +
  theme_new() +
  labs(title = "Weekly Waterbird Counts — Shannon Wetland",
       y     = "Total Count")

# (b) Seasonal differencing (lag = 52)
wetland_sdiff_ts <- wetland_ts %>%
  dplyr::mutate(seasonal_diff_waterbirds = difference(total_waterbirds, lag = 52))

autoplot(wetland_sdiff_ts, seasonal_diff_waterbirds) +
  theme_new() +
  labs(title    = "Waterbird Counts — Seasonally Differenced (lag=52)",
       y        = "Change from Same Week Last Year")

# (c) KPSS test on seasonally differenced series
wetland_sdiff_ts %>%
  features(seasonal_diff_waterbirds, unitroot_kpss) %>%
  print_output("KPSS test after seasonal differencing")

# (d) If needed, apply additional first-order difference
wetland_ts %>%
  dplyr::mutate(double_diff_waterbirds = difference(total_waterbirds, lag = 52) %>%
                  difference()) %>%
  autoplot(double_diff_waterbirds) +
  theme_new() +
  labs(title = "Waterbird Counts — Double Differenced",
       y     = "Doubly Differenced Count")

# Answer 3:
co2_ts <- as_tsibble(co2)

# (a) Additive decomposition
dcmp_co2_additive <- co2_ts %>%
  model(classical_decomposition(value, type = "additive")) %>%
  components()

autoplot(dcmp_co2_additive) +
  theme_new() +
  labs(title = "Classical Additive Decomposition — CO2")

# (b) Multiplicative decomposition
dcmp_co2_multiplicative <- co2_ts %>%
  model(classical_decomposition(value, type = "multiplicative")) %>%
  components()

autoplot(dcmp_co2_multiplicative) +
  theme_new() +
  labs(title = "Classical Multiplicative Decomposition — CO2")

# (c) Additive is more appropriate because the seasonal amplitude (~6 ppm)
# remains approximately constant even as the CO2 level rises from 315 to 370 ppm.
# Multiplicative would imply the amplitude grows proportionally with level —
# which is not strongly supported by the data.

# (d) Extract trend from additive decomposition
dcmp_co2_additive %>%
  as_tsibble() %>%
  autoplot(trend) +
  theme_new() +
  labs(title = "Trend Component — Atmospheric CO2",
       y     = "CO2 (ppm)")

# Answer 4:
oak_woodland_ts <- load_ecological_tsibble(
  file_path  = "input_data/forest_canopy_cover.csv",
  index_col  = "date",
  index_type = "yearquarter",
  key_col    = "forest_type"
) %>%
  dplyr::filter(forest_type == "Oak_Woodland")

# (a) STL decomposition
dcmp_oak <- oak_woodland_ts %>%
  model(stl = STL(canopy_cover_pct))

# (b) Plot components
components(dcmp_oak) %>%
  autoplot() +
  theme_new() +
  labs(title = "STL Decomposition — Oak Woodland Canopy Cover")

# (c) Proportion of variance explained by seasonal component
oak_components <- components(dcmp_oak)
seasonal_variance <- var(oak_components$season_year, na.rm = TRUE)
total_variance    <- var(oak_components$canopy_cover_pct, na.rm = TRUE)
cat("Seasonal component explains",
    round(seasonal_variance / total_variance * 100, 1),
    "% of total variation\n")

# (d) Seasonally adjusted overlay
components(dcmp_oak) %>%
  as_tsibble() %>%
  autoplot(canopy_cover_pct, colour = "gray") +
  geom_line(aes(y = season_adjust), colour = pallete[1], linewidth = 1) +
  theme_new() +
  labs(title = "Oak Woodland — Original vs Seasonally Adjusted",
       y     = "Canopy Cover (%)")

# Answer 5:
butterfly_ma_ts <- butterfly_ts %>%
  dplyr::mutate(
    `3-MA`  = forecast::ma(abundance, order = 3),
    `6-MA`  = forecast::ma(abundance, order = 6),
    `12-MA` = forecast::ma(abundance, order = 12)
  )

# (b) Plot overlaid
butterfly_ma_long <- butterfly_ma_ts %>%
  as_tibble() %>%
  dplyr::select(date, abundance, `3-MA`, `6-MA`, `12-MA`) %>%
  tidyr::pivot_longer(c(`3-MA`, `6-MA`, `12-MA`),
                      names_to  = "ma_order",
                      values_to = "ma_value")

ggplot(butterfly_ma_long, aes(x = date)) +
  geom_line(aes(y = abundance), colour = "gray", alpha = 0.6) +
  geom_line(aes(y = ma_value, colour = ma_order), linewidth = 1) +
  scale_colour_manual(values = pallete[1:3]) +
  theme_new() +
  labs(title  = "Butterfly Abundance with Moving Averages",
       y      = "Abundance (count)",
       colour = "MA Order")

# (c) The 12-MA best reveals the long-term trend because it averages over
# a full year, thereby eliminating the annual seasonal cycle entirely.

# (d) As MA order increases, more data points are lost at the beginning
# and end of the series (NA values). A 12-MA loses 6 points at each end.

# Answer 6:
red_deer_ts <- tsibble(
  Year  = 2010:2024,
  Count = c(120, 135, 142, 155, 148, 163, 171, 180, 175, 190,
            198, 205, 215, 220, 228),
  index = Year
)

# (a) KPSS test
red_deer_ts %>%
  features(Count, unitroot_kpss) %>%
  print_output("KPSS test: Red Deer Population")

# (b) First differencing and re-test
red_deer_diff_ts <- red_deer_ts %>%
  dplyr::mutate(annual_growth = difference(Count))

red_deer_diff_ts %>%
  features(annual_growth, unitroot_kpss) %>%
  print_output("KPSS test after differencing: Red Deer")

# (c) Side-by-side plots
plot_deer_raw  <- autoplot(red_deer_ts, Count) +
  theme_new() +
  labs(title = "Red Deer — Original", y = "Population Count")

plot_deer_diff <- autoplot(red_deer_diff_ts, annual_growth) +
  theme_new() +
  labs(title = "Red Deer — First Difference", y = "Annual Change")

plot_deer_raw / plot_deer_diff

# (d) The differenced values represent annual population growth (net change).
# Positive values indicate population increase; negative values indicate decline.

# Answer 7:
stl_co2_components <- co2_ts %>%
  model(stl = STL(value)) %>%
  components() %>%
  dplyr::select(index, season_year) %>%
  dplyr::mutate(method = "STL")

classical_co2_components <- co2_ts %>%
  model(classical_decomposition(value, type = "additive")) %>%
  components() %>%
  dplyr::select(index, seasonal) %>%
  dplyr::rename(season_year = seasonal) %>%
  dplyr::mutate(method = "Classical")

dplyr::bind_rows(stl_co2_components, classical_co2_components) %>%
  ggplot(aes(x = index, y = season_year, colour = method)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = pallete[1:2]) +
  theme_new() +
  labs(title    = "STL vs Classical Seasonal Component — CO2",
       subtitle = "STL can adapt to slowly changing seasonal patterns",
       y        = "Seasonal Component",
       colour   = "Method")
# The two methods agree closely. STL's advantage is visible where the seasonal
# amplitude may have shifted slightly over 40 years of measurements.
