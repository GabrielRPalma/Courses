####################################################################################################
###
### File:    02_Vector_Autoregressive_Models.R
### Purpose: Examples and exercises for Vector Autoregressive Models (VAR)
### Authors: Gabriel Rodrigues Palma
### Date:    17/06/25
###
####################################################################################################
# load packages -----
source('00_source.r')

################################################################################
################### Vector Autoregressive Models (VAR) #########################
################################################################################

# In ecology, species do not exist in isolation. Predator and prey populations
# influence each other; temperature affects both plant growth and herbivore
# abundance; competing species suppress each other.
#
# VAR models capture these BIDIRECTIONAL relationships between multiple time
# series simultaneously. Each variable is modelled as a linear function of
# its own past values AND the past values of ALL other variables.
#
# VAR(1) model for two series (y1, y2):
#   [y1_t]   [c1]   [phi_11  phi_12] [y1_{t-1}]   [e1_t]
#   [y2_t] = [c2] + [phi_21  phi_22] [y2_{t-1}] + [e2_t]
#
# Compact notation:
#   y_t = c + A_1 * y_{t-1} + ... + A_p * y_{t-p} + epsilon_t
#

################################################################################
############## Example 1: Predator-Prey Data — Ladybird and Aphids ############
################################################################################

# Aphids are prey; ladybirds (Coccinellidae) are predators. The interaction
# creates lagged cross-dependencies:
#   - High aphid counts → high ladybird counts (food drives predator growth)
#   - High ladybird counts → low aphid counts (predation effect)

ladybird_aphid <- read.csv("input_data/ladybird_aphid_dynamics.csv")

cat("Ladybird-aphid data dimensions:", dim(ladybird_aphid), "\n")
head(ladybird_aphid)

# Plot both species together
p1 <- ladybird_aphid %>%
  pivot_longer(cols = c(aphid_count, ladybird_count),
               names_to = "species", values_to = "count") %>%
  ggplot(aes(x = as.Date(date), y = count, colour = species)) +
  geom_line(alpha = 0.7) +
  scale_colour_manual(values = pallete[c(1, 2)],
                      labels = c("Aphids", "Ladybirds")) +
  labs(title = "Predator-Prey Dynamics: Ladybird and Aphid Populations",
       subtitle = "Classic ecological interaction — ladybird peaks lag behind aphid peaks",
       x = "Date", y = "Count", colour = "Species") +
  theme_new()
print(p1)

# Cross-correlation: what lag shows the strongest correlation?
ccf(ladybird_aphid$aphid_count,
    ladybird_aphid$ladybird_count,
    lag.max = 20, plot = TRUE,
    main = "CCF: Aphid vs Ladybird Counts")
# Positive lag → ladybird counts follow aphid counts

################################################################################
################## Example 2: Fitting a VAR Model with vars::VAR() ############
################################################################################

# vars::VAR() requires a numeric matrix or ts object.
# Prepare the two species counts as a multivariate time series.

var_ts <- ts(
  ladybird_aphid %>% dplyr::select(aphid_count, ladybird_count),
  start = c(2018, 1), frequency = 52
)

# Fit VAR(1) — one lag
var_fit1 <- vars::VAR(var_ts, p = 1, type = "const")
summary(var_fit1)

# The summary shows TWO equations:
#   aphid_count_t    = c1 + phi_11 * aphid_{t-1} + phi_12 * ladybird_{t-1} + e1
#   ladybird_count_t = c2 + phi_21 * aphid_{t-1} + phi_22 * ladybird_{t-1} + e2
#
# phi_12 < 0 → ladybirds reduce aphid growth (predation)
# phi_21 > 0 → aphids increase ladybird growth (food source)

cat("\n--- VAR(1) Coefficient Matrix ---\n")
print(coef(var_fit1))

################################################################################
################# Example 3: Selecting Lag Order with VARselect() #############
################################################################################

# VARselect() computes information criteria for lag orders 1 to lag.max.
# In ecology, BIC (SC) is preferred — it penalises complexity more heavily,
# reducing overfitting on noisy ecological data.

var_select <- vars::VARselect(var_ts, lag.max = 10, type = "const")

print_output(var_select$selection, "Optimal Lag Order by Information Criterion")

best_lag <- var_select$selection["SC(n)"]
cat("BIC-selected lag order: p =", best_lag, "\n")

var_best <- vars::VAR(var_ts, p = best_lag, type = "const")

################################################################################
#################### Example 4: Granger Causality Testing ####################
################################################################################

# Granger causality: does knowing past aphid counts help predict ladybird
# counts, BEYOND what past ladybird counts alone provide?
#
# Tested with an F-test comparing the full vs restricted model.
# If the full model is significantly better → aphids Granger-cause ladybirds.

# Test: do aphids Granger-cause ladybirds?
granger_aphid_to_ladybird <- vars::causality(var_best, cause = "aphid_count")
print_output(granger_aphid_to_ladybird$Granger,
             "Granger Causality: Aphid → Ladybird")
# p < 0.05 → aphid counts help predict ladybird counts

# Test: do ladybirds Granger-cause aphids?
granger_ladybird_to_aphid <- vars::causality(var_best, cause = "ladybird_count")
print_output(granger_ladybird_to_aphid$Granger,
             "Granger Causality: Ladybird → Aphid")
# In a true predator-prey system, we expect BIDIRECTIONAL causality.

################################################################################
##################### Example 5: Impulse Response Functions ##################
################################################################################

# IRFs show how a shock to one variable propagates through the system.
# Ecological question: "If aphid numbers suddenly spike, how do ladybird
# numbers respond over the following weeks?"

irf_aphid_to_ladybird <- vars::irf(var_best,
                                    impulse  = "aphid_count",
                                    response = "ladybird_count",
                                    n.ahead  = 20,
                                    boot = TRUE, ci = 0.95)
plot(irf_aphid_to_ladybird,
     main = "IRF: Shock to Aphids → Response of Ladybirds")

irf_ladybird_to_aphid <- vars::irf(var_best,
                                    impulse  = "ladybird_count",
                                    response = "aphid_count",
                                    n.ahead  = 20,
                                    boot = TRUE, ci = 0.95)
plot(irf_ladybird_to_aphid,
     main = "IRF: Shock to Ladybirds → Response of Aphids")
# Expect: aphid shock → ladybird increase (more food)
#         ladybird shock → aphid decrease (more predation)

################################################################################
################ Example 6: Forecast Error Variance Decomposition #############
################################################################################

# FEVD: "How much of the forecast uncertainty in aphid counts is attributable
# to shocks in aphid counts vs shocks in ladybird counts?"

fevd_result <- vars::fevd(var_best, n.ahead = 20)
plot(fevd_result,
     main = "Forecast Error Variance Decomposition")

print_output(fevd_result$aphid_count,
             "FEVD: Proportion of Aphid variance explained by each variable")
print_output(fevd_result$ladybird_count,
             "FEVD: Proportion of Ladybird variance explained by each variable")

################################################################################
##################### Example 7: Multivariate Forecasting ####################
################################################################################

# VAR produces JOINT forecasts for all variables simultaneously.

var_forecast <- predict(var_best, n.ahead = 26, ci = 0.95)
plot(var_forecast,
     main = "VAR Forecasts: Aphid and Ladybird Populations (26 weeks ahead)")

# Extract forecasts as data frames
fc_aphid    <- as.data.frame(var_forecast$fcst$aphid_count)
fc_ladybird <- as.data.frame(var_forecast$fcst$ladybird_count)

cat("\n--- Aphid forecast (first 10 steps) ---\n")
print(head(fc_aphid, 10))

cat("\n--- Ladybird forecast (first 10 steps) ---\n")
print(head(fc_ladybird, 10))

# Publication-quality forecast plot
n_hist <- nrow(ladybird_aphid)
n_fc   <- 26

forecast_plot_data <- bind_rows(
  ladybird_aphid %>%
    tail(100) %>%
    mutate(type = "observed", date = as.Date(date)) %>%
    dplyr::select(date, aphid_count, ladybird_count, type),
  tibble(
    date          = seq(as.Date(ladybird_aphid$date[n_hist]) + 7,
                        by = "week", length.out = n_fc),
    aphid_count    = fc_aphid$fcst,
    ladybird_count = fc_ladybird$fcst,
    type          = "forecast"
  )
)

p_fc <- forecast_plot_data %>%
  pivot_longer(cols = c(aphid_count, ladybird_count),
               names_to = "species", values_to = "count") %>%
  ggplot(aes(x = date, y = count, colour = species, linetype = type)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = pallete[1:2]) +
  scale_linetype_manual(values = c("observed" = "solid", "forecast" = "dashed")) +
  labs(title = "VAR Multivariate Forecast: Predator-Prey System",
       subtitle = "Joint forecasts respect cross-species dependencies",
       x = "Date", y = "Count") +
  theme_new()
print(p_fc)

################################################################################
################### Vector Autoregressive Models Exercises ####################
################################################################################

# Exercise 1: VAR on Simulated Ecological Data
# Simulate a simple two-species interaction (e.g., plant-herbivore):
# a) Generate 200 time steps with plant = 0.6*plant_{t-1} + 0.2*herbivore_{t-1}
#    and herbivore = 0.1*plant_{t-1} + 0.5*herbivore_{t-1} + noise
# b) Convert to ts and use VARselect() to find the optimal lag order
# c) Fit the VAR model with the selected lag
# d) Test Granger causality in both directions
# e) Interpret: does plant biomass Granger-cause herbivore abundance?

# Exercise 2: Impulse Response Interpretation
# Using var_best from the ladybird-aphid data:
# a) Compute all four IRF combinations (aphid→aphid, aphid→ladybird,
#    ladybird→aphid, ladybird→ladybird) with n.ahead = 30
# b) Which IRF shows the strongest response?
# c) After how many weeks does the impulse response die out?
# d) Write an ecological interpretation of the aphid→ladybird IRF

# Exercise 3: Three-Species VAR (Food Chain)
# Simulate a plant-herbivore-predator system:
#   plant:    20 + 0.7*plant_{t-1} - 0.05*herbivore_{t-1} + noise
#   herbivore: 5 + 0.1*plant_{t-1} + 0.6*herbivore_{t-1} - 0.08*predator_{t-1}
#   predator:  2 + 0.05*herbivore_{t-1} + 0.65*predator_{t-1} + noise
# a) Generate 300 observations
# b) Fit a VAR and select optimal lag order with VARselect()
# c) Test all pairwise Granger causality relationships
# d) Do the significant relationships match the food chain structure?

# Exercise 4: FEVD Ecological Interpretation
# Using var_best (ladybird-aphid):
# a) Compute FEVD with n.ahead = 52 (one year)
# b) At what forecast horizon does the ladybird contribution to aphid variance
#    stabilise?
# c) What percentage of aphid forecast variance at 52 weeks is attributable to
#    ladybird shocks?
# d) Discuss: what does this tell us about top-down (predator) control?

# Exercise 5: VAR vs Separate ARIMA
# Using the ladybird-aphid data:
# a) Fit separate ARIMA models to aphid_count and ladybird_count
# b) Fit a VAR model to both series jointly
# c) Generate 26-week forecasts from both approaches
# d) Compare the forecasts visually — does the VAR produce more ecologically
#    coherent joint forecasts?
# e) Discuss when univariate ARIMA is sufficient vs when VAR is needed

################################################################################
############### Vector Autoregressive Models — Answers ########################
################################################################################

# Answer 1: VAR on Simulated Plant-Herbivore Data
set.seed(123)
n_sim    <- 200
plant    <- numeric(n_sim)
herbivore <- numeric(n_sim)
plant[1]     <- 50
herbivore[1] <- 10

for (t in 2:n_sim) {
  plant[t]     <- 0.5 + 0.6 * plant[t-1] + 0.2 * herbivore[t-1] +
                  rnorm(1, 0, 0.5)
  herbivore[t] <- 0.8 + 0.15 * plant[t-1] + 0.5 * herbivore[t-1] +
                  rnorm(1, 0, 0.4)
}

plant_herbivore_ts <- ts(
  data.frame(plant = plant, herbivore = herbivore),
  start = 1, frequency = 1
)

# b) Lag selection
sim_select <- vars::VARselect(plant_herbivore_ts, lag.max = 8, type = "const")
print(sim_select$selection)

# c) Fit VAR
sim_var <- vars::VAR(plant_herbivore_ts,
                      p = sim_select$selection["SC(n)"],
                      type = "const")
summary(sim_var)

# d) Granger causality
vars::causality(sim_var, cause = "herbivore")$Granger
vars::causality(sim_var, cause = "plant")$Granger

# e) If p < 0.05, plant biomass Granger-causes herbivore abundance, reflecting
#    the bottom-up trophic dependency: more food supports more herbivores.

# Answer 2: All Four IRFs
irf_all <- vars::irf(var_best, n.ahead = 30, boot = TRUE, ci = 0.95)
plot(irf_all)

# b) aphid→ladybird is typically strongest (predator responds to prey)
# c) Response usually dies out within 15-20 weeks
# d) An aphid boom (e.g., warm spring) causes increased ladybird populations
#    3-6 weeks later as the food supply grows. The response peaks around
#    week 8-10 then declines as the aphid bloom subsides through density
#    dependence and increased predation pressure.

# Answer 3: Three-Species Food Chain
set.seed(456)
n3    <- 300
sp_a  <- sp_b <- sp_c <- numeric(n3)
sp_a[1] <- 100; sp_b[1] <- 50; sp_c[1] <- 20

for (t in 2:n3) {
  sp_a[t] <- 20 + 0.7 * sp_a[t-1] - 0.05 * sp_b[t-1] + rnorm(1, 0, 3)
  sp_b[t] <- 5  + 0.1 * sp_a[t-1] + 0.6  * sp_b[t-1] - 0.08 * sp_c[t-1] +
             rnorm(1, 0, 2)
  sp_c[t] <- 2  + 0.05 * sp_b[t-1] + 0.65 * sp_c[t-1] + rnorm(1, 0, 1)
}

three_sp_ts <- ts(cbind(plant = sp_a, herbivore = sp_b, predator = sp_c))

three_sp_select <- vars::VARselect(three_sp_ts, lag.max = 6, type = "const")
three_sp_var    <- vars::VAR(three_sp_ts,
                              p = three_sp_select$selection["SC(n)"],
                              type = "const")

vars::causality(three_sp_var, cause = "plant")$Granger
vars::causality(three_sp_var, cause = "herbivore")$Granger
vars::causality(three_sp_var, cause = "predator")$Granger
# Granger causality should align with the food chain: plant→herbivore→predator.

# Answer 4: FEVD at 52-Week Horizon
fevd_52 <- vars::fevd(var_best, n.ahead = 52)
print(fevd_52$aphid_count)
# The ladybird contribution typically stabilises around weeks 15-20.
# If ladybird shocks explain > 15% of aphid forecast variance, this supports
# meaningful top-down predator control, consistent with biological control
# theory in agroecosystems.

# Answer 5: VAR vs Separate ARIMA
aphid_ts_sep <- ts(ladybird_aphid$aphid_count,
                    start = c(2018, 1), frequency = 52)
lady_ts_sep  <- ts(ladybird_aphid$ladybird_count,
                    start = c(2018, 1), frequency = 52)

arima_aphid_sep <- forecast::auto.arima(aphid_ts_sep)
arima_lady_sep  <- forecast::auto.arima(lady_ts_sep)

# 26-week forecasts
fc_arima_aphid <- forecast::forecast(arima_aphid_sep, h = 26)
fc_arima_lady  <- forecast::forecast(arima_lady_sep,  h = 26)
fc_var_sep     <- predict(var_best, n.ahead = 26)

cat("ARIMA aphid forecast (mean, first 5):", head(fc_arima_aphid$mean, 5), "\n")
cat("VAR aphid forecast (first 5):",
    head(fc_var_sep$fcst$aphid_count[,"fcst"], 5), "\n")

# VAR forecasts show correlated oscillations between species.
# ARIMA forecasts are independent, often producing unrealistic patterns
# where both species move in the same direction simultaneously.
# VAR is necessary when cross-species interactions drive dynamics.

################################################################################
############### Vector Autoregressive Models — Advanced Examples ##############
################################################################################

# Advanced Example 1: Comparison of VAR(1) through VAR(5) using AICc
lapply(1:5, function(p) {
  fit <- vars::VAR(var_ts, p = p, type = "const")
  data.frame(p = p, AIC = AIC(fit), BIC = BIC(fit))
}) %>%
  bind_rows() %>%
  print_output("VAR Lag Selection: AIC and BIC by Order")

# Advanced Example 2: Structural VAR (SVAR) Interpretation
# The reduced-form VAR coefficients can be interpreted, but SVAR identifies
# structural shocks. This is a brief demonstration using vars::SVAR().

svar_amat <- matrix(c(NA, 0, NA, NA), nrow = 2)  # lower-triangular A matrix
svar_fit  <- tryCatch(
  vars::SVAR(var_best, estmethod = "direct", Amat = svar_amat),
  error = function(e) cat("SVAR failed:", conditionMessage(e), "\n")
)

# Advanced Example 3: Stability Check of the VAR Model
# A VAR model is stationary (stable) if all eigenvalues of the companion
# matrix lie inside the unit circle.
stability_check <- vars::stability(var_best, type = "OLS-CUSUM")
plot(stability_check)
cat("VAR model is stable if all eigenvalues are inside the unit circle.\n")

# Advanced Example 4: Forecasting with Confidence Intervals Plotted Manually
fc_26 <- predict(var_best, n.ahead = 26, ci = 0.95)

aphid_fc_df <- as.data.frame(fc_26$fcst$aphid_count) %>%
  mutate(step = row_number()) %>%
  rename(forecast = fcst, lower = lower, upper = upper)

ggplot(aphid_fc_df, aes(x = step)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = pallete[1], alpha = 0.2) +
  geom_line(aes(y = forecast), colour = pallete[1], linewidth = 1) +
  labs(title = "VAR 26-Week Aphid Forecast with 95% CI",
       x = "Forecast step (weeks)", y = "Aphid count") +
  theme_new()

# Advanced Example 5: Multivariate Residual Diagnostics
# Check for serial correlation in VAR residuals (Portmanteau test)
serial_test <- vars::serial.test(var_best, lags.pt = 16, type = "PT.asymptotic")
print_output(serial_test, "Portmanteau Test for Residual Serial Correlation")
# p > 0.05 → no significant serial correlation; model is adequately specified.

################################################################################
############ Vector Autoregressive Models — Advanced Exercises ################
################################################################################

# Advanced Exercise 1: Climate-Driven Herbivore VAR
# Using aphid_climate_brazil.csv, create a bivariate VAR with
# aphid_abundance and temp_mean:
# a) Make both series stationary with make_stationary()
# b) Fit VAR(p) using BIC-optimal lag order
# c) Test Granger causality: does temperature Granger-cause aphid abundance?
# d) Compute IRFs: how does a temperature shock affect aphid counts over 20 weeks?
# e) Is the lagged response consistent with known aphid biology?

# Advanced Exercise 2: Seasonal VAR
# Predator-prey systems often have seasonal forcing (more prey in spring/summer).
# a) Filter ladybird_aphid data to spring-summer only (weeks 12-40 per year)
# b) Fit VAR on this seasonal subset
# c) Compare IRF strength to the full-year VAR — is the predator response faster
#    during the active growing season?

# Advanced Exercise 3: VAR Order Uncertainty
# When BIC and AIC disagree on the optimal lag order:
# a) Fit VAR(1), VAR(2), and VAR(3) to the ladybird-aphid data
# b) For each, test Granger causality (aphid → ladybird)
# c) Does the conclusion change with different lag orders?
# d) Which criterion should an ecologist prefer, and why?

# Advanced Exercise 4: FEVD with Ecological Interpretation
# Using the three-species plant-herbivore-predator VAR:
# a) Compute FEVD for all three species at horizons 5, 10, 20, and 52 weeks
# b) Plot the FEVD for the herbivore: what proportion is explained by each species?
# c) Does top-down control (predator → herbivore) or bottom-up (plant → herbivore)
#    dominate? At what forecast horizon does the balance shift?

# Advanced Exercise 5: Rolling VAR Forecasts
# a) Set up a rolling-window forecast for the ladybird-aphid system
#    (window = 100 weeks, rolling forward 1 step at a time)
# b) Collect 1-step-ahead forecasts from VAR and from separate ARIMA models
# c) Compute RMSE over all rolling windows for both methods
# d) Does the VAR consistently outperform the separate ARIMA?
# e) When does the VAR give the worst forecasts? Examine those time points.
