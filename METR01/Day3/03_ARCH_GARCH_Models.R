####################################################################################################
###
### File:    03_ARCH_GARCH_Models.R
### Purpose: Examples and exercises for ARCH and GARCH Models
### Authors: Gabriel Rodrigues Palma
### Date:    17/06/25
###
####################################################################################################
# load packages -----
source('00_source.r')

################################################################################
###################### ARCH and GARCH Models ###################################
################################################################################

# Most time series models (ARIMA, ETS, VAR) predict the MEAN of the series.
# But some ecological time series don't just change in level — their
# VARIABILITY changes too.
#
# Population crashes can be followed by wild fluctuations as the ecosystem
# recovers. Calm periods alternate with volatile ones. This is called
# VOLATILITY CLUSTERING.
#
# ARCH and GARCH models capture this by modelling the CONDITIONAL VARIANCE
# — how the spread of the series changes over time.
#
# ARCH(q) — Autoregressive Conditional Heteroskedasticity:
#   sigma_t^2 = alpha_0 + alpha_1*epsilon_{t-1}^2 + ... + alpha_q*epsilon_{t-q}^2
#   Variance depends on past squared residuals ("surprises")
#
# GARCH(1,1) — Generalised ARCH:
#   sigma_t^2 = alpha_0 + alpha_1*epsilon_{t-1}^2 + beta_1*sigma_{t-1}^2
#   Adds lagged variance — more parsimonious than high-order ARCH
#   (analogous to ARMA being more parsimonious than high-order AR)
#
# Extensions:
#   eGARCH: log(sigma_t^2) = omega + alpha*|z_{t-1}| + gamma*z_{t-1}
#             + beta*log(sigma_{t-1}^2)
#   Allows ASYMMETRIC effects — positive and negative shocks differ

# Constants for GARCH diagnostics ---------------------------
GARCH_LB_LAG <- 10           # Ljung-Box lag for ARCH effects
GARCH_FORECAST_HORIZON <- 20 # Default volatility forecast steps

################################################################################
########### Example 1: Volatility Clustering — DAX Returns #####################
################################################################################

# EuStockMarkets (base R): German DAX index, 1991-1998, daily.
# Log returns are the standard transformation for financial data.

data("EuStockMarkets")
dax_prices  <- EuStockMarkets[, "DAX"]
dax_returns <- diff(log(dax_prices))

# Plot returns: notice periods of high and low variability
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
plot(dax_prices,
     main = "DAX Prices (1991-1998)",
     ylab = "Price", col = pallete[1])
plot(dax_returns,
     main = "DAX Log Returns",
     ylab = "Log return", col = pallete[2])
par(mfrow = c(1, 1))
# The returns look stationary around zero, but the SPREAD changes!
# Periods of tight clustering alternate with wide swings.

################################################################################
########### Example 2: Detecting Heteroskedasticity — ACF of Squared Returns ##
################################################################################

# If variance were constant, squared returns would show no autocorrelation.
# Significant ACF in squared returns → ARCH effects present.

par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
acf(as.numeric(dax_returns),
    main = "ACF of DAX Returns",
    lag.max = 30, col = pallete[1])
acf(as.numeric(dax_returns^2),
    main = "ACF of Squared DAX Returns",
    lag.max = 30, col = pallete[2])
par(mfrow = c(1, 1))
# Returns: minimal autocorrelation (mean is ~unpredictable)
# Squared returns: STRONG autocorrelation (variance IS predictable)

# Formal Ljung-Box test on squared returns
lb_test <- Box.test(as.numeric(dax_returns^2),
                     lag = GARCH_LB_LAG, type = "Ljung-Box")
print_output(lb_test, "Ljung-Box Test on Squared DAX Returns")
# p < 0.05 confirms significant ARCH effects

################################################################################
########### Example 3: Fitting GARCH(1,1) with tseries::garch() ###############
################################################################################

# tseries::garch() provides the simplest GARCH implementation.
# garch(order = c(q, p)) fits a GARCH(p,q) model.
# Note: tseries uses the order (ARCH, GARCH), not (GARCH, ARCH).

garch_simple <- tseries::garch(as.numeric(dax_returns),
                                order = c(1, 1),
                                trace = FALSE)
summary(garch_simple)

# Extract conditional standard deviations (volatility estimates)
garch_vol <- garch_simple$fitted.values[, 1]

# Plot returns with estimated volatility bands
par(mfrow = c(1, 1))
plot(as.numeric(dax_returns), type = "l", col = "grey60",
     main = "DAX Returns with GARCH(1,1) Volatility Bands",
     ylab = "Log return", xlab = "Time")
lines(garch_vol,  col = pallete[1], lwd = 2)
lines(-garch_vol, col = pallete[1], lwd = 2)
legend("topright",
       legend = c("Returns", "+/- Cond. Std Dev"),
       col = c("grey60", pallete[1]), lwd = c(1, 2))

################################################################################
########### Example 4: Full GARCH Workflow with rugarch #######################
################################################################################

# rugarch workflow: ugarchspec() → ugarchfit() → ugarchforecast()
#
# Step 1: Specify the model
# Step 2: Fit the model
# Step 3: Diagnose and forecast

# Specify GARCH(1,1) with constant mean and Student-t innovations
spec_garch11 <- rugarch::ugarchspec(
  variance.model   = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model       = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "std"
)

# Fit the model
fit_garch11 <- rugarch::ugarchfit(
  spec = spec_garch11,
  data = as.numeric(dax_returns)
)

show(fit_garch11)

cat("\n--- GARCH(1,1) Coefficients ---\n")
print(coef(fit_garch11))

# alpha1 + beta1 close to 1 → high persistence of volatility
compute_garch_persistence(fit_garch11)

################################################################################
########### Example 5: Standardised Residual Diagnostics #####################
################################################################################

# Good GARCH fit → standardised residuals (epsilon_t / sigma_t) should be
# independent and identically distributed random variables.

std_resid <- residuals(fit_garch11, standardize = TRUE)

par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

plot(std_resid, type = "l",
     main = "Standardised Residuals",
     ylab = "z_t", col = pallete[1])

acf(as.numeric(std_resid),
    main = "ACF of Std Residuals",
    lag.max = 20, col = pallete[2])

acf(as.numeric(std_resid^2),
    main = "ACF of Squared Std Residuals",
    lag.max = 20, col = pallete[3])

qqnorm(as.numeric(std_resid),
       main = "QQ Plot of Std Residuals",
       col = pallete[4])
qqline(as.numeric(std_resid))

par(mfrow = c(1, 1))

# Formal test: no ARCH effects should remain in standardised residuals
lb_std <- Box.test(as.numeric(std_resid^2),
                    lag = GARCH_LB_LAG, type = "Ljung-Box")
print_output(lb_std, "Ljung-Box on Squared Standardised Residuals")
# p > 0.05 → GARCH has captured the volatility structure

################################################################################
########### Example 6: GARCH Variant Comparison — sGARCH vs eGARCH ###########
################################################################################

# eGARCH allows asymmetric effects (leverage): negative shocks can increase
# volatility more than positive shocks of the same magnitude.
# gjrGARCH uses a threshold to capture this asymmetry differently.

spec_egarch <- rugarch::ugarchspec(
  variance.model   = list(model = "eGARCH", garchOrder = c(1, 1)),
  mean.model       = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "std"
)
fit_egarch <- rugarch::ugarchfit(spec = spec_egarch,
                                  data = as.numeric(dax_returns))

spec_gjr <- rugarch::ugarchspec(
  variance.model   = list(model = "gjrGARCH", garchOrder = c(1, 1)),
  mean.model       = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "std"
)
fit_gjr <- rugarch::ugarchfit(spec = spec_gjr,
                               data = as.numeric(dax_returns))

model_comparison <- data.frame(
  model   = c("sGARCH(1,1)", "eGARCH(1,1)", "gjrGARCH(1,1)"),
  aic     = c(rugarch::infocriteria(fit_garch11)[1],
              rugarch::infocriteria(fit_egarch)[1],
              rugarch::infocriteria(fit_gjr)[1]),
  bic     = c(rugarch::infocriteria(fit_garch11)[2],
              rugarch::infocriteria(fit_egarch)[2],
              rugarch::infocriteria(fit_gjr)[2]),
  loglik  = c(rugarch::likelihood(fit_garch11),
              rugarch::likelihood(fit_egarch),
              rugarch::likelihood(fit_gjr))
)

print_output(model_comparison, "GARCH Variant Comparison (DAX Returns)")
# Lower AIC/BIC = better model; eGARCH often wins for financial data.

################################################################################
########### Example 7: Volatility Forecasting ################################
################################################################################

# GARCH's primary use: forecasting future VOLATILITY.
# Crucial for risk assessment — in finance AND ecology.

fc_garch <- rugarch::ugarchforecast(fit_garch11, n.ahead = GARCH_FORECAST_HORIZON)

fc_sigma <- as.numeric(rugarch::sigma(fc_garch))
fc_mean  <- as.numeric(fitted(fc_garch))

cat("\n--- Volatility Forecast (next", GARCH_FORECAST_HORIZON, "periods) ---\n")
print(data.frame(
  step     = 1:GARCH_FORECAST_HORIZON,
  mean_fc  = round(fc_mean, 6),
  sigma_fc = round(fc_sigma, 6)
))

# Historical volatility + forecast plot
hist_sigma <- as.numeric(rugarch::sigma(fit_garch11))

volatility_plot_data <- data.frame(
  time  = c(1:length(hist_sigma),
             (length(hist_sigma)+1):(length(hist_sigma) + GARCH_FORECAST_HORIZON)),
  sigma = c(hist_sigma, fc_sigma),
  type  = c(rep("Historical", length(hist_sigma)),
             rep("Forecast", GARCH_FORECAST_HORIZON))
)

p_vol <- volatility_plot_data %>%
  tail(200) %>%
  ggplot(aes(x = time, y = sigma, colour = type)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(values = pallete[c(1, 2)]) +
  labs(title = "GARCH(1,1) Conditional Volatility: Historical + Forecast",
       subtitle = "Volatility forecasts converge toward the long-run average",
       x = "Time", y = "Conditional Std Dev", colour = "") +
  theme_new()
print(p_vol)

################################################################################
########### Example 8: Ecological Application — Population Volatility #########
################################################################################

# Ecological analogue: insect populations showing variance clustering.
# After disturbance events (drought, habitat loss), population variability
# increases — periods of instability followed by gradual recovery.

pop_vol <- read.csv("input_data/population_volatility.csv")

pop_ts      <- ts(pop_vol$population_index,
                   start = c(2005, 1), frequency = 12)
pop_returns <- diff(log(pop_ts))

# Plot with disturbance event markers
par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
plot(pop_ts, main = "Monthly Insect Population Index",
     ylab = "Population index", col = pallete[1])
abline(v = c(2009.5, 2015.5, 2020.5), lty = 2, col = "red")
text(c(2009.5, 2015.5, 2020.5), par("usr")[4] * 0.95,
     labels = c("Drought", "Habitat loss", "Extreme weather"),
     cex = 0.7, col = "red")

plot(pop_returns, main = "Population Growth Rate (Log Returns)",
     ylab = "Growth rate", col = pallete[2])
par(mfrow = c(1, 1))

# Check for ARCH effects in population returns
acf(as.numeric(pop_returns^2),
    main = "ACF of Squared Population Growth Rates",
    lag.max = 24)

# Fit GARCH(1,1) to population growth rates
spec_eco <- rugarch::ugarchspec(
  variance.model   = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model       = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "std"
)

fit_eco    <- rugarch::ugarchfit(spec = spec_eco,
                                  data = as.numeric(pop_returns))
eco_sigma  <- as.numeric(rugarch::sigma(fit_eco))

par(mfrow = c(2, 1), mar = c(4, 4, 3, 1))
plot(as.numeric(pop_returns), type = "l", col = "grey60",
     main = "Population Growth Rate with GARCH Volatility",
     ylab = "Growth rate")
lines(eco_sigma,  col = pallete[1], lwd = 2)
lines(-eco_sigma, col = pallete[1], lwd = 2)

plot(eco_sigma, type = "l", col = pallete[2], lwd = 2,
     main = "Conditional Volatility: Periods of Population Instability",
     ylab = "Cond. Std Dev", xlab = "Time (months)")
par(mfrow = c(1, 1))

################################################################################
######################## ARCH/GARCH Exercises ##################################
################################################################################

# Exercise 1: ARCH Effect Detection on FTSE Returns
# Using EuStockMarkets FTSE (UK index):
# a) Compute log returns
# b) Plot ACF of returns and squared returns side-by-side
# c) Perform Ljung-Box test on squared returns (lag = 10)
# d) Are significant ARCH effects present?

# Exercise 2: GARCH(1,1) vs ARCH(3) Comparison
# Using dax_returns:
# a) Fit GARCH(1,1) using tseries::garch(order = c(1, 1))
# b) Fit ARCH(3) using tseries::garch(order = c(3, 0))
# c) Compare the fitted values and residual diagnostics
# d) Why is GARCH(1,1) preferred over ARCH(3)?

# Exercise 3: Distribution Choice for rugarch
# Using dax_returns:
# a) Fit sGARCH(1,1) with normal distribution (distribution.model = "norm")
# b) Fit sGARCH(1,1) with Student-t distribution (already done as fit_garch11)
# c) Fit sGARCH(1,1) with skewed Student-t (distribution.model = "sstd")
# d) Compare all three using infocriteria()
# e) Why does the heavy-tailed distribution typically fit better?

# Exercise 4: eGARCH Leverage Interpretation
# Using fit_egarch from Example 6:
# a) Extract coefficients using coef()
# b) The "gamma1" captures asymmetry — is it significant? What sign?
# c) In ecological terms: could population DECLINES create more variability
#    than equivalent increases? When might this happen?

# Exercise 5: Ecological Volatility Forecasting
# Using population_volatility.csv:
# a) Fit a GARCH(1,1) to the population growth rates
# b) Generate a 24-month volatility forecast
# c) Plot the forecast alongside historical conditional volatility
# d) In conservation terms, what does a high volatility forecast mean?
# e) Compare GARCH sigma to a rolling 12-month standard deviation

################################################################################
######################## ARCH/GARCH — Answers #################################
################################################################################

# Answer 1: ARCH Effects in FTSE Returns
ftse_returns <- diff(log(EuStockMarkets[, "FTSE"]))

# b) ACF comparison
par(mfrow = c(1, 2))
acf(as.numeric(ftse_returns),   main = "ACF: FTSE Returns",         lag.max = 20)
acf(as.numeric(ftse_returns^2), main = "ACF: Squared FTSE Returns", lag.max = 20)
par(mfrow = c(1, 1))

# c) Ljung-Box test
Box.test(as.numeric(ftse_returns^2), lag = 10, type = "Ljung-Box")

# d) Squared returns show significant spikes; Ljung-Box p < 0.05 confirms ARCH.

# Answer 2: GARCH(1,1) vs ARCH(3)
garch11_tseries <- tseries::garch(as.numeric(dax_returns),
                                   order = c(1, 1), trace = FALSE)
arch3_tseries   <- tseries::garch(as.numeric(dax_returns),
                                   order = c(3, 0), trace = FALSE)

summary(garch11_tseries)
summary(arch3_tseries)

# d) GARCH(1,1) uses 3 parameters to achieve what ARCH(3) does with 4.
#    The beta_1 term captures volatility persistence parsimoniously, just as
#    ARMA(1,1) is more efficient than AR(10) for modelling the mean.

# Answer 3: Distribution Comparison
spec_norm <- rugarch::ugarchspec(
  variance.model   = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model       = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "norm"
)
fit_norm <- rugarch::ugarchfit(spec = spec_norm,
                                data = as.numeric(dax_returns))

spec_sstd <- rugarch::ugarchspec(
  variance.model   = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model       = list(armaOrder = c(0, 0), include.mean = TRUE),
  distribution.model = "sstd"
)
fit_sstd <- rugarch::ugarchfit(spec = spec_sstd,
                                data = as.numeric(dax_returns))

dist_comparison <- data.frame(
  distribution = c("Normal", "Student-t", "Skewed Student-t"),
  aic = c(rugarch::infocriteria(fit_norm)[1],
          rugarch::infocriteria(fit_garch11)[1],
          rugarch::infocriteria(fit_sstd)[1]),
  bic = c(rugarch::infocriteria(fit_norm)[2],
          rugarch::infocriteria(fit_garch11)[2],
          rugarch::infocriteria(fit_sstd)[2])
)
print(dist_comparison)
# Ecological and financial data have heavier tails than Gaussian — extreme events
# (population crashes, market crashes) are more frequent than predicted by a
# normal model. Student-t and skewed-t distributions capture this.

# Answer 4: eGARCH Leverage
print(coef(fit_egarch))
# gamma1 captures the leverage effect.
# Negative gamma1: negative shocks increase volatility MORE than positive shocks.
# In ecology: population DECLINES often destabilise ecosystems more than
# equivalent increases. A population crash can trigger cascading effects (loss
# of pollinators, prey scarcity for predators) that sustain high variability,
# whereas a population boom is often self-correcting through density dependence.

# Answer 5: Ecological Volatility Forecast
pop_ret <- diff(log(ts(pop_vol$population_index, frequency = 12)))

spec_eco5 <- rugarch::ugarchspec(
  variance.model   = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model       = list(armaOrder = c(1, 0), include.mean = TRUE),
  distribution.model = "std"
)
fit_eco5 <- rugarch::ugarchfit(spec = spec_eco5,
                                data = as.numeric(pop_ret))

# b) 24-month forecast
fc_eco5   <- rugarch::ugarchforecast(fit_eco5, n.ahead = 24)
hist_vol5 <- as.numeric(rugarch::sigma(fit_eco5))
fc_vol5   <- as.numeric(rugarch::sigma(fc_eco5))

# c) Plot
plot(c(hist_vol5, fc_vol5), type = "l", col = pallete[1],
     main = "Ecological Volatility: Historical + 24-month Forecast",
     ylab = "Conditional Std Dev", xlab = "Month")
abline(v = length(hist_vol5), lty = 2)

# d) High volatility forecast signals ecological instability.
#    Managers should: increase monitoring frequency, prepare intervention
#    resources, and avoid introducing additional stressors during this period.

# e) Rolling std dev comparison
roll_sd <- slider::slide_dbl(as.numeric(pop_ret), sd, .before = 11, .complete = TRUE)
plot(roll_sd, type = "l", col = pallete[2],
     main = "Rolling 12-month Std Dev vs GARCH Conditional Sigma",
     ylab = "Volatility estimate", xlab = "Month")
lines(hist_vol5, col = pallete[1])
legend("topright",
       legend = c("Rolling SD", "GARCH sigma"),
       col = pallete[c(2, 1)], lwd = 1)
# GARCH responds faster to volatility changes (parametric model structure),
# whereas rolling SD is a lagging indicator.

################################################################################
###################### ARCH/GARCH — Advanced Examples ########################
################################################################################

# Advanced Example 1: GARCH-in-Mean — Volatility as a Predictor
# Hypothesis: higher ecological volatility may predict lower mean abundance
# (risk-return trade-off in ecology: instability reduces average population).

spec_gim <- rugarch::ugarchspec(
  variance.model   = list(model = "sGARCH", garchOrder = c(1, 1)),
  mean.model       = list(armaOrder = c(1, 0), include.mean = TRUE,
                           archm = TRUE, archpow = 2),
  distribution.model = "std"
)

fit_gim <- tryCatch(
  rugarch::ugarchfit(spec = spec_gim, data = as.numeric(pop_ret)),
  error = function(e) {
    cat("GARCH-in-Mean failed:", conditionMessage(e), "\n")
    NULL
  }
)

if (!is.null(fit_gim)) {
  cat("GARCH-in-Mean archm coefficient:",
      round(coef(fit_gim)["archm"], 4), "\n")
}

# Advanced Example 2: Rolling Volatility Estimation
# Estimate conditional volatility over a rolling 52-week window.
window_size <- 52
n_total     <- length(dax_returns)
roll_garch_sigma <- rep(NA, n_total)

for (end_idx in (window_size + 1):n_total) {
  window_data <- as.numeric(dax_returns)[(end_idx - window_size):end_idx]
  fit_win <- tryCatch(
    rugarch::ugarchfit(spec = spec_garch11, data = window_data,
                        solver = "hybrid"),
    error = function(e) NULL
  )
  if (!is.null(fit_win)) {
    last_sigma <- tail(as.numeric(rugarch::sigma(fit_win)), 1)
    roll_garch_sigma[end_idx] <- last_sigma
  }
}

plot(roll_garch_sigma, type = "l", col = pallete[1],
     main = "Rolling GARCH Conditional Sigma (52-week windows)",
     ylab = "Sigma", xlab = "Day")

# Advanced Example 3: Comparing Normal vs Heavy-Tailed Distribution Impact
# Plot the density of standardised residuals from fit_norm and fit_garch11
std_norm <- as.numeric(residuals(fit_norm, standardize = TRUE))
std_t    <- as.numeric(residuals(fit_garch11, standardize = TRUE))

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))
hist(std_norm, breaks = 40, freq = FALSE,
     main = "Normal GARCH Std Residuals",
     xlab = "Standardised residual", col = pallete[1])
curve(dnorm(x), add = TRUE, col = "black", lwd = 2)

hist(std_t, breaks = 40, freq = FALSE,
     main = "Student-t GARCH Std Residuals",
     xlab = "Standardised residual", col = pallete[2])
curve(dnorm(x), add = TRUE, col = "black", lwd = 2)
par(mfrow = c(1, 1))

# Advanced Example 4: Stationarity Condition for GARCH
# GARCH(1,1) is covariance-stationary if alpha1 + beta1 < 1.
# Print the persistence for all fitted models.
persistence_tbl <- data.frame(
  model = c("sGARCH(1,1)", "eGARCH(1,1)", "gjrGARCH(1,1)"),
  persistence = c(
    coef(fit_garch11)["alpha1"] + coef(fit_garch11)["beta1"],
    exp(coef(fit_egarch)["beta1"]),          # eGARCH persistence
    coef(fit_gjr)["alpha1"] + coef(fit_gjr)["beta1"] +
      0.5 * coef(fit_gjr)["gamma1"]          # GJR persistence
  )
)
print_output(persistence_tbl, "GARCH Volatility Persistence by Model")

# Advanced Example 5: Ecological Disturbance Detection via Conditional Variance
# Mark times when conditional variance exceeds the 95th percentile as
# "high-volatility periods" — potential ecological disturbance events.

VOLATILITY_THRESHOLD <- quantile(eco_sigma, 0.95)
disturbance_periods  <- which(eco_sigma > VOLATILITY_THRESHOLD)

cat("High-volatility months (potential disturbances):\n")
print(disturbance_periods)
cat("These correspond to", length(disturbance_periods),
    "months exceeding the 95th percentile of conditional sigma.\n")

################################################################################
###################### ARCH/GARCH — Advanced Exercises #######################
################################################################################

# Advanced Exercise 1: EGARCH vs GJR on Ecological Data
# Using pop_ret (population growth rates):
# a) Fit sGARCH(1,1), eGARCH(1,1), and gjrGARCH(1,1)
# b) Compare the three models using AIC and BIC
# c) Plot the conditional volatility from each model side-by-side
# d) Which model identifies disturbance events most clearly?
# e) Does the eGARCH gamma1 show the leverage effect in the ecological data?

# Advanced Exercise 2: GARCH with AR Mean Equation
# Some ecological series have autocorrelated means.
# a) Fit ARMA(1,1)-GARCH(1,1) to population growth rates
#    (armaOrder = c(1, 1), garchOrder = c(1, 1))
# b) Compare to ARMA(0,0)-GARCH(1,1) using AICc
# c) Does including an AR term improve the fit?
# d) Interpret: what does AR persistence in the mean equation mean ecologically?

# Advanced Exercise 3: Simulation Study for GARCH
# a) Simulate 500 observations from a known GARCH(1,1) process with:
#    alpha0 = 0.01, alpha1 = 0.1, beta1 = 0.85, Student-t(df=6) innovations
# b) Fit GARCH(1,1) to the simulated data
# c) Compare the estimated parameters to the true parameters
# d) How large a sample is needed to recover the true parameters?

# Advanced Exercise 4: Multivariate GARCH (DCC) Concept
# The DCC (Dynamic Conditional Correlation) model extends GARCH to multiple
# series, modelling time-varying correlations between assets or species.
# a) Compute the rolling 52-week correlation between aphid and ladybird returns
# b) Plot the rolling correlation over time
# c) Does the correlation change during peak ecological activity months?
# d) Discuss: what would a DCC model add over computing rolling correlations?

# Advanced Exercise 5: Out-of-Sample Volatility Forecasting Evaluation
# Using dax_returns:
# a) Split into training (first 80%) and test (last 20%) sets
# b) Fit GARCH(1,1) and eGARCH(1,1) on the training set
# c) Generate volatility forecasts for the test period
# d) Use squared returns as a proxy for realised variance
# e) Compute RMSE between forecasted sigma^2 and squared returns
# f) Which GARCH variant has lower RMSE on the test set?
