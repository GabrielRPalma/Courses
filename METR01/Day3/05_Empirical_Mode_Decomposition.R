####################################################################################################
###
### File:    05_Empirical_Mode_Decomposition.R
### Purpose: Examples and exercises for Empirical Mode Decomposition (EMD)
### Authors: Gabriel Rodrigues Palma
### Date:    17/06/25
###
####################################################################################################
# load packages -----
source('00_source.r')

################################################################################
################## Empirical Mode Decomposition (EMD) ##########################
################################################################################

# Classical decomposition (STL, Fourier) ASSUMES fixed seasonal shapes.
# EMD is DATA-DRIVEN — it makes NO assumptions about the shape of the signal.
#
# EMD breaks a time series into Intrinsic Mode Functions (IMFs), ordered
# from the MOST oscillatory (highest frequency, stochastic) to the MOST
# smooth (lowest frequency, deterministic).
#
# The EMD equation (Huang et al., 1998):
#   x(t) = sum_{i=1}^{n} c_i(t) + r_n(t)
#   where c_i(t) are the IMFs and r_n(t) is the residual trend.
#
# An IMF must satisfy two conditions:
#   1. The number of extrema and zero-crossings differs by at most one
#   2. The mean of the upper and lower envelopes is zero at every point
#
# The SIFTING PROCESS:
#   Step 1: Identify local maxima and minima of the signal
#   Step 2: Construct upper (eup) and lower (elow) envelopes via cubic spline
#   Step 3: Compute mean envelope: m(t) = 0.5 * (eup + elow)
#   Step 4: Subtract: h(t) = x(t) - m(t)
#   Step 5: Repeat steps 1-4 on h(t) until it satisfies IMF conditions
#   Step 6: Store as IMF c_i; residual: r(t) = x(t) - c_i(t)
#   Step 7: Repeat on the residual until monotone or < 2 extrema
#
# REFERENCE: Palma, G.R., Skoczen, M., & Maguire, P. (2025).
# "Asset price movement prediction using empirical mode decomposition
# and Gaussian mixture models." arXiv:2503.20678.

# Constants for EMD frequency-band reconstruction ---------------------------
# Following Palma et al. (2025): adaptive cutoffs based on total IMF count J
# r1 = max(1, J-6): high-stochasticity cutoff
# r2 = max(1, J-4): medium-stochasticity cutoff
# r3 = max(1, J-2): low-stochasticity cutoff

compute_emd_cutoffs <- function(n_imfs) {
  # Compute adaptive frequency-band cutoff indices.
  #
  # Args:
  #   n_imfs: integer, total number of IMFs from EMD
  #
  # Returns:
  #   named integer vector with r1, r2, r3
  r1 <- max(1L, n_imfs - 6L)
  r2 <- max(1L, n_imfs - 4L)
  r3 <- max(1L, n_imfs - 2L)
  cat("Total IMFs (J):", n_imfs, "\n")
  cat("Cutoff indices — r1:", r1, " r2:", r2, " r3:", r3, "\n")
  return(c(r1 = r1, r2 = r2, r3 = r3))
}

reconstruct_emd_bands <- function(emd_result) {
  # Reconstruct four frequency bands from an EMD result object.
  # Following the Palma et al. (2025) adaptive scheme.
  #
  # Args:
  #   emd_result: object from EMD::emd()
  #
  # Returns:
  #   list with high_stoch, medium_stoch, low_stoch, trend_comp
  if (is.null(emd_result$imf)) stop("emd_result must contain $imf matrix")
  j_total  <- emd_result$nimf
  cutoffs  <- compute_emd_cutoffs(j_total)
  r1 <- cutoffs["r1"]; r2 <- cutoffs["r2"]; r3 <- cutoffs["r3"]

  high_stoch   <- rowSums(emd_result$imf[, 1:r1,           drop = FALSE])
  medium_stoch <- rowSums(emd_result$imf[, 1:r2,           drop = FALSE])
  low_stoch    <- rowSums(emd_result$imf[, 1:r3,           drop = FALSE])
  trend_comp   <- rowSums(emd_result$imf[, (r3+1):j_total, drop = FALSE]) +
                  emd_result$residue

  return(list(
    high_stoch   = high_stoch,
    medium_stoch = medium_stoch,
    low_stoch    = low_stoch,
    trend_comp   = trend_comp
  ))
}

################################################################################
################# Example 1: EMD on a Synthetic Composite Signal ###############
################################################################################

# We construct a signal with KNOWN components to verify EMD's ability to
# separate frequencies without any prior specification.

set.seed(42)
t_idx      <- 1:500
high_freq  <- 2 * sin(2 * pi * t_idx / 15)   # period ~15
medium_freq <- 5 * sin(2 * pi * t_idx / 80)  # period ~80
linear_trend <- 0.02 * t_idx                  # linear trend
noise        <- rnorm(500, 0, 0.5)

synthetic_signal <- high_freq + medium_freq + linear_trend + noise

plot(t_idx, synthetic_signal, type = "l", col = pallete[1],
     main = "Composite Signal: High-freq + Medium-freq + Trend + Noise",
     xlab = "Time", ylab = "Value")

# Apply EMD
emd_synthetic <- EMD::emd(synthetic_signal, boundary = "wave")

n_imfs_syn <- emd_synthetic$nimf
cat("Synthetic signal — IMFs extracted:", n_imfs_syn, "\n")

# Plot all IMFs
par(mfrow = c(n_imfs_syn + 2, 1), mar = c(2, 4, 2, 1))
plot(t_idx, synthetic_signal, type = "l",
     main = "Original Signal", ylab = "Signal")

for (i in 1:n_imfs_syn) {
  plot(t_idx, emd_synthetic$imf[, i], type = "l",
       col = pallete[(i %% 7) + 1],
       main = paste0("IMF ", i, " (Frequency: high → low)"),
       ylab = paste0("IMF ", i))
}

plot(t_idx, emd_synthetic$residue, type = "l", col = pallete[3],
     main = "Residue (Trend)", ylab = "Residue")
par(mfrow = c(1, 1))
# IMF 1 captures the noise + high-frequency oscillation
# IMF 2 captures the medium-frequency component
# Residue captures the trend — no frequency specification needed!

################################################################################
################# Example 2: EMD on DAX Financial Returns #####################
################################################################################

data("EuStockMarkets")
dax_prices <- as.numeric(EuStockMarkets[, "DAX"])
t_dax      <- seq_along(dax_prices)

emd_dax <- EMD::emd(dax_prices, boundary = "wave")

cat("DAX decomposition:", emd_dax$nimf, "IMFs extracted\n")

n_dax <- emd_dax$nimf
par(mfrow = c(min(n_dax + 2, 10), 1), mar = c(2, 4, 1.5, 1))

plot(t_dax, dax_prices, type = "l", main = "DAX Prices", ylab = "Price",
     col = pallete[1])

for (i in 1:min(n_dax, 8)) {
  plot(t_dax, emd_dax$imf[, i], type = "l",
       col = pallete[(i %% 7) + 1],
       main = paste0("IMF ", i),
       ylab = paste0("C_", i))
}

plot(t_dax, emd_dax$residue, type = "l", col = pallete[3],
     main = "Residue (Long-term trend)", ylab = "Trend")
par(mfrow = c(1, 1))

# From stochastic to deterministic:
# IMF 1-2: High-frequency noise (day-to-day randomness)
# IMF 3-5: Medium-frequency cycles (market corrections, seasonal effects)
# IMF 6+:  Low-frequency dynamics (macroeconomic trends)
# Residue: Overall long-term growth trend

################################################################################
################# Example 3: Reconstructing Frequency Bands (Palma 2025) ######
################################################################################

# Following Palma et al. (2025), we define four frequency bands using
# adaptive cutoff indices based on total IMF count J.

cutoffs_dax  <- compute_emd_cutoffs(emd_dax$nimf)
bands_dax    <- reconstruct_emd_bands(emd_dax)

# Plot the four components
bands_df <- data.frame(
  time      = rep(t_dax, 4),
  value     = c(bands_dax$high_stoch,
                bands_dax$medium_stoch,
                bands_dax$low_stoch,
                bands_dax$trend_comp),
  component = rep(c("High Stochasticity",
                     "Medium Stochasticity",
                     "Low Stochasticity",
                     "Trend (Deterministic)"),
                   each = length(t_dax))
)

bands_df$component <- factor(bands_df$component,
                               levels = c("High Stochasticity",
                                          "Medium Stochasticity",
                                          "Low Stochasticity",
                                          "Trend (Deterministic)"))

p_bands <- ggplot(bands_df, aes(x = time, y = value)) +
  geom_line(aes(colour = component), linewidth = 0.5) +
  facet_wrap(~component, ncol = 1, scales = "free_y") +
  scale_colour_manual(values = pallete[1:4]) +
  labs(title = "EMD Frequency Components (Palma et al., 2025)",
       subtitle = "From most stochastic (noise) to most deterministic (trend)",
       x = "Time", y = "Value") +
  theme_new() +
  theme(legend.position = "none")
print(p_bands)

################################################################################
################# Example 4: EMD on Ecological Time Series ######################
################################################################################

waterbird <- read.csv("input_data/waterbird_migration.csv")
wb_counts <- waterbird$count

emd_wb <- EMD::emd(wb_counts, boundary = "wave")
cat("Waterbird decomposition:", emd_wb$nimf, "IMFs extracted\n")

n_wb <- emd_wb$nimf
par(mfrow = c(min(n_wb + 2, 8), 1), mar = c(2, 4, 1.5, 1))

plot(wb_counts, type = "l",
     main = "Waterbird Counts (Original)",
     ylab = "Count", col = pallete[1])

for (i in 1:min(n_wb, 6)) {
  plot(emd_wb$imf[, i], type = "l",
       col = pallete[(i %% 7) + 1],
       main = paste0("IMF ", i), ylab = paste0("C_", i))
}

plot(emd_wb$residue, type = "l", col = pallete[3],
     main = "Residue (Population Trend)", ylab = "Trend")
par(mfrow = c(1, 1))

# Ecological interpretation:
# IMF 1: Month-to-month random fluctuations (weather, counting errors)
# IMF 2-3: Seasonal migration patterns (dominant ecological signal)
# IMF 4+: Multi-year cycles (El Niño, habitat changes)
# Residue: Long-term population trend (wetland restoration impact)

################################################################################
################# Example 5: EEMD — Ensemble EMD for Robustness ###############
################################################################################

# Standard EMD can suffer from MODE MIXING: different frequency components
# leak into the same IMF when the signal has intermittent oscillations.
#
# EEMD fixes this by:
#   1. Adding white noise to the signal
#   2. Applying EMD
#   3. Repeating many times with different noise realisations
#   4. Averaging the IMFs across all trials
#
# The noise cancels out in the average, but separates frequencies more cleanly.

eemd_wb <- EMD::emd(wb_counts, boundary = "wave", max.sift = 200)

cat("Standard EMD IMFs:", emd_wb$nimf, "\n")
cat("EEMD IMFs:",         eemd_wb$nimf, "\n")

# Compare first 3 IMFs from both methods
par(mfrow = c(3, 2), mar = c(3, 4, 2, 1))
for (i in 1:min(3, emd_wb$nimf)) {
  plot(emd_wb$imf[, i], type = "l", col = pallete[1],
       main = paste0("EMD - IMF ", i), ylab = "")
  if (i <= eemd_wb$nimf) {
    plot(eemd_wb$imf[, i], type = "l", col = pallete[2],
         main = paste0("EEMD - IMF ", i), ylab = "")
  }
}
par(mfrow = c(1, 1))

################################################################################
################# Example 6: IMF Components as ML Features ####################
################################################################################

# CRITICAL CONNECTION TO DAY 4 (Palma et al., 2025):
# IMF components become INPUT FEATURES for machine learning models.
#
# Instead of feeding the raw signal into Random Forest or XGBoost, we
# decompose first and create a FEATURE MATRIX where each column is an IMF
# component or reconstructed frequency band.

# Build the ML feature matrix from EMD output
emd_features <- data.frame(count = wb_counts)

for (i in 1:emd_wb$nimf) {
  emd_features[[paste0("imf_", i)]] <- emd_wb$imf[, i]
}
emd_features$residue <- emd_wb$residue

# Add reconstructed frequency bands
bands_wb <- reconstruct_emd_bands(emd_wb)
emd_features$high_stoch   <- bands_wb$high_stoch
emd_features$medium_stoch <- bands_wb$medium_stoch
emd_features$low_stoch    <- bands_wb$low_stoch
emd_features$trend_comp   <- bands_wb$trend_comp

# Add the target: next month's count (for supervised learning)
emd_features$count_next <- c(wb_counts[-1], NA)

print_output(head(emd_features, 10), "EMD Feature Matrix for ML")
cat("Dimensions:", dim(emd_features), "\n")
cat("Columns:", paste(names(emd_features), collapse = ", "), "\n")

# This data frame goes directly into Day 4's ML pipeline.

################################################################################
################# Example 7: EMD Feature Importance Preview ###################
################################################################################

# Which EMD features best correlate with next month's count?

emd_cors <- emd_features %>%
  dplyr::select(-count) %>%
  na.omit() %>%
  cor()

cat("\n--- Correlation of EMD features with count_next ---\n")
count_cors       <- sort(emd_cors[, "count_next"], decreasing = TRUE)
print(round(count_cors, 3))

# Trend and low-stochasticity components should have the highest correlation —
# they capture slow-moving dynamics that are most predictable.

best_feature <- names(count_cors)[2]  # skip count_next itself

p_scatter <- emd_features %>%
  na.omit() %>%
  ggplot(aes(x = .data[[best_feature]], y = count_next)) +
  geom_point(alpha = 0.5, colour = pallete[1]) +
  geom_smooth(method = "lm", colour = pallete[2]) +
  labs(title = paste("EMD Feature vs Next Month Count:", best_feature),
       x = best_feature, y = "Next Month Count") +
  theme_new()
print(p_scatter)

cat("\n--- BRIDGE TO DAY 4 ---\n")
cat("Tomorrow, these EMD-derived columns become the input matrix for:\n")
cat("  - Random Forest regression\n")
cat("  - XGBoost regression\n")
cat("  - Lasso regression\n")
cat("The ML model learns which frequency bands best predict future abundance.\n")

################################################################################
#################### EMD Exercises ############################################
################################################################################

# Exercise 1: EMD on Simulated Ecological Data
# Create a synthetic time series with 3 known components:
# a) High-frequency: 3*sin(2*pi*t/10),  t = 1:400
# b) Seasonal:       8*sin(2*pi*t/52)   (annual cycle for weekly data)
# c) Linear trend:   0.05*t
# d) Noise:          rnorm(400, 0, 1)
# Apply EMD. How many IMFs are extracted? Do they match the true components?

# Exercise 2: Aphid EMD Decomposition
# Load aphid_climate_brazil.csv and apply EMD to aphid_abundance:
# a) How many IMFs are produced?
# b) Plot all IMFs and the residue
# c) Which IMF best captures the seasonal pattern?
# d) Sum the first 2 IMFs — does this look like noise?

# Exercise 3: Reconstructed Component Analysis
# Using the waterbird EMD results:
# a) Reconstruct the 4 frequency bands using reconstruct_emd_bands()
# b) Verify: max(abs(original - sum(all_imfs) - residue)) should be ~0
# c) Plot each band and discuss its ecological meaning
# d) Which band has the highest variance? The lowest?

# Exercise 4: EMD Feature Matrix Construction
# Using aphid_climate_brazil.csv:
# a) Apply EMD to aphid_abundance
# b) Apply EMD to temp_mean
# c) Create a combined feature data frame with IMFs from both series,
#    reconstructed frequency bands, and target = next week's aphid abundance
# d) How many total features are created?
# e) Compute correlations with the target — which features are strongest?

# Exercise 5: EMD vs STL Comparison
# Using waterbird data:
# a) Apply both EMD and STL decomposition
# b) Compare the trend from EMD (residue) with the trend from STL
# c) Plot them overlaid — are they similar?
# d) Which method is more flexible? Which requires fewer assumptions?
# e) When would you prefer EMD over STL in ecological research?

################################################################################
#################### EMD — Answers ###########################################
################################################################################

# Answer 1: EMD on Synthetic Ecological Signal
set.seed(100)
t_ex1        <- 1:400
high_ex1     <- 3 * sin(2 * pi * t_ex1 / 10)
seasonal_ex1 <- 8 * sin(2 * pi * t_ex1 / 52)
trend_ex1    <- 0.05 * t_ex1
noise_ex1    <- rnorm(400, 0, 1)
signal_ex1   <- high_ex1 + seasonal_ex1 + trend_ex1 + noise_ex1

emd_ex1 <- EMD::emd(signal_ex1, boundary = "wave")
cat("IMFs extracted:", emd_ex1$nimf, "\n")

par(mfrow = c(emd_ex1$nimf + 2, 1), mar = c(2, 4, 1.5, 1))
plot(signal_ex1, type = "l", main = "Original", ylab = "")
for (i in 1:emd_ex1$nimf) {
  plot(emd_ex1$imf[, i], type = "l",
       main = paste("IMF", i), ylab = "")
}
plot(emd_ex1$residue, type = "l", main = "Residue", ylab = "")
par(mfrow = c(1, 1))
# EMD typically extracts 4-6 IMFs; IMF 1 = noise + high-freq,
# a middle IMF = seasonal, residue = trend.

# Answer 2: Aphid EMD
aphid_br  <- read.csv("aphid_climate_brazil.csv")
emd_aphid <- EMD::emd(aphid_br$aphid_abundance, boundary = "wave")
cat("Aphid EMD:", emd_aphid$nimf, "IMFs\n")

par(mfrow = c(min(emd_aphid$nimf + 2, 8), 1), mar = c(2, 4, 1.5, 1))
plot(aphid_br$aphid_abundance, type = "l", main = "Aphid Abundance", ylab = "")
for (i in 1:min(emd_aphid$nimf, 6)) {
  plot(emd_aphid$imf[, i], type = "l", main = paste("IMF", i), ylab = "")
}
plot(emd_aphid$residue, type = "l", main = "Residue", ylab = "")
par(mfrow = c(1, 1))

# d) Sum of first 2 IMFs — high-frequency noise
noise_sum_aphid <- rowSums(emd_aphid$imf[, 1:min(2, emd_aphid$nimf), drop = FALSE])
plot(noise_sum_aphid, type = "l", main = "Sum of IMF 1+2 (High-frequency Noise)")

# Answer 3: Reconstruction Verification
bands_wb_a3  <- reconstruct_emd_bands(emd_wb)

reconstructed <- rowSums(emd_wb$imf) + emd_wb$residue
max_error     <- max(abs(wb_counts - reconstructed))
cat("Max reconstruction error:", max_error, "\n")  # Should be ~0

par(mfrow = c(4, 1), mar = c(3, 4, 2, 1))
plot(bands_wb_a3$high_stoch,   type = "l", main = "High Stochasticity",   ylab = "")
plot(bands_wb_a3$medium_stoch, type = "l", main = "Medium Stochasticity", ylab = "")
plot(bands_wb_a3$low_stoch,    type = "l", main = "Low Stochasticity",    ylab = "")
plot(bands_wb_a3$trend_comp,   type = "l", main = "Trend (Deterministic)", ylab = "")
par(mfrow = c(1, 1))

cat("Variance — High:",   var(bands_wb_a3$high_stoch),
    " Medium:", var(bands_wb_a3$medium_stoch),
    " Low:",    var(bands_wb_a3$low_stoch),
    " Trend:",  var(bands_wb_a3$trend_comp), "\n")

# Answer 4: Combined Feature Matrix from Aphid + Temperature
emd_aph4  <- EMD::emd(aphid_br$aphid_abundance, boundary = "wave")
emd_temp4 <- EMD::emd(aphid_br$temp_mean, boundary = "wave")

feat_df <- data.frame(row.names = seq_len(nrow(aphid_br)))
for (i in 1:emd_aph4$nimf) {
  feat_df[[paste0("aphid_imf_", i)]] <- emd_aph4$imf[, i]
}
feat_df$aphid_residue <- emd_aph4$residue

for (i in 1:emd_temp4$nimf) {
  feat_df[[paste0("temp_imf_", i)]] <- emd_temp4$imf[, i]
}
feat_df$temp_residue <- emd_temp4$residue
feat_df$target       <- c(aphid_br$aphid_abundance[-1], NA)

cat("Total features:", ncol(feat_df) - 1, "\n")

feat_cors <- cor(feat_df %>% na.omit())
print(round(sort(feat_cors[, "target"], decreasing = TRUE), 3))

# Answer 5: EMD vs STL Trend Comparison
waterbird_ts_a5 <- waterbird %>%
  mutate(Month = yearmonth(date)) %>%
  as_tsibble(index = Month)

stl_wb_a5 <- waterbird_ts_a5 %>%
  model(STL(count ~ season(window = "periodic"))) %>%
  components()

emd_trend_a5 <- emd_wb$residue
stl_trend_a5 <- as.numeric(stl_wb_a5$trend)

plot(emd_trend_a5, type = "l", col = pallete[1], lwd = 2,
     main = "Trend Comparison: EMD Residue vs STL Trend",
     ylab = "Trend")
lines(stl_trend_a5, col = pallete[2], lwd = 2)
legend("bottomright",
       legend = c("EMD Residue", "STL Trend"),
       col = pallete[1:2], lwd = 2)

# d) EMD is more flexible — no assumptions about seasonality or trend shape.
#    STL assumes additive structure and a fixed seasonal window.
# e) EMD is preferred when: data has non-stationary seasonality, multiple
#    overlapping cycles, or non-sinusoidal oscillations — common in ecology
#    where population dynamics are driven by nonlinear processes.

################################################################################
#################### EMD — Advanced Examples ##################################
################################################################################

# Advanced Example 1: Instantaneous Frequency via Hilbert Transform
# After extracting IMFs, we can compute the instantaneous frequency using
# the Hilbert-Huang Transform (HHT).

hilbert_imf1 <- EMD::hilbertspec(emd_wb$imf)

cat("\n--- Instantaneous frequency of IMF 1 (first 20 values) ---\n")
print(round(head(hilbert_imf1$instantfreq[, 1], 20), 4))

# High instantaneous frequency → stochastic IMF
# Low instantaneous frequency  → deterministic/trend IMF

# Advanced Example 2: IMF Reconstruction with Reconstruction Weights
# Instead of binary grouping, apply a linear combination of IMFs as a
# noise-filtered version of the original series.

n_imfs_wb      <- emd_wb$nimf
# Use only IMFs 2 to J-1 (skip pure noise and pure trend)
filtered_signal <- rowSums(
  emd_wb$imf[, 2:max(2, n_imfs_wb - 1), drop = FALSE]
)

p_filtered <- ggplot() +
  geom_line(aes(x = seq_along(wb_counts), y = wb_counts),
            colour = "grey60", linewidth = 0.5) +
  geom_line(aes(x = seq_along(filtered_signal), y = filtered_signal),
            colour = pallete[1], linewidth = 1) +
  labs(title = "Noise-Filtered Waterbird Counts (Middle IMFs Only)",
       subtitle = "Grey = original; colour = filtered (IMF 2 to J-1)",
       x = "Month", y = "Count") +
  theme_new()
print(p_filtered)

# Advanced Example 3: EEMD Stability — Comparing Multiple Runs
# Demonstrate that EEMD produces stable IMFs across different noise seeds.

eemd_run1 <- EMD::emd(wb_counts, boundary = "wave", max.sift = 100)
eemd_run2 <- EMD::emd(wb_counts + rnorm(length(wb_counts), 0, 0.1),
                       boundary = "wave", max.sift = 100)

cat("EEMD run 1 IMFs:", eemd_run1$nimf, "\n")
cat("EEMD run 2 IMFs:", eemd_run2$nimf, "\n")

if (eemd_run1$nimf == eemd_run2$nimf) {
  imf_corr <- diag(cor(eemd_run1$imf, eemd_run2$imf))
  cat("Correlation between corresponding IMFs across runs:\n")
  print(round(imf_corr, 3))
  # High correlations (> 0.95) confirm IMF stability.
}

# Advanced Example 4: Cross-Frequency Analysis — Aphid and Temperature IMFs
# Compute correlations between IMFs of the aphid series and IMFs of the
# temperature series to identify which frequency bands are coupled.

if (emd_aph4$nimf == emd_temp4$nimf) {
  cross_freq_cor <- cor(emd_aph4$imf, emd_temp4$imf)
  cat("\n--- Cross-frequency correlation matrix (Aphid IMFs vs Temp IMFs) ---\n")
  print(round(cross_freq_cor, 2))
  # Strong off-diagonal correlations reveal cross-frequency coupling.
}

# Advanced Example 5: EMD Feature Matrix for Supervised Learning
# Build the full EMD feature matrix with lagged versions of each band,
# ready for Random Forest or XGBoost on Day 4.

n_rows_wb      <- length(wb_counts)
n_lags         <- 3

full_feature_df <- data.frame(
  count      = wb_counts,
  high_stoch = bands_wb$high_stoch,
  med_stoch  = bands_wb$medium_stoch,
  low_stoch  = bands_wb$low_stoch,
  trend      = bands_wb$trend_comp
)

# Add lags for each band
for (band_name in c("high_stoch", "med_stoch", "low_stoch", "trend")) {
  for (lag_k in 1:n_lags) {
    col_name <- paste0(band_name, "_lag", lag_k)
    full_feature_df[[col_name]] <- dplyr::lag(full_feature_df[[band_name]],
                                               n = lag_k)
  }
}
full_feature_df$count_next <- c(wb_counts[-1], NA)

print_output(head(full_feature_df, 6), "Full EMD Feature Matrix with Lags")
cat("Total columns:", ncol(full_feature_df), "\n")

################################################################################
#################### EMD — Advanced Exercises #################################
################################################################################

# Advanced Exercise 1: EMD on Multiple Ecological Variables
# Using aphid_climate_brazil.csv, apply EMD to all numeric variables
# (aphid_abundance, temp_mean, temp_max, rainfall, humidity, wind_speed):
# a) For each variable, print the number of IMFs extracted
# b) Do all variables yield the same number of IMFs? Why or why not?
# c) Compute the correlation between the residue (trend) of aphid abundance
#    and the residue of temp_mean — are the long-term trends correlated?

# Advanced Exercise 2: Tuning EMD Parameters
# The max.sift parameter in EMD::emd() controls the stopping criterion.
# a) Apply EMD to wb_counts with max.sift = 10, 50, 100, 300
# b) Does the number of IMFs change?
# c) Compare the IMF 1 across different max.sift values
# d) At what sift count do the IMFs stabilise?

# Advanced Exercise 3: Spectral Analysis of IMFs
# For each IMF from emd_wb, compute the dominant frequency.
# a) Apply spectrum() to each IMF
# b) Extract the frequency corresponding to the peak power
# c) Convert frequency to period (period = 1/frequency * sampling_interval)
# d) Order the IMFs by their dominant period — does the ordering match
#    the expected "high frequency to low frequency" sequence?

# Advanced Exercise 4: EMD for Anomaly Detection
# a) Apply EMD to pop_vol population_index
# b) Extract the high-stochasticity band (IMF 1)
# c) Standardise this band: (band - mean) / sd
# d) Flag values with |standardised| > 2.5 as potential anomalies
# e) Do these anomalies correspond to the known disturbance events?

# Advanced Exercise 5: Comparing EMD Feature Sets for Prediction
# Using waterbird data, build three feature sets:
#   Set A: raw lagged counts (lag 1-6)
#   Set B: individual IMFs (all IMFs as columns)
#   Set C: reconstructed bands (high, medium, low, trend) with 3 lags each
# a) For each set, fit a linear model to predict count_next
# b) Compare R-squared and RMSE on a held-out test set (last 24 months)
# c) Which feature set gives the best predictions?
# d) Discuss: why might the band decomposition outperform raw lags?
