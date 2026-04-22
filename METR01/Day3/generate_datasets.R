####################################################################################################
###
### File:    generate_datasets.R
### Purpose: Generate synthetic ecological datasets for Day 3
### Authors: Gabriel Rodrigues Palma
### Date:    17/06/25
###
####################################################################################################

# This script generates four synthetic ecological datasets used throughout
# Day 3: Fundamentals of Time Series Analysis II.
# Run this script ONCE before working through the other Day 3 scripts.

library(tidyverse)

set.seed(42)

# Constants ---------------------------
N_YEARS_WATERBIRD  <- 15   # waterbird monitoring period (2010-2024)
N_YEARS_LADYBIRD   <- 7    # ladybird-aphid study period (2018-2024)
N_YEARS_POP_VOL    <- 20   # population volatility period (2005-2024)
N_WEEKS_APHID_BR   <- 211  # aphid + climate dataset (matching Palma et al. 2025)

################################################################################
#### Dataset 1: Waterbird Migration (Monthly, 2010-2024) ######################
################################################################################

# Monthly waterbird counts from a wetland monitoring station.
# Strong seasonal migration patterns: peak arrivals in autumn (Oct-Nov),
# departures in spring (Mar-Apr), low counts in summer (Jun-Aug).
# Mimics seasonal dynamics of migratory waders and wildfowl in Irish and
# European wetlands.

n_months <- 12 * N_YEARS_WATERBIRD
dates_waterbird <- seq(as.Date("2010-01-01"), by = "month",
                        length.out = n_months)
years_wb  <- as.numeric(format(dates_waterbird, "%Y"))
months_wb <- as.numeric(format(dates_waterbird, "%m"))

# Trend: slight increase over time (wetland restoration success)
trend_wb <- seq(50, 80, length.out = n_months)

# Seasonal component: strong migration pattern
seasonal_pattern_wb <- c(120, 90, 60, 40, 30, 20, 15, 25, 60, 150, 180, 160)
seasonal_wb         <- seasonal_pattern_wb[months_wb]

# Interannual variability and noise
noise_wb  <- rnorm(n_months, mean = 0, sd = 15)
cycle_wb  <- 10 * sin(2 * pi * (1:n_months) / 60)   # ~5-year cycle

waterbird_counts <- round(pmax(0, trend_wb + seasonal_wb + cycle_wb + noise_wb))

waterbird_migration <- tibble(
  date    = dates_waterbird,
  year    = years_wb,
  month   = months_wb,
  count   = waterbird_counts,
  species = "Mixed waders"
)

write.csv(waterbird_migration, "waterbird_migration.csv", row.names = FALSE)
cat("Saved: waterbird_migration.csv (", nrow(waterbird_migration), "rows)\n")

################################################################################
#### Dataset 2: Ladybird-Aphid Dynamics (Weekly, 2018-2024) ###################
################################################################################

# Weekly predator-prey data simulating ladybird (Coccinellidae) and
# aphid (Aphididae) interactions on cereal crops.
# Lotka-Volterra-inspired dynamics with seasonal forcing:
#   - Aphids increase in spring/summer, decline in winter
#   - Ladybirds follow with a lag (classic predator-prey cycles)

n_weeks_lb    <- 52 * N_YEARS_LADYBIRD
dates_lb      <- seq(as.Date("2018-01-01"), by = "week",
                      length.out = n_weeks_lb)

# Initialise populations
aphid_pop    <- numeric(n_weeks_lb)
ladybird_pop <- numeric(n_weeks_lb)
aphid_pop[1]    <- 200
ladybird_pop[1] <- 30

# Lotka-Volterra parameters with seasonal forcing
ALPHA_APHID   <- 0.04    # aphid intrinsic growth rate (per week)
BETA_PRED     <- 0.0008  # predation rate
GAMMA_LADY    <- 0.02    # ladybird death rate
DELTA_LADY    <- 0.0003  # ladybird reproduction efficiency from prey

for (t in 2:n_weeks_lb) {
  week_of_year    <- ((t - 1) %% 52) + 1
  seasonal_force  <- 1 + 0.8 * sin(2 * pi * (week_of_year - 10) / 52)

  d_aphid    <- ALPHA_APHID * seasonal_force * aphid_pop[t-1] -
                BETA_PRED * aphid_pop[t-1] * ladybird_pop[t-1]
  d_ladybird <- DELTA_LADY * aphid_pop[t-1] * ladybird_pop[t-1] -
                GAMMA_LADY * ladybird_pop[t-1]

  aphid_pop[t]    <- max(5,   aphid_pop[t-1]    + d_aphid    + rnorm(1, 0, 8))
  ladybird_pop[t] <- max(1,   ladybird_pop[t-1] + d_ladybird + rnorm(1, 0, 2))
  aphid_pop[t]    <- min(aphid_pop[t],    1500)
  ladybird_pop[t] <- min(ladybird_pop[t],  200)
}

ladybird_aphid_dynamics <- tibble(
  date           = dates_lb,
  year           = as.numeric(format(dates_lb, "%Y")),
  week           = as.numeric(format(dates_lb, "%U")),
  aphid_count    = round(aphid_pop),
  ladybird_count = round(ladybird_pop)
)

write.csv(ladybird_aphid_dynamics,
          "ladybird_aphid_dynamics.csv",
          row.names = FALSE)
cat("Saved: ladybird_aphid_dynamics.csv (", nrow(ladybird_aphid_dynamics), "rows)\n")

################################################################################
#### Dataset 3: Population Volatility (Monthly, 2005-2024) ####################
################################################################################

# Monthly insect population index showing variance clustering:
#   - Calm periods with low variability (stable environment)
#   - Disturbance events (drought, habitat loss) trigger high volatility
# Demonstrates the ecological analogue of GARCH: population variability is
# autocorrelated, with clusters of instability.

n_months_pv     <- 12 * N_YEARS_POP_VOL
dates_pv        <- seq(as.Date("2005-01-01"), by = "month",
                        length.out = n_months_pv)

# Base signal: seasonal oscillation + trend
base_trend_pv    <- seq(100, 130, length.out = n_months_pv)
base_seasonal_pv <- 20 * sin(2 * pi * (1:n_months_pv) / 12)

# GARCH-like volatility: calm then disturbed periods
sigma_pv  <- numeric(n_months_pv)
sigma_pv[1] <- 5
ALPHA0_GARCH <- 4
ALPHA1_GARCH <- 0.25
BETA1_GARCH  <- 0.65

epsilon_pv <- rnorm(n_months_pv)

for (t in 2:n_months_pv) {
  sigma_pv[t] <- sqrt(ALPHA0_GARCH +
                       ALPHA1_GARCH * (sigma_pv[t-1] * epsilon_pv[t-1])^2 +
                       BETA1_GARCH  * sigma_pv[t-1]^2)
}

# Disturbance shocks at specific times (ecological disturbance events)
shocks_pv <- rep(0, n_months_pv)
shocks_pv[55:70]   <- 15   # drought 2009-2010
shocks_pv[130:145] <- 20   # habitat fragmentation 2015-2016
shocks_pv[185:200] <- 18   # extreme weather 2020-2021

volatility_noise_pv <- sigma_pv * epsilon_pv + shocks_pv * abs(rnorm(n_months_pv))

population_index_pv <- round(
  pmax(10, base_trend_pv + base_seasonal_pv + volatility_noise_pv)
)

population_volatility <- tibble(
  date              = dates_pv,
  year              = as.numeric(format(dates_pv, "%Y")),
  month             = as.numeric(format(dates_pv, "%m")),
  population_index  = population_index_pv,
  disturbance_event = ifelse(shocks_pv > 0, "yes", "no")
)

write.csv(population_volatility,
          "population_volatility.csv",
          row.names = FALSE)
cat("Saved: population_volatility.csv (", nrow(population_volatility), "rows)\n")

################################################################################
#### Dataset 4: Aphid + Climate Brazil (Weekly, mimicking Palma et al. 2025) ##
################################################################################

# Weekly aphid abundance + 5 climate variables, mimicking the structure from
# Palma et al. (2025) Ecological Informatics paper on time series
# reconstruction and ML forecasting.
#
# Variables follow Table 1 of the paper:
#   temp_mean:       mean weekly temperature (C)
#   temp_max:        maximum weekly temperature (C)
#   rainfall:        weekly accumulated rainfall (mm)
#   humidity:        mean relative humidity (%)
#   wind_speed:      mean wind speed (m/s)
#   aphid_abundance: total aphid count (target variable)

dates_ab   <- seq(as.Date("2015-06-01"), by = "week",
                   length.out = N_WEEKS_APHID_BR)
week_idx   <- 1:N_WEEKS_APHID_BR
season_cyc <- 2 * pi * (week_idx %% 52) / 52

# Climate variables with seasonal patterns typical of Southern Brazil
temp_mean_ab  <- 18 + 6 * sin(season_cyc - pi/4) + rnorm(N_WEEKS_APHID_BR, 0, 1.5)
temp_max_ab   <- temp_mean_ab + 5 + rnorm(N_WEEKS_APHID_BR, 0, 1)
rainfall_ab   <- pmax(0, 30 + 25 * sin(season_cyc + pi/6) +
                        rnorm(N_WEEKS_APHID_BR, 0, 15))
humidity_ab   <- 70 + 10 * sin(season_cyc + pi/3) + rnorm(N_WEEKS_APHID_BR, 0, 4)
wind_speed_ab <- 3  +  1 * sin(season_cyc)        + rnorm(N_WEEKS_APHID_BR, 0, 0.5)

# Aphid abundance: driven by temperature (positive, lagged ~3 weeks),
# rainfall (negative, lagged ~5 weeks), and autoregressive dynamics.
# Structure follows Palma et al. (2025).
aphid_ab      <- numeric(N_WEEKS_APHID_BR)
aphid_ab[1:6] <- round(runif(6, 10, 80))

for (t in 7:N_WEEKS_APHID_BR) {
  ar_part      <- 0.4  * aphid_ab[t-1] + 0.15 * aphid_ab[t-2]
  temp_effect  <- 2.5  * temp_mean_ab[t-3]
  rain_effect  <- -0.4 * rainfall_ab[t-5]
  humid_effect <- 0.3  * humidity_ab[t-2]
  season_eff   <- 30   * sin(season_cyc[t] - pi/6)

  aphid_ab[t] <- max(0, round(
    -20 + ar_part + temp_effect + rain_effect + humid_effect +
      season_eff + rnorm(1, 0, 12)
  ))
}

aphid_climate_brazil <- tibble(
  date             = dates_ab,
  year             = as.numeric(format(dates_ab, "%Y")),
  week             = as.numeric(format(dates_ab, "%U")),
  temp_mean        = round(temp_mean_ab,  2),
  temp_max         = round(temp_max_ab,   2),
  rainfall         = round(rainfall_ab,   2),
  humidity         = round(humidity_ab,   2),
  wind_speed       = round(wind_speed_ab, 2),
  aphid_abundance  = aphid_ab
)

write.csv(aphid_climate_brazil,
          "aphid_climate_brazil.csv",
          row.names = FALSE)
cat("Saved: aphid_climate_brazil.csv (", nrow(aphid_climate_brazil), "rows)\n")

################################################################################
cat("\n=== All Day 3 datasets generated successfully ===\n")
cat("Files created in working directory:\n")
cat("  1. waterbird_migration.csv\n")
cat("  2. ladybird_aphid_dynamics.csv\n")
cat("  3. population_volatility.csv\n")
cat("  4. aphid_climate_brazil.csv\n")
