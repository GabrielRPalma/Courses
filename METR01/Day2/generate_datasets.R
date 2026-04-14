####################################################################################################
###
### File:    generate_datasets.R
### Purpose: Generate synthetic ecological CSV datasets for Day 2 examples and exercises
### Authors: Gabriel Rodrigues Palma
### Date:    17/06/25
###
####################################################################################################

# This script generates four realistic ecological datasets used across all Day 2 scripts.
# Run this script FIRST before running any other Day 2 scripts.
# All datasets are saved as CSV files in the current working directory.
#
# Datasets generated:
#   1. butterfly_meadow_monitoring.csv  — monthly butterfly abundance with climate drivers
#   2. wetland_bird_counts.csv          — weekly waterbird counts with water level
#   3. river_water_quality.csv          — daily dissolved oxygen and temperature
#   4. forest_canopy_cover.csv          — quarterly canopy cover for 3 forest types

# Dataset generation constants (ALL_CAPS for configuration values)
RANDOM_SEED          <- 2026    # Ensures reproducibility across all datasets
N_MONTHS_BUTTERFLY   <- 120     # 10 years of monthly butterfly data (2015-2024)
N_QUARTERS_FOREST    <- 60      # 15 years of quarterly forest data (2010-2024)
TEMP_WARMING_RATE    <- 0.02    # Monthly warming trend (°C per month)
BUTTERFLY_RECOVERY   <- 0.1     # Monthly abundance increase trend (population recovery)
FOREST_OAK_RECOVERY  <- 0.05    # Quarterly canopy recovery rate for Oak Woodland
FOREST_SPRUCE_GROWTH <- 0.08    # Quarterly canopy growth rate for Sitka Spruce
POLLUTION_EVENTS     <- 8       # Number of random DO pollution events in river data

set.seed(RANDOM_SEED)

################################################################################
############## Dataset 1: Butterfly Meadow Monitoring ##########################
################################################################################
# Monthly butterfly abundance, temperature, and rainfall
# Location: Irish meadow site (fictional "Burren Meadow")
# Period: January 2015 – December 2024 (N_MONTHS_BUTTERFLY = 120 observations)
# Ecological context:
#   - Abundance peaks in summer (Jun-Aug), near zero in winter
#   - Positively correlated with temperature (thermal performance)
#   - Weakly negatively correlated with excess rainfall (reduces flight activity)
#   - Slight upward trend in abundance (population recovery)
#   - Slight warming trend in temperature (climate change signal)

butterfly_dates <- seq(as.Date("2015-01-01"), by = "month",
                       length.out = N_MONTHS_BUTTERFLY)
month_index_butterfly <- rep(1:12, 10)

# Temperature: seasonal pattern with slight warming trend (Irish climate)
# Mean range: ~7°C in winter, ~16°C in summer
temperature_butterfly <- 10 +
  3 * sin(2 * pi * (month_index_butterfly - 4) / 12) +
  TEMP_WARMING_RATE * (1:N_MONTHS_BUTTERFLY) +
  rnorm(N_MONTHS_BUTTERFLY, 0, 1.2)

# Rainfall: seasonal pattern (wetter in winter — characteristic of Atlantic Ireland)
# Mean range: ~80mm in summer, ~120mm in winter
rainfall_butterfly <- 100 - 20 * sin(2 * pi * (month_index_butterfly - 4) / 12) +
  rnorm(N_MONTHS_BUTTERFLY, 0, 15)
rainfall_butterfly <- pmax(rainfall_butterfly, 5)  # minimum 5mm per month

# Butterfly abundance: driven by temperature, rainfall, seasonal cycle, and trend
seasonal_butterfly  <- pmax(0, sin(2 * pi * (month_index_butterfly - 3) / 12)) * 80
trend_butterfly     <- BUTTERFLY_RECOVERY * (1:N_MONTHS_BUTTERFLY)
temp_effect_bf      <- 2.5 * (temperature_butterfly - mean(temperature_butterfly))
rain_effect_bf      <- -0.3 * (rainfall_butterfly - mean(rainfall_butterfly))
noise_butterfly     <- rnorm(N_MONTHS_BUTTERFLY, 0, 8)

abundance_butterfly <- round(pmax(0,
                                  20 + seasonal_butterfly + trend_butterfly +
                                    temp_effect_bf + rain_effect_bf +
                                    noise_butterfly))

butterfly_data <- data.frame(
  date          = butterfly_dates,
  year          = as.integer(format(butterfly_dates, "%Y")),
  month         = as.integer(format(butterfly_dates, "%m")),
  abundance     = abundance_butterfly,
  temperature_c = round(temperature_butterfly, 1),
  rainfall_mm   = round(rainfall_butterfly, 1)
)

write.csv(butterfly_data, "input_data/butterfly_meadow_monitoring.csv", row.names = FALSE)
cat("Created: butterfly_meadow_monitoring.csv (", N_MONTHS_BUTTERFLY, "obs)\n")

################################################################################
############## Dataset 2: Wetland Bird Counts ##################################
################################################################################
# Weekly waterbird species counts with water level
# Location: Shannon Wetland, Ireland (fictional monitoring site)
# Period: January 2018 – December 2024 (weekly observations)
# Ecological context:
#   - Waterbird counts peak in winter (migratory species from continental Europe)
#   - Mallard: year-round resident, peaks in winter; positively related to water level
#   - Teal: winter migrant, absent in summer (weeks 13-39)
#   - Lapwing: winter visitor with declining trend (habitat loss pressure)
#   - Total count = sum of all three species

start_date_wetland <- as.Date("2018-01-01")
end_date_wetland   <- as.Date("2024-12-29")
wetland_dates      <- seq(start_date_wetland, end_date_wetland, by = "week")
n_weeks_wetland    <- length(wetland_dates)
week_of_year       <- as.integer(format(wetland_dates, "%U"))

# Water level (metres): higher in winter, lower in summer
# Range: 1.5–3.5m, peak in January, trough in July
water_level_wetland <- 2.5 + 0.8 * cos(2 * pi * (week_of_year - 1) / 52) +
  rnorm(n_weeks_wetland, 0, 0.3)
water_level_wetland <- round(pmax(water_level_wetland, 0.5), 2)

# Mallard: common year-round resident, peaks in winter, responds to water level
mallard_count <- round(pmax(0,
                            45 + 30 * cos(2 * pi * (week_of_year - 1) / 52) +
                              8 * (water_level_wetland - mean(water_level_wetland)) +
                              rnorm(n_weeks_wetland, 0, 10)))

# Teal: winter migrant (weeks 1-12 and 40-52), rare in summer
teal_seasonal <- ifelse(week_of_year <= 12 | week_of_year >= 40,
                        50 + 25 * cos(2 * pi * (week_of_year - 1) / 52),
                        2)
teal_count <- round(pmax(0, teal_seasonal + rnorm(n_weeks_wetland, 0, 8)))

# Lapwing: winter visitor with a long-term declining trend
lapwing_count <- round(pmax(0,
                            35 + 20 * cos(2 * pi * (week_of_year - 1) / 52) -
                              0.03 * (1:n_weeks_wetland) +  # declining trend
                              rnorm(n_weeks_wetland, 0, 7)))

total_waterbirds_count <- mallard_count + teal_count + lapwing_count

wetland_data <- data.frame(
  date             = wetland_dates,
  year             = as.integer(format(wetland_dates, "%Y")),
  week             = week_of_year,
  mallard          = mallard_count,
  teal             = teal_count,
  lapwing          = lapwing_count,
  total_waterbirds = total_waterbirds_count,
  water_level_m    = water_level_wetland
)

write.csv(wetland_data, "input_data/wetland_bird_counts.csv", row.names = FALSE)
cat("Created: wetland_bird_counts.csv (", n_weeks_wetland, "obs)\n")

################################################################################
############## Dataset 3: River Water Quality ##################################
################################################################################
# Daily dissolved oxygen and water temperature
# Location: River Liffey, Dublin, Ireland (fictional monitoring station)
# Period: January 2022 – December 2024 (daily observations)
# Ecological context:
#   - Dissolved oxygen is inversely related to temperature (Henry's Law)
#   - Seasonal pattern: high DO in winter, low DO in summer
#   - Occasional pollution events cause sudden DO drops
#   - DO > CRITICAL_DO_THRESHOLD (5 mg/L) required for aquatic life

start_date_river <- as.Date("2022-01-01")
end_date_river   <- as.Date("2024-12-31")
river_dates      <- seq(start_date_river, end_date_river, by = "day")
n_days_river     <- length(river_dates)
day_of_year      <- as.integer(format(river_dates, "%j"))

# Water temperature: seasonal, 4-18°C range typical for an Irish river
water_temperature <- 11 + 7 * sin(2 * pi * (day_of_year - 80) / 365) +
  rnorm(n_days_river, 0, 1.5)
water_temperature <- round(pmax(water_temperature, 1), 1)

# Dissolved oxygen (mg/L): inversely related to temperature
# Range: ~12 mg/L in winter, ~8 mg/L in summer
dissolved_oxygen <- 12 - 0.25 * water_temperature +
  rnorm(n_days_river, 0, 0.8)

# Add occasional pollution events (sudden DO drops representing effluent spills)
pollution_event_days    <- sample(1:n_days_river, POLLUTION_EVENTS)
dissolved_oxygen[pollution_event_days] <- dissolved_oxygen[pollution_event_days] -
  runif(POLLUTION_EVENTS, 2, 4)
dissolved_oxygen <- round(pmax(dissolved_oxygen, 2), 1)

river_data <- data.frame(
  date                  = river_dates,
  year                  = as.integer(format(river_dates, "%Y")),
  month                 = as.integer(format(river_dates, "%m")),
  day_of_year           = day_of_year,
  water_temperature_c   = water_temperature,
  dissolved_oxygen_mgl  = dissolved_oxygen
)

write.csv(river_data, "input_data/river_water_quality.csv", row.names = FALSE)
cat("Created: river_water_quality.csv (", n_days_river, "obs)\n")

################################################################################
############## Dataset 4: Forest Canopy Cover ##################################
################################################################################
# Quarterly canopy cover percentage at 3 Irish forest types
# Location: Ireland — representative of three contrasting management types
# Period: Q1 2010 – Q4 2024 (N_QUARTERS_FOREST = 60 observations per forest type)
# Forest types:
#   Oak_Woodland:  deciduous, strong seasonal signal (leaves drop in Q4/Q1)
#   Sitka_Spruce:  evergreen conifer, stable high canopy with growth trend
#   Native_Mixed:  intermediate deciduous/evergreen mix
# Ecological context:
#   - Oak shows phenological seasonality: low canopy in Q1 (winter), peak in Q3 (summer)
#   - Spruce is stable year-round; slight long-term increase from ongoing growth
#   - Native mixed shows intermediate seasonality

forest_dates    <- seq(as.Date("2010-01-01"), by = "quarter",
                       length.out = N_QUARTERS_FOREST)
quarter_index   <- rep(1:4, 15)

# Oak woodland: strong seasonal canopy (drops ~15% in Q1 winter, peaks in Q3 summer)
oak_seasonal_offsets <- c(-15, 10, 20, -5)  # Q1, Q2, Q3, Q4
oak_canopy_cover <- 65 + oak_seasonal_offsets[quarter_index] +
  FOREST_OAK_RECOVERY * (1:N_QUARTERS_FOREST) +
  rnorm(N_QUARTERS_FOREST, 0, 3)
oak_canopy_cover <- round(pmin(pmax(oak_canopy_cover, 15), 98), 1)

# Sitka spruce plantation: stable high canopy with slow growth trend
spruce_canopy_cover <- 82 + 2 * sin(2 * pi * quarter_index / 4) +
  FOREST_SPRUCE_GROWTH * (1:N_QUARTERS_FOREST) +
  rnorm(N_QUARTERS_FOREST, 0, 1.5)
spruce_canopy_cover <- round(pmin(pmax(spruce_canopy_cover, 60), 99), 1)

# Native mixed forest: intermediate seasonality between oak and spruce
mixed_seasonal_offsets <- c(-8, 8, 12, -3)  # Q1, Q2, Q3, Q4
mixed_canopy_cover <- 70 + mixed_seasonal_offsets[quarter_index] +
  0.03 * (1:N_QUARTERS_FOREST) +
  rnorm(N_QUARTERS_FOREST, 0, 2.5)
mixed_canopy_cover <- round(pmin(pmax(mixed_canopy_cover, 25), 98), 1)

forest_data <- data.frame(
  date            = rep(forest_dates, 3),
  year            = rep(as.integer(format(forest_dates, "%Y")), 3),
  quarter         = rep(quarter_index, 3),
  forest_type     = rep(c("Oak_Woodland", "Sitka_Spruce", "Native_Mixed"),
                        each = N_QUARTERS_FOREST),
  canopy_cover_pct = c(oak_canopy_cover, spruce_canopy_cover, mixed_canopy_cover)
)

write.csv(forest_data, "input_data/forest_canopy_cover.csv", row.names = FALSE)
cat("Created: forest_canopy_cover.csv (", nrow(forest_data), "obs)\n")

################################################################################
cat("\n--- All four datasets generated successfully! ---\n")
cat("1. butterfly_meadow_monitoring.csv  (", N_MONTHS_BUTTERFLY,
    "obs, monthly, 2015-2024)\n")
cat("2. wetland_bird_counts.csv          (", n_weeks_wetland,
    "obs, weekly,  2018-2024)\n")
cat("3. river_water_quality.csv          (", n_days_river,
    "obs, daily,   2022-2024)\n")
cat("4. forest_canopy_cover.csv          (", nrow(forest_data),
    "obs, quarterly, 2010-2024)\n")
