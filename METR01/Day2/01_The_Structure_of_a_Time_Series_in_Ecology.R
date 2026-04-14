####################################################################################################
###
### File:    01_The_Structure_of_a_Time_Series_in_Ecology.R
### Purpose: Examples and exercises for The Structure of a Time Series in Ecology
### Authors: Gabriel Rodrigues Palma
### Date:    17/06/25
###
####################################################################################################
# load packages -----
source('00_source.r')
source('generate_datasets.R')

################################################################################
############ The Structure of a Time Series in Ecology ########################
################################################################################
# A time series is a sequence of observations recorded at successive points
# in time: y = {y_1, y_2, ..., y_T}. In ecology, this could be monthly
# butterfly counts, daily temperature readings, or annual population estimates.
# The key features are: an INDEX (time), OBSERVATIONS (values), and
# REGULARITY (consistent time intervals).
#
# In the tidyverts ecosystem (fpp3), the core data structure is a tsibble —
# a time-aware tibble that understands temporal ordering and gaps. Every
# tsibble must have a unique time index; duplicate time points cause an error,
# which acts as a built-in quality check for field data entry mistakes.

################################################################################
############################ Example 1: Creating a tsibble #####################
################################################################################

# Imagine you have been counting butterflies in a meadow since 2015.
# The raw data is stored in a CSV with a date column and an abundance column.
# We use load_ecological_tsibble() (defined in 00_source.r) to load and
# convert it into a tsibble in one step, keeping our analysis scripts clean.

butterfly_ts <- load_ecological_tsibble(
  file_path  = "input_data/butterfly_meadow_monitoring.csv",
  index_col  = "date",
  index_type = "yearmonth"
)

print_output(head(butterfly_ts, 10), "Butterfly tsibble (first 10 rows)")

# Key properties of a tsibble:
# - index: the time column (Month — automatically created as yearmonth)
# - interval: automatically detected (1M = one month)
# - no duplicate time points allowed

cat("Index variable:", index_var(butterfly_ts), "\n")
cat("Interval:", format(interval(butterfly_ts)), "\n")
cat("Number of observations:", nrow(butterfly_ts), "\n")

################################################################################
################### Example 2: Creating a tsibble from scratch #################
################################################################################

# Sometimes you start with raw field notes rather than a CSV.
# Let's create a simple annual population tsibble manually using tsibble().
# This is useful for entering small datasets directly in R without a CSV.

red_deer_population <- tsibble(
  Year  = 2010:2024,
  Count = c(120, 135, 142, 155, 148, 163, 171, 180, 175, 190,
            198, 205, 215, 220, 228),
  index = Year
)

print_output(red_deer_population, "Red deer population (annual)")

# The tsibble knows this is annual data (interval = 1Y).
# It will warn if there are gaps or duplicates — a protection against
# accidentally skipping a survey year.

cat("Detected interval:", format(interval(red_deer_population)), "\n")
cat("Series spans", min(red_deer_population$Year),
    "to", max(red_deer_population$Year), "\n")

################################################################################
################### Example 3: Keyed tsibbles — Multiple Sites #################
################################################################################

# In ecology, we often monitor multiple species or sites simultaneously.
# The KEY variable identifies different series within the same tsibble.
# This allows a single data object to hold an entire monitoring network,
# with all tidyverse and fable operations applied to each series automatically.

# Load the forest canopy cover dataset (3 forest types, quarterly)
# The key = forest_type creates 3 separate time series in one object.
forest_ts <- load_ecological_tsibble(
  file_path  = "input_data/forest_canopy_cover.csv",
  index_col  = "date",
  index_type = "yearquarter",
  key_col    = "forest_type"
)

print_output(head(forest_ts, 12), "Forest canopy tsibble (keyed by forest type)")

# Each combination of key variables defines a separate time series.
# Here we have 3 series: Oak_Woodland, Sitka_Spruce, Native_Mixed
cat("Number of keys (series):", n_keys(forest_ts), "\n")
cat("Key variables:", key_vars(forest_ts), "\n")

################################################################################
################### Example 4: Prison data — Complex keys ######################
################################################################################

# The fpp3 textbook uses the Australian prison population dataset.
# This demonstrates a multi-keyed tsibble with four grouping variables —
# analogous to species × site × method × sex in ecological monitoring networks.

prison_ts <- readr::read_csv("https://OTexts.com/fpp3/extrafiles/prison_population.csv") %>%
  dplyr::mutate(Quarter = yearquarter(Date)) %>%
  dplyr::select(-Date) %>%
  as_tsibble(key = c(State, Gender, Legal, Indigenous),
             index = Quarter)

print_output(head(prison_ts), "Australian prison population (multi-keyed)")
cat("Number of distinct series:", n_keys(prison_ts), "\n")

# Each unique combination of State × Gender × Legal × Indigenous
# forms a separate time series — this mirrors ecological designs
# (e.g., species × site × sampling method × season).

################################################################################
################### Example 5: Time Plots — autoplot() ########################
################################################################################

# The most basic visualisation: plot the series against time.
# autoplot() from feasts automatically creates appropriate time plots
# and colours multiple series by their key variables.

# Butterfly abundance over time
plot_butterfly_abundance <- autoplot(butterfly_ts, abundance) +
  theme_new() +
  labs(title    = "Monthly Butterfly Abundance",
       subtitle = "Burren Meadow, Ireland (2015-2024)",
       y        = "Abundance (count)",
       x        = "Month") +
  scale_colour_manual(values = pallete)

print(plot_butterfly_abundance)

# Interpretation: Strong seasonal pattern with peaks in summer.
# Possible upward trend over the decade — a positive sign for conservation.
# Near-zero counts in winter months indicate that butterflies are inactive.

# Forest canopy cover — multiple series in one plot
# The key (forest_type) automatically produces one coloured line per forest.
plot_forest_canopy <- autoplot(forest_ts, canopy_cover_pct) +
  theme_new() +
  labs(title    = "Quarterly Canopy Cover by Forest Type",
       subtitle = "Irish forests (2010-2024)",
       y        = "Canopy Cover (%)",
       x        = "Quarter") +
  scale_colour_manual(values = pallete)

print(plot_forest_canopy)

# Interpretation: Sitka Spruce (evergreen conifer) has consistently high
# canopy cover (~85-95%). Oak Woodland (deciduous) shows strong seasonal
# fluctuation — leaves drop in autumn/winter. Native Mixed is intermediate.

################################################################################
############### Example 6: Seasonal Plots — gg_season() ########################
################################################################################

# Seasonal plots overlay each year on the same axis to reveal repeating
# annual patterns. This is crucial in ecology where phenology (timing of
# biological events) drives many processes. Each line represents one year,
# making it easy to compare peak timing and amplitude across years.

# CO2 data — the famous Mauna Loa dataset (built into R)
co2_ts <- as_tsibble(co2)

plot_co2_seasonal <- co2_ts %>%
  gg_season(value, period = "year") +
  theme_new() +
  labs(title    = "Atmospheric CO2 — Seasonal Pattern",
       subtitle = "Mauna Loa Observatory (each line = one year)",
       y        = "CO2 concentration (ppm)")

print(plot_co2_seasonal)

# Interpretation: CO2 drops during Northern Hemisphere summer (plants absorb
# CO2 during photosynthesis) and rises in winter (decomposition + fossil fuels).
# Each year's line is higher than the last — the relentless upward trend.

# Butterfly abundance seasonal pattern — phenological consistency
plot_butterfly_seasonal <- butterfly_ts %>%
  gg_season(abundance, period = "year") +
  theme_new() +
  labs(title    = "Butterfly Abundance — Seasonal Pattern",
       subtitle = "Each line represents one year",
       y        = "Abundance (count)")

print(plot_butterfly_seasonal)

# Interpretation: Clear peak in June-August across all years.
# Winter months consistently near zero. The seasonal pattern is stable —
# this is a strongly seasonal ecological time series.

# Irish energy demand (aimsir17 package) — sub-daily seasonality
# This shows that seasonality can operate at any time scale, not just annual.
energy_demand_ts <- eirgrid17 %>%
  dplyr::distinct(date, .keep_all = TRUE) %>%
  as_tsibble(index = date)

# Step 1: Create the base plot object
p <- energy_demand_ts %>%
  fill_gaps() %>%
  gg_season(IEDemand, period = "week")

# Step 2: Extract factor levels from the plot's internal data
id_levels <- levels(p$data$id)
n_levels  <- length(id_levels)

# Step 3: Pick 3 break indices — min, mid, max
break_idx <- round(c(1, n_levels))

# Step 4: Override the colour scale with only 3 legend entries
plot_energy_weekly <- p +
  scale_colour_gradientn(
    colours = scales::hue_pal()(9),
    breaks  = break_idx,
    labels  = id_levels[break_idx]
  ) +
  theme_new() +
  labs(y        = "MWh",
       title    = "Weekly Energy Demand Pattern in Ireland (2017)",
       subtitle = "Lower demand on weekends, peak on weekdays")

print(plot_energy_weekly)

################################################################################
############### Example 7: Subseries Plots — gg_subseries() ####################
################################################################################

# Subseries plots show the data for each season (e.g., each month)
# in separate mini-panels. The blue horizontal line shows the mean for
# that season. This makes it easy to compare seasonal patterns AND spot
# trends within each individual season — useful for detecting phenological
# shifts (e.g., are June butterflies becoming more abundant over time?).

plot_butterfly_subseries <- butterfly_ts %>%
  gg_subseries(abundance) +
  theme_new() +
  theme(axis.text.x = element_text(size = 8, angle = 45, hjust = 1)) +
  labs(title    = "Butterfly Abundance — Subseries Plot",
       subtitle = "Blue line = mean for each month",
       y        = "Abundance (count)")

print(plot_butterfly_subseries)

# Interpretation: Each panel shows one month across all years.
# June, July, August have the highest means (blue lines).
# Upward slope within any panel signals a phenological trend in that month.

################################################################################
############### Example 8: Lag Plots — gg_lag() ################################
################################################################################

# Lag plots show y_t vs y_{t-k} for various lags k.
# Strong linear relationships indicate autocorrelation — observations close
# in time are similar. This is essential for identifying temporal dependencies
# before choosing an ARIMA or ETS model.

#Y = {Y_1, Y_2, ..., T_t , ..., Y_T}
#t = {1, 2, ..., T}

#Sample of 5 observations
#y = {y1, y2, y3, y4, y5}

#lag(y) = {y1, y2, y3, y4}
# Correlation between y and lag(y)
# cor(y, lag(y, 1)) 
# y2, y3, y4, y5
# y1, y2, y3, y4

y <- c(1, 2, 3, 4, 5)
y_lag <- lag(y)
cor(y[2:5], y_lag[2:5])

plot_co2_lags <- co2_ts %>%
  gg_lag(value, geom = "point", lags = 1:12, alpha = 0.4) +
  theme_new() +
  labs(x        = "lag(CO2, k)",
       y        = "CO2 concentration (ppm)",
       title    = "Lag Plots for Atmospheric CO2",
       subtitle = "Strong autocorrelation at all lags; weakest at lag 6")

print(plot_co2_lags)

# Interpretation:
# - Lag 1: Very strong positive linear relationship — today's CO2 is nearly
#   identical to last month's (strong short-term persistence).
# - Lag 6: Weakest relationship — 6 months apart means opposite seasons.
# - Lag 12: Strong again — same month, one year apart.

plot_butterfly_lags <- butterfly_ts %>%
  gg_lag(abundance, geom = "point", lags = 1:12, alpha = 0.5) +
  theme_new() +
  labs(title    = "Lag Plots for Butterfly Abundance",
       subtitle = "Strongest correlation at lags 1 and 12")

print(plot_butterfly_lags)

################################################################################
############### Example 9: Autocorrelation — ACF() #############################
################################################################################

# The autocorrelation function (ACF) summarises the lag plots into one chart.
# It shows the correlation coefficient r_k at each lag k.
# Formula: r_k = sum((y_t - y_bar)(y_{t-k} - y_bar)) / sum((y_t - y_bar)^2)
# Blue dashed lines indicate 95% significance bounds.
# Spikes outside those bounds indicate statistically significant autocorrelation.

plot_acf_butterfly <- butterfly_ts %>%
  ACF(abundance, lag_max = 36) %>%
  autoplot() +
  theme_new() +
  labs(title    = "ACF of Butterfly Abundance",
       subtitle = "Sinusoidal ACF pattern reveals strong seasonality (period = 12 months)")

print(plot_acf_butterfly)

# Interpretation:
# - The ACF oscillates with a period of 12 — the hallmark of monthly seasonal data.
# - Peaks at lags 12, 24, 36 (same month, different years).
# - Troughs at lags 6, 18, 30 (opposite season).
# - Slow decay suggests a trend component as well.

plot_acf_co2 <- co2_ts %>%
  ACF(value, lag_max = 48) %>%
  autoplot() +
  theme_new() +
  labs(title    = "ACF of Atmospheric CO2",
       subtitle = "Very slow decay = strong trend; oscillation = seasonality")

print(plot_acf_co2)

# Interpretation: The ACF barely decays — this means the series has a very
# strong trend (non-stationary). The slight oscillation at period 12 confirms
# the annual seasonal cycle driven by vegetation photosynthesis.

################################################################################
############ Structure of a Time Series in Ecology Exercises ###################
################################################################################

# Exercise 1: Create a tsibble from wetland data
# Load "wetland_bird_counts.csv" and convert it to a weekly tsibble using
# load_ecological_tsibble(). Plot the total waterbird counts using autoplot().
# Describe the seasonal pattern you observe. Is the peak in summer or winter?

# Exercise 2: Seasonal plot for wetland birds
# Using the wetland bird tsibble from Exercise 1, create a seasonal plot
# (gg_season) for the mallard counts. What months have the highest mallard
# abundance? Why might this be ecologically (hint: consider migration)?

# Exercise 3: River water quality tsibble
# Load "river_water_quality.csv" and convert to a daily tsibble.
# Create autoplot() for both dissolved_oxygen_mgl and water_temperature_c.
# Use patchwork (plot_do / plot_temp) to stack the plots vertically.
# Describe the relationship between temperature and dissolved oxygen.

# Exercise 4: Lag plots for butterfly temperature
# Using the butterfly_ts data, create lag plots for temperature_c at lags
# 1 through 12. At which lag is the relationship weakest? At which lag is
# it strongest? Explain why in ecological terms.

# Exercise 5: ACF comparison across three series
# Compute and plot the ACF for: (a) butterfly abundance,
# (b) butterfly temperature, and (c) CO2 concentration.
# Use patchwork (acf_a | acf_b | acf_c) to display all three side by side.
# Compare: which series shows the strongest trend? Which shows clearest seasonality?

################################################################################
######### Structure of a Time Series in Ecology — Advanced ####################
################################################################################
# Advanced examples explore multi-key tsibbles, custom time grids, and
# combining multiple visualisation tools to build a complete exploratory
# analysis pipeline for ecological monitoring data.

# Advanced Example 1: Multi-species keyed tsibble from wetland data
wetland_species_ts <- load_ecological_tsibble(
  file_path  = "input_data/wetland_bird_counts.csv",
  index_col  = "date",
  index_type = "yearweek"
) %>%
  tidyr::pivot_longer(c(mallard, teal, lapwing),
                      names_to  = "species",
                      values_to = "abundance_count") %>%
  as_tsibble(index = date, key = species)

autoplot(wetland_species_ts, abundance_count) +
  theme_new() +
  scale_colour_manual(values = pallete) +
  labs(title    = "Advanced Example 1: Multi-species Wetland Tsibble",
       subtitle = "Each species is a separate keyed time series",
       y        = "Count", colour = "Species")

# Advanced Example 2: Seasonal plot with multiple species
wetland_species_ts %>%
  gg_season(abundance_count, period = "year") +
  facet_wrap(~ species, scales = "free_y") +
  theme_new() +
  labs(title    = "Advanced Example 2: Seasonal Patterns by Species",
       subtitle = "Each panel shows one species; lines = years",
       y        = "Count")

# Advanced Example 3: ACF for each species in the keyed tsibble
wetland_species_ts %>%
  ACF(abundance_count, lag_max = 52) %>%
  autoplot() +
  facet_wrap(~ species, ncol = 1) +
  theme_new() +
  labs(title    = "Advanced Example 3: ACF by Species",
       subtitle = "Teal shows the sharpest seasonal cutoff (strict winter migrant)")

# Advanced Example 4: summarise_ts_features() on all species
lapply(c("mallard", "teal", "lapwing"), function(sp) {
  ts_subset <- wetland_species_ts %>% dplyr::filter(species == sp)
  features  <- summarise_ts_features(ts_subset, "abundance_count")
  cat("\nFeatures for", sp, ":\n")
  print(features)
})

# Advanced Example 5: Combining autoplot + annotate for data storytelling
butterfly_ts %>%
  autoplot(abundance) +
  theme_new() +
  annotate("rect",
           xmin  = yearmonth("2020 Jan"),
           xmax  = yearmonth("2020 Dec"),
           ymin  = -Inf, ymax = Inf,
           alpha = 0.15, fill = "steelblue") +
  annotate("text",
           x     = yearmonth("2020 Jul"),
           y     = max(butterfly_ts$abundance, na.rm = TRUE) * 0.9,
           label = "COVID lockdown",
           colour = "steelblue") +
  labs(title    = "Advanced Example 5: Annotated Time Plot",
       subtitle = "Shaded region highlights the 2020 lockdown period",
       y        = "Abundance (count)")

################################################################################
####### Structure of a Time Series in Ecology — Advanced Exercises #############
################################################################################

# Advanced Exercise 1: Subseries plot for forest canopy
# Filter forest_ts to "Oak_Woodland" only and create a subseries plot of
# canopy_cover_pct. Which quarter shows the strongest trend over time?
# Does any quarter appear to be declining?

# Advanced Exercise 2: Interpret a lag plot for forest canopy
# Using forest_ts filtered to "Sitka_Spruce", create lag plots for
# canopy_cover_pct at lags 1 through 8 (quarterly data, so lag 4 = one year).
# Which lag shows the strongest relationship? What does this imply about
# the degree of seasonal memory in a conifer plantation?

# Advanced Exercise 3: Multi-species ACF comparison
# Using the multi-species wetland tsibble (Advanced Example 1):
# Compute and plot the ACF for each species (mallard, teal, lapwing).
# Use patchwork to display them in a single figure.
# Which species has the most complex autocorrelation pattern? Why?

# Advanced Exercise 4: Detect phenological shifts
# Using butterfly_ts gg_subseries(abundance):
# (a) Has the peak month (highest mean) shifted over the observation period?
# (b) Are there months where the trend within the subseries is clearly increasing?
# (c) What ecological mechanism might explain a shift in peak timing?

# Advanced Exercise 5: Create an annotated diagnostic dashboard
# For the butterfly_ts data, create a 2×2 patchwork layout:
# (a) Top-left:  autoplot() of abundance
# (b) Top-right: gg_season() seasonal plot
# (c) Bottom-left: gg_subseries() subseries plot
# (d) Bottom-right: ACF plot (lag_max = 36)
# Add a shared title using the patchwork plot_annotation() function.

################################################################################
######### Structure of a Time Series in Ecology — Answers (Ex 1-5) ############
################################################################################

# Answer 1:
wetland_ts <- load_ecological_tsibble(
  file_path  = "input_data/wetland_bird_counts.csv",
  index_col  = "date",
  index_type = "yearweek"
)

autoplot(wetland_ts, total_waterbirds) +
  theme_new() +
  labs(title    = "Weekly Waterbird Counts at Shannon Wetland",
       subtitle = "2018-2024",
       y        = "Total Waterbird Count",
       x        = "Week")
# The seasonal pattern shows peaks in winter (October-March) when migratory
# waterbirds are present, and lower counts in summer when many species
# have migrated north to breed.

# Answer 2:
wetland_ts %>%
  gg_season(mallard, period = "year") +
  theme_new() +
  labs(title    = "Seasonal Plot — Mallard Counts",
       subtitle = "Shannon Wetland (each line = one year)",
       y        = "Mallard Count")
# Mallard abundance peaks in winter months (November-February).
# Irish populations are supplemented by continental European birds
# seeking milder winters — a classic irruption pattern.

# Answer 3:
river_ts <- load_ecological_tsibble(
  file_path  = "input_data/river_water_quality.csv",
  index_col  = "date",
  index_type = "date"
)

plot_do <- autoplot(river_ts, dissolved_oxygen_mgl) +
  theme_new() +
  labs(title = "Dissolved Oxygen — River Liffey",
       y     = "DO (mg/L)")

plot_temp <- autoplot(river_ts, water_temperature_c) +
  theme_new() +
  labs(title = "Water Temperature — River Liffey",
       y     = "Temperature (°C)")

plot_do / plot_temp
# Temperature and dissolved oxygen are inversely related (Henry's Law):
# warmer water holds less dissolved oxygen.
# Summer = high temperature + low DO; winter = the opposite.

# Answer 4:
butterfly_ts %>%
  gg_lag(temperature_c, geom = "point", lags = 1:12, alpha = 0.5) +
  theme_new() +
  labs(title    = "Lag Plots for Temperature",
       x        = "lag(Temperature, k)",
       y        = "Temperature (°C)")
# The relationship is weakest at lag 6 (opposite season: January vs July).
# It is strongest at lag 12 (same month, one year apart) and lag 1
# (consecutive months are thermally very similar in a temperate climate).

# Answer 5:
acf_butterfly <- butterfly_ts %>%
  ACF(abundance, lag_max = 36) %>%
  autoplot() +
  theme_new() +
  labs(title = "ACF: Butterfly Abundance")

acf_temp <- butterfly_ts %>%
  ACF(temperature_c, lag_max = 36) %>%
  autoplot() +
  theme_new() +
  labs(title = "ACF: Temperature")

acf_co2 <- co2_ts %>%
  ACF(value, lag_max = 36) %>%
  autoplot() +
  theme_new() +
  labs(title = "ACF: CO2")

acf_butterfly | acf_temp | acf_co2
# CO2 shows the strongest trend (very slow ACF decay — almost no decay).
# Temperature shows the clearest pure seasonality (regular sine wave in ACF).
# Butterfly abundance shows both trend and seasonality (decaying oscillation).

# Answer 6:
forest_ts %>%
  dplyr::filter(forest_type == "Oak_Woodland") %>%
  gg_subseries(canopy_cover_pct) +
  theme_new() +
  labs(title    = "Oak Woodland Canopy Cover — Subseries Plot",
       subtitle = "Blue line = mean for each quarter",
       y        = "Canopy Cover (%)")
# Q2 (spring leafing) and Q3 (peak summer) typically show increasing trends
# because oak woodland recovery and climate warming are extending the growing season.

# Answer 7:
forest_ts %>%
  dplyr::filter(forest_type == "Sitka_Spruce") %>%
  gg_lag(canopy_cover_pct, geom = "point", lags = 1:8, alpha = 0.5) +
  theme_new() +
  labs(title    = "Lag Plots: Sitka Spruce Canopy Cover",
       subtitle = "Quarterly data — lag 4 = one year")
# Lag 1 (one quarter) is extremely strong — Sitka Spruce canopy barely changes
# between consecutive quarters, reflecting the stability of evergreen conifers.
# Lag 4 (one year) is also very strong, confirming that inter-annual variation
# is small. This suggests a slow-changing process with very long memory.
