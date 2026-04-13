####################################################################################################
###
### File:    01_R_Syntax_&_Variables.R
### Purpose: Examples and exercises for R Syntax & Variables
### Authors: Gabriel Rodrigues Palma
### Date:    17/06/25
###
####################################################################################################
# load packeges -----
source('00_source.r')

################################################################################
############################ R Syntax & Variables ##############################
################################################################################
# Example 1: Basic Variable Assignment for Species Data
species_name <- "Quercus robur"
tree_height <- 15.3
measurement_date <- "2024-06-15"
cat("Species:", species_name, "\nHeight:", tree_height, "meters\n")
cat("Date:", measurement_date, "\n")

# Example 2: Ecological Site Variables
site_id <- "FOREST_01"
latitude <- 45.2386
longitude <- -121.7849
elevation <- 1250
researcher <- "Dr. Smith"

# Example 3: Wildlife Population Data
deer_count <- 47
survey_area <- 125.5
population_density <- deer_count / survey_area
cat("Population density:", population_density, "deer per hectare\n")

# Example 4: Environmental Measurements
temperature <- 18.5
humidity <- 72
wind_speed <- 3.2
weather_station <- "Alpine_Station_02"
print(paste("Weather at", weather_station, ":", temperature, "°C"))

# Example 5: Biodiversity Index Calculation
species_richness <- 24
shannon_index <- 2.87
simpson_index <- 0.91
cat("Biodiversity metrics - Richness:", species_richness, 
    "Shannon:", shannon_index, "\n")

################################################################################
####################### R Syntax & Variables Exercises ######################
################################################################################

# Exercise 1: Create variables for bird observation data
# Create variables: bird_species, observation_time, location, observer_name
# Print a summary statement

# Exercise 2: Calculate forest carbon storage
# Variables: plot_area (hectares), biomass_per_hectare (tons), carbon_fraction
# Calculate total carbon stored

# Exercise 3: Water quality measurements
# Create variables for pH, dissolved_oxygen, temperature, turbidity
# Create a station_id and measurement_date

# Exercise 4: Pollinator survey data
# Variables: bee_count, butterfly_count, total_flowers, sampling_duration
# Calculate pollinator density per flower

# Exercise 5: Soil analysis results
# Create variables for nitrogen, phosphorus, potassium levels
# Add soil_type and collection_date variables

################################################################################
##################### R Syntax & Variables - Advanced #######################
################################################################################
# Example 1: Marine Ecosystem Variables
ocean_temperature <- 14.2
salinity <- 35.1
depth_meters <- 250
marine_protected_area <- TRUE
fish_abundance <- 156

# Example 2: Climate Data Assignment
annual_precipitation <- 1250.5
mean_temperature <- 12.8
growing_season_days <- 180
climate_zone <- "Temperate"
data_source <- "Weather_Station_5"

# Example 3: Conservation Status Variables
species_scientific <- "Panthera tigris"
common_name <- "Bengal Tiger"
population_estimate <- 2500
conservation_status <- "Endangered"
habitat_area <- 50000

# Example 4: Phenology Study Variables
flowering_date <- "2024-04-15"
leaf_emergence <- "2024-03-22"
fruit_set <- "2024-06-10"
study_year <- 2024
phenology_site <- "Meadow_Plot_A"

# Example 5: Microhabitat Characterization
canopy_cover <- 85.5
understory_density <- 0.65
litter_depth <- 4.2
soil_moisture <- 28.7
microhabitat_id <- "MH_001"

# Example 6: Mathematical operations I
population_size <- 2500
square_root_pop <- sqrt(population_size)
natural_log <- log(population_size)
log_base_10 <- log10(population_size)
exponential_growth <- exp(2.5)
sine_angle <- sin(pi/4)
cosine_angle <- cos(pi/3)
tangent_angle <- tan(pi/6)
absolute_value <- abs(-15.7)
ceiling_value <- ceiling(12.3)
floor_value <- floor(12.8)
cat("Square root:", round(square_root_pop, 2), "\n")
cat("Natural log:", round(natural_log, 2), "\n")
cat("Exponential:", round(exponential_growth, 2), "\n")

# Example 6: Mathematical operations II
species_counts <- c(45, 32, 28, 67, 23, 51, 39, 44)
mean_abundance <- mean(species_counts)
median_abundance <- median(species_counts)
standard_deviation <- sd(species_counts)
variance_value <- var(species_counts)
min_count <- min(species_counts)
max_count <- max(species_counts)
sum_total <- sum(species_counts)
range_values <- range(species_counts)
quantiles <- quantile(species_counts, c(0.25, 0.75))
cat("Mean:", mean_abundance, "\n")
cat("SD:", round(standard_deviation, 2), "\n")
cat("Range:", min_count, "to", max_count, "\n")

# Example 6: Mathematical operations III
community_matrix <- matrix(c(12, 8, 15, 6, 20, 4, 18, 9, 14, 7, 11, 3),
                           nrow = 3, ncol = 4)
matrix_transpose <- t(community_matrix)
row_sums <- rowSums(community_matrix)
col_means <- colMeans(community_matrix)
matrix_multiplication <- community_matrix %*% t(community_matrix)
determinant_val <- det(matrix_multiplication)
eigen_values <- eigen(matrix_multiplication)$values
matrix_inverse <- solve(matrix_multiplication)
cat("Row sums:", row_sums, "\n")
cat("Column means:", col_means, "\n")
cat("Determinant:", round(determinant_val, 2), "\n")



################################################################################
################# R Syntax & Variables - Advanced Exercises ##################
################################################################################

# Exercise 1: Coral reef survey data
# Create variables for coral_coverage, fish_diversity, water_visibility
# Add reef_name and survey_date

# Exercise 2: Migratory bird tracking
# Variables: species_name, band_number, capture_location, wing_length
# Calculate body_condition_index using weight and wing_length

# Exercise 3: Forest fire impact assessment
# Create variables for burn_severity, recovery_time, species_loss
# Add fire_date and assessment_date

# Exercise 4: Pollination network analysis
# Variables: plant_species, pollinator_species, interaction_frequency
# Add network_site and observation_period

# Exercise 5: Invasive species monitoring
# Create variables for invasive_cover, native_cover, invasion_rate
# Calculate impact_ratio and add monitoring_date
