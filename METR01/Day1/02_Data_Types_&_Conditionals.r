####################################################################################################
###
### File:    02_Data_Types_&_Conditionals.R
### Purpose: Examples and exercises for Data types and conditionals
### Authors: Gabriel Rodrigues Palma
### Date:    17/06/25
###
####################################################################################################

# load packeges -----
source('00_source.r')

################################################################################
########################## Data Types & Conditionals ########################
################################################################################

# Example 1: Species Classification by Conservation Status
species_population <- 250
if (species_population < 100) {
  status <- "Critically Endangered"
} else if (species_population < 500) {
  status <- "Endangered"
} else {
  status <- "Vulnerable"
}
cat("Conservation Status:", status, "\n")

# Example 2: Habitat Suitability Assessment
temperature <- 22.5
precipitation <- 850
if (temperature > 20 && precipitation > 800) {
  habitat_quality <- "Optimal"
} else if (temperature > 15 && precipitation > 500) {
  habitat_quality <- "Suitable"
} else {
  habitat_quality <- "Marginal"
}

# Example 3: Water Quality Classification
ph_level <- 7.2
dissolved_oxygen <- 8.5
if (ph_level >= 6.5 && ph_level <= 8.5 && dissolved_oxygen >= 7) {
  water_quality <- "Excellent"
} else if (ph_level >= 6.0 && ph_level <= 9.0 && dissolved_oxygen >= 5) {
  water_quality <- "Good"
} else {
  water_quality <- "Poor"
}

# Example 4: Pollinator Activity Assessment
temperature <- 25
wind_speed <- 2.1
cloud_cover <- 30
if (temperature >= 18 && wind_speed <= 5 && cloud_cover <= 50) {
  pollinator_activity <- "High"
} else {
  pollinator_activity <- "Low"
}

# Example 5: Forest Fire Risk Evaluation
moisture_content <- 12
temperature <- 35
wind_speed <- 25
if (moisture_content < 15 && temperature > 30 && wind_speed > 20) {
  fire_risk <- "Extreme"
} else if (moisture_content < 20 && temperature > 25) {
  fire_risk <- "High"
} else {
  fire_risk <- "Moderate"
}

################################################################################
################### Data Types & Conditionals Exercises #####################
################################################################################

# Exercise 1: Bird migration timing assessment
# Use day_of_year variable to classify migration periods
# Early (< 100), Peak (100-150), Late (> 150)

# Exercise 2: Soil fertility classification
# Use nitrogen, phosphorus, potassium levels
# Classify as High, Medium, or Low fertility

# Exercise 3: Predator-prey ratio evaluation
# Calculate predator_prey_ratio and classify ecosystem balance
# Balanced (0.1-0.3), Predator-heavy (>0.3), Prey-heavy (<0.1)

# Exercise 4: Coral bleaching assessment
# Use water temperature to predict bleaching risk
# Low (<29°C), Medium (29-31°C), High (>31°C)

# Exercise 5: Seed germination success prediction
# Use temperature and moisture to predict success rates
# Create logical conditions for optimal germination

################################################################################
################## Data Types & Conditionals - II #####################
################################################################################

# Example 1: Species Distribution Modeling
elevation <- 1500
slope <- 25
aspect <- "North"
if (elevation > 1000 && elevation < 2000 && slope < 30) {
  if (aspect == "North" || aspect == "Northeast") {
    habitat_suitability <- "High"
  } else {
    habitat_suitability <- "Medium"
  }
} else {
  habitat_suitability <- "Low"
}

# Example 2: Breeding Success Factors
nest_height <- 12.5
predator_presence <- FALSE
food_availability <- "High"
weather_conditions <- "Favorable"
if (!predator_presence && food_availability == "High") {
  if (weather_conditions == "Favorable" && nest_height > 10) {
    breeding_success <- "Very High"
  } else {
    breeding_success <- "High"
  }
} else {
  breeding_success <- "Low"
}

# Example 3: Vegetation Type Classification
annual_rainfall <- 1200
mean_temperature <- 18
soil_type <- "Clay"
if (annual_rainfall > 1000 && mean_temperature > 15) {
  if (soil_type == "Clay" || soil_type == "Loam") {
    vegetation_type <- "Deciduous Forest"
  } else {
    vegetation_type <- "Mixed Forest"
  }
} else if (annual_rainfall < 500) {
  vegetation_type <- "Grassland"
} else {
  vegetation_type <- "Woodland"
}

# Example 4: Fish Stock Assessment
biomass_index <- 0.75
fishing_pressure <- "Medium"
recruitment_rate <- 0.85
if (biomass_index > 0.8 && recruitment_rate > 0.7) {
  stock_status <- "Healthy"
} else if (biomass_index > 0.4 && fishing_pressure != "High") {
  stock_status <- "Cautious"
} else {
  stock_status <- "Overfished"
}

# Example 5: Invasive Species Risk Assessment
establishment_probability <- 0.7
economic_impact <- "High"
control_feasibility <- "Low"
species_origin <- "Exotic"
if (species_origin == "Exotic" && establishment_probability > 0.5) {
  if (economic_impact == "High" && control_feasibility == "Low") {
    invasion_risk <- "Critical"
  } else {
    invasion_risk <- "Moderate"
  }
} else {
  invasion_risk <- "Low"
}

################################################################################
############### Data Types & Conditionals - Advanced Exercises ###############
################################################################################

# Exercise 1: Climate change vulnerability assessment
# Use temperature_change, precipitation_change, species_adaptability
# Classify vulnerability as Low, Medium, High, or Critical

# Exercise 2: Pollination network stability
# Use network_connectivity, pollinator_diversity, plant_diversity
# Determine network stability and resilience

# Exercise 3: Marine protected area effectiveness
# Use fish_abundance_inside, fish_abundance_outside, protection_duration
# Assess MPA effectiveness categories

# Exercise 4: Forest succession stage determination
# Use tree_age, canopy_cover, understory_development
# Classify as Pioneer, Early, Mid, or Late succession

# Exercise 5: Wildlife corridor quality assessment
# Use corridor_width, habitat_connectivity, human_disturbance
# Determine corridor effectiveness for species movement

################################################################################
################## Data Types & Conditionals - III #####################
################################################################################
population_count <- 250
cat("Value:", population_count, "\n")
cat("Type:", typeof(population_count), "\n")

species_name <- "Canis lupus"
cat("Value:", species_name, "\n")
cat("Type:", typeof(species_name), "\n")

is_endangered <- TRUE
cat("Value:", is_endangered, "\n")
cat("Type:", typeof(is_endangered), "\n")

temperature_readings <- c(17.2, 18.5, 19.0)
cat("Value:", temperature_readings, "\n")
cat("Type:", typeof(temperature_readings), "\n")

site_info <- list(site="Delta", lat=44.25, protected=FALSE)
cat("Value:", site_info$site, site_info$lat, site_info$protected, "\n")
cat("Type:", typeof(site_info), "\n")
