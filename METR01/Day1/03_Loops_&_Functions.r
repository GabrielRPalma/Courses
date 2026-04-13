####################################################################################################
###
### File:    03_Loops_&_Functions.R
### Purpose: Examples and exercises for Loops and functions
### Authors: Gabriel Rodrigues Palma
### Date:    17/06/25
###
####################################################################################################

# load packeges -----
source('00_source.r')

################################################################################
############################# Loops & Functions ##############################
################################################################################

# Example 1: Population Growth Simulation
initial_population <- 100
growth_rate <- 0.05
years <- 10
population <- initial_population

for (year in 1:years) {
  population <- population * (1 + growth_rate)
  cat("Year", year, ": Population =", round(population, 0), "\n")
}

# Example 2: Species Diversity Calculation Function
calculate_shannon_diversity <- function(species_counts) {
  total_individuals <- sum(species_counts)
  proportions <- species_counts / total_individuals
  shannon_index <- -sum(proportions * log(proportions))
  return(shannon_index)
}

# Usage
species_data <- c(45, 32, 28, 15, 12, 8, 5)
diversity <- calculate_shannon_diversity(species_data)
cat("Shannon Diversity Index:", round(diversity, 3), "\n")

# Example 3: Temperature Data Processing Loop
daily_temps <- c(18.5, 19.2, 17.8, 20.1, 21.5, 19.9, 18.7)
temp_categories <- character(length(daily_temps))

for (i in 1:length(daily_temps)) {
  if (daily_temps[i] < 18) {
    temp_categories[i] <- "Cool"
  } else if (daily_temps[i] < 21) {
    temp_categories[i] <- "Moderate"
  } else {
    temp_categories[i] <- "Warm"
  }
}

# Example 4: Pollination Efficiency Function
pollination_efficiency <- function(visits_per_flower, pollen_transfer_rate) {
  efficiency <- visits_per_flower * pollen_transfer_rate * 100
  if (efficiency > 80) {
    return(list(efficiency = efficiency, category = "High"))
  } else if (efficiency > 50) {
    return(list(efficiency = efficiency, category = "Medium"))
  } else {
    return(list(efficiency = efficiency, category = "Low"))
  }
}

# Example 5: Biomass Accumulation Over Time
calculate_biomass_growth <- function(initial_biomass, growth_rates) {
  biomass_values <- numeric(length(growth_rates) + 1)
  biomass_values[1] <- initial_biomass
  
  for (i in 1:length(growth_rates)) {
    biomass_values[i + 1] <- biomass_values[i] * (1 + growth_rates[i])
  }
  return(biomass_values)
}

################################################################################
######################## Loops & Functions Exercises #########################
################################################################################

# Exercise 1: Carbon sequestration calculation
# Write a function to calculate annual carbon sequestration
# Use tree age, species type, and environmental conditions

# Exercise 2: Migration distance tracker
# Create a loop to track cumulative migration distance
# Use daily movement data and calculate total journey

# Exercise 3: Predator-prey dynamics simulation
# Write functions for predator and prey population changes
# Use Lotka-Volterra equations simplified version

# Exercise 4: Seed dispersal probability
# Create a function to calculate dispersal success
# Consider distance, wind speed, and seed characteristics

# Exercise 5: Water quality monitoring loop
# Process multiple water samples with quality assessments
# Use pH, temperature, and dissolved oxygen measurements

################################################################################
####################### Loops & Functions - Advanced ########################
################################################################################

# Example 1: Advanced Population Dynamics with Carrying Capacity
logistic_growth <- function(N0, r, K, time_steps) {
  population <- numeric(time_steps + 1)
  population[1] <- N0
  
  for (t in 1:time_steps) {
    population[t + 1] <- population[t] + r * population[t] * 
      (1 - population[t] / K)
  }
  return(population)
}

# Usage
deer_population <- logistic_growth(N0 = 50, r = 0.1, K = 500, time_steps = 100)

# Example 2: Biodiversity Metrics Suite
biodiversity_metrics <- function(species_abundances) {
  
  total_individuals <- sum(species_abundances)
  species_richness <- length(species_abundances)
  
  # Shannon diversity
  proportions <- species_abundances / total_individuals
  shannon <- -sum(proportions * log(proportions))
  
  # Simpson diversity
  simpson <- 1 - sum(proportions^2)
  
  # Evenness
  evenness <- shannon / log(species_richness)
  
  return(list(
    richness = species_richness,
    shannon = shannon,
    simpson = simpson,
    evenness = evenness
  ))
}

# Example 3: Climate Change Impact Assessment
assess_climate_impact <- function(species_data, temp_change, precip_change) {
  impact_scores <- numeric(length(species_data))
  
  for (i in 1:length(species_data)) {
    temp_tolerance <- species_data[[i]]$temp_tolerance
    precip_tolerance <- species_data[[i]]$precip_tolerance
    
    temp_impact <- abs(temp_change) / temp_tolerance
    precip_impact <- abs(precip_change) / precip_tolerance
    
    impact_scores[i] <- (temp_impact + precip_impact) / 2
  }
  
  return(impact_scores)
}

# Example 4: Metapopulation Dynamics
metapopulation_simulation <- function(patches, migration_rate, 
                                      extinction_rate, colonization_rate) {
  occupied <- patches
  time_series <- list()
  
  for (generation in 1:50) {
    # Extinction events
    for (i in 1:length(occupied)) {
      if (occupied[i] && runif(1) < extinction_rate) {
        occupied[i] <- FALSE
      }
    }
    
    # Colonization events
    for (i in 1:length(occupied)) {
      if (!occupied[i] && runif(1) < colonization_rate) {
        occupied[i] <- TRUE
      }
    }
    
    time_series[[generation]] <- sum(occupied)
  }
  
  return(time_series)
}

# Example 5: Ecological Network Analysis
calculate_network_metrics <- function(interaction_matrix) {
  species_count <- nrow(interaction_matrix)
  connectance <- sum(interaction_matrix > 0) / (species_count^2)
  
  # Calculate degree for each species
  degrees <- numeric(species_count)
  for (i in 1:species_count) {
    degrees[i] <- sum(interaction_matrix[i, ] > 0) + 
      sum(interaction_matrix[, i] > 0)
  }
  
  return(list(
    connectance = connectance,
    mean_degree = mean(degrees),
    max_degree = max(degrees)
  ))
}
################################################################################
#################### Loops & Functions - Advanced Exercises ##################
################################################################################

# Exercise 1: Species area relationship modeling
# Create a function implementing the species-area curve
# S = c * A^z, where S is species number, A is area

# Exercise 2: Trophic cascade simulation
# Write functions to model cascading effects through food web
# Include top predator, mesopredator, and prey levels

# Exercise 3: Habitat fragmentation analysis
# Create a function to assess fragmentation effects
# Consider patch size, edge effects, and connectivity

# Exercise 4: Pollinator network robustness
# Write a function to test network stability
# Simulate species removals and measure network collapse

# Exercise 5: Fire spread modeling
# Create a cellular automaton model for fire spread
# Include wind direction, fuel load, and moisture content

my_function <- function(argument){
  # This function do the following ...
  
  #Input:
  #     first_argument (Type): Description of the argument
  #Output:
  #     my_result (Type): Description of the returned object
  
  
}

