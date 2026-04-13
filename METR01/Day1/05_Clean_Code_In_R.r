####################################################################################################
###
### File:    05_Clean_Code_In_R.R
### Purpose: Examples and exercises for Clearn code in R
### Authors: Gabriel Rodrigues Palma
### Date:    17/06/25

###
####################################################################################################

# load packeges -----
source('00_source.r')

################################################################################
############################### Clean Code in R ##############################
################################################################################

# Example 1: Clear Variable Names and Documentation
# Bad example:
# x <- 25
# y <- 0.15
# z <- x * y

# Good example:
tree_diameter_cm <- 25
allometric_coefficient <- 0.15
estimated_biomass_kg <- tree_diameter_cm * allometric_coefficient

# Document the calculation
cat("Estimated biomass for", tree_diameter_cm, "cm diameter tree:", 
    estimated_biomass_kg, "kg\n")

# Example 2: Well-structured Function with Clear Purpose
calculate_species_evenness <- function(species_abundances, 
                                       method = "pielou") {
  # Calculate species evenness using Pielou's evenness index
  # 
  # Args:
  #   species_abundances: numeric vector of species counts
  #   method: character, evenness calculation method
  # 
  # Returns:
  #   numeric value of evenness index (0-1)
  
  if (length(species_abundances) == 0) {
    stop("Species abundances vector cannot be empty")
  }
  
  species_richness <- length(species_abundances)
  total_individuals <- sum(species_abundances)
  proportions <- species_abundances / total_individuals
  
  shannon_diversity <- -sum(proportions * log(proportions))
  max_diversity <- log(species_richness)
  
  evenness <- shannon_diversity / max_diversity
  
  return(round(evenness, 4))
}

# Example 3: Consistent Code Structure and Comments
# Field data processing pipeline
process_field_data <- function(raw_data_path) {
  # Step 1: Load and validate data
  field_data <- read.csv(raw_data_path, stringsAsFactors = FALSE)
  
  # Check for required columns
  required_cols <- c("site_id", "species", "abundance", "date")
  missing_cols <- setdiff(required_cols, names(field_data))
  
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", 
               paste(missing_cols, collapse = ", ")))
  }
  
  # Step 2: Clean and standardize data
  field_data$species <- toupper(field_data$species)
  field_data$date <- as.Date(field_data$date)
  
  # Step 3: Remove invalid records
  valid_data <- field_data[field_data$abundance > 0 & 
                             !is.na(field_data$abundance), ]
  
  return(valid_data)
}

# Example 4: Meaningful Constants and Configuration
# Define ecological constants
SHANNON_BASE <- exp(1)  # Natural logarithm base for Shannon index
MIN_VIABLE_POPULATION <- 50  # Minimum population for viability
CRITICAL_HABITAT_THRESHOLD <- 0.3  # Proportion of habitat remaining

# Conservation status assessment
assess_conservation_status <- function(population_size, habitat_remaining) {
  if (population_size < MIN_VIABLE_POPULATION) {
    status <- "Critical"
  } else if (habitat_remaining < CRITICAL_HABITAT_THRESHOLD) {
    status <- "Threatened"
  } else {
    status <- "Stable"
  }
  
  return(status)
}

# Example 5: Error Handling and Input Validation
calculate_biodiversity_index <- function(community_matrix, 
                                         index_type = "shannon") {
  # Validate inputs
  if (!is.matrix(community_matrix) && !is.data.frame(community_matrix)) {
    stop("community_matrix must be a matrix or data frame")
  }
  
  if (!index_type %in% c("shannon", "simpson", "richness")) {
    stop("index_type must be 'shannon', 'simpson', or 'richness'")
  }
  
  # Convert to matrix if data frame
  if (is.data.frame(community_matrix)) {
    community_matrix <- as.matrix(community_matrix)
  }
  
  # Check for negative values
  if (any(community_matrix < 0, na.rm = TRUE)) {
    warning("Negative values found in community matrix")
  }
  
  # Calculate requested index
  switch(index_type,
         "richness" = apply(community_matrix > 0, 1, sum),
         "shannon" = apply(community_matrix, 1, function(x) {
           x <- x[x > 0]
           -sum((x/sum(x)) * log(x/sum(x)))
         }),
         "simpson" = apply(community_matrix, 1, function(x) {
           x <- x[x > 0]
           1 - sum((x/sum(x))^2)
         })
  )
}

################################################################################
########################## Clean Code in R Exercises #########################
################################################################################

# Exercise 1: Refactor poorly written code
# Given messy code for population growth calculation
# Improve variable names, add comments, and structure properly

# Exercise 2: Create a well-documented function
# Write a function to calculate carrying capacity
# Include proper documentation, error handling, and examples

# Exercise 3: Implement consistent coding style
# Take a working but messy ecological analysis script
# Apply consistent indentation, spacing, and naming conventions

# Exercise 4: Add proper error handling
# Create a function for habitat suitability modeling
# Include input validation and meaningful error messages

# Exercise 5: Organize code into logical modules
# Structure a complete ecological analysis workflow
# Separate data loading, processing, analysis, and visualization

################################################################################
######################### Clean Code in R - Advanced #########################
################################################################################

# Example 1: Modular Code Organization
# File: ecological_functions.R

# Species diversity calculations module
diversity_functions <- list(
  
  shannon = function(abundances) {
    proportions <- abundances / sum(abundances)
    -sum(proportions * log(proportions), na.rm = TRUE)
  },
  
  simpson = function(abundances) {
    proportions <- abundances / sum(abundances)
    1 - sum(proportions^2, na.rm = TRUE)
  },
  
  pielou_evenness = function(abundances) {
    shannon_div <- diversity_functions$shannon(abundances)
    max_div <- log(length(abundances))
    shannon_div / max_div
  }
)

# Example 2: Configuration Management
# Configuration for ecological analysis
ecological_config <- list(
  # Data parameters
  data_path = "data/",
  output_path = "results/",
  
  # Analysis parameters
  min_abundance_threshold = 5,
  rare_species_cutoff = 0.01,
  bootstrap_iterations = 1000,
  
  # Visualization parameters
  plot_width = 10,
  plot_height = 8,
  figure_dpi = 300,
  
  # Statistical parameters
  alpha_level = 0.05,
  confidence_level = 0.95
)

# Example 3: Comprehensive Error Handling and Logging
analyze_community_structure <- function(community_data, 
                                        config = ecological_config) {
  # Initialize analysis log
  analysis_log <- list()
  
  tryCatch({
    # Validate input data
    if (nrow(community_data) == 0) {
      stop("Community data is empty")
    }
    
    analysis_log$start_time <- Sys.time()
    analysis_log$n_sites <- nrow(community_data)
    analysis_log$n_species <- ncol(community_data)
    
    # Filter rare species
    species_totals <- colSums(community_data)
    common_species <- species_totals >= config$min_abundance_threshold
    
    if (sum(common_species) == 0) {
      stop("No species meet minimum abundance threshold")
    }
    
    filtered_data <- community_data[, common_species]
    analysis_log$n_species_filtered <- ncol(filtered_data)
    
    # Calculate diversity indices
    diversity_results <- data.frame(
      site = rownames(filtered_data),
      shannon = apply(filtered_data, 1, diversity_functions$shannon),
      simpson = apply(filtered_data, 1, diversity_functions$simpson),
      evenness = apply(filtered_data, 1, diversity_functions$pielou_evenness)
    )
    
    analysis_log$end_time <- Sys.time()
    analysis_log$duration <- analysis_log$end_time - analysis_log$start_time
    
    return(list(
      results = diversity_results,
      log = analysis_log,
      config_used = config
    ))
    
  }, error = function(e) {
    analysis_log$error <- e$message
    analysis_log$error_time <- Sys.time()
    
    return(list(
      results = NULL,
      log = analysis_log,
      error = e$message
    ))
  })
}

# Example 4: Unit Testing Framework
# Test functions for ecological calculations
test_diversity_functions <- function() {
  cat("Testing diversity functions...\n")
  
  # Test data
  test_abundances <- c(10, 20, 30, 40)
  
  # Test Shannon diversity
  shannon_result <- diversity_functions$shannon(test_abundances)
  expected_shannon <- 1.2799  # Pre-calculated expected value
  
  if (abs(shannon_result - expected_shannon) < 0.001) {
    cat("✓ Shannon diversity test passed\n")
  } else {
    cat("✗ Shannon diversity test failed\n")
  }
  
  # Test Simpson diversity
  simpson_result <- diversity_functions$simpson(test_abundances)
  expected_simpson <- 0.74  # Pre-calculated expected value
  
  if (abs(simpson_result - expected_simpson) < 0.01) {
    cat("✓ Simpson diversity test passed\n")
  } else {
    cat("✗ Simpson diversity test failed\n")
  }
  
  # Test edge cases
  tryCatch({
    diversity_functions$shannon(c())
    cat("✗ Empty vector test failed - should have thrown error\n")
  }, error = function(e) {
    cat("✓ Empty vector test passed - error properly handled\n")
  })
}

# Example 5: Performance Optimization and Profiling
# Optimized function for large datasets
calculate_diversity_optimized <- function(community_matrix) {
  # Pre-allocate results vector
  n_sites <- nrow(community_matrix)
  shannon_values <- numeric(n_sites)
  
  # Vectorized calculation where possible
  row_sums <- rowSums(community_matrix)
  
  for (i in seq_len(n_sites)) {
    site_abundances <- community_matrix[i, ]
    site_abundances <- site_abundances[site_abundances > 0]
    
    if (length(site_abundances) > 0) {
      proportions <- site_abundances / row_sums[i]
      shannon_values[i] <- -sum(proportions * log(proportions))
    } else {
      shannon_values[i] <- 0
    }
  }
  
  return(shannon_values)
}

# Performance comparison function
compare_diversity_performance <- function(community_data, iterations = 10) {
  cat("Comparing diversity calculation performance...\n")
  
  # Time the standard function
  start_time <- Sys.time()
  for (i in 1:iterations) {
    result1 <- apply(community_data, 1, diversity_functions$shannon)
  }
  standard_time <- Sys.time() - start_time
  
  # Time the optimized function
  start_time <- Sys.time()
  for (i in 1:iterations) {
    result2 <- calculate_diversity_optimized(community_data)
  }
  optimized_time <- Sys.time() - start_time
  
  cat("Standard function time:", standard_time, "\n")
  cat("Optimized function time:", optimized_time, "\n")
  cat("Speedup factor:", as.numeric(standard_time / optimized_time), "\n")
}
################################################################################
###################### Clean Code in R - Advanced Exercises ##################
################################################################################

# Exercise 1: Create a comprehensive testing suite
# Develop unit tests for all ecological functions
# Include edge cases, error conditions, and performance tests

# Exercise 2: Implement a logging system
# Create a logging framework for ecological analyses
# Include different log levels (info, warning, error)

# Exercise 3: Design a configuration system
# Create a flexible configuration system for ecological studies
# Allow for different study types and parameter sets

# Exercise 4: Optimize code for large datasets
# Profile and optimize functions for big ecological datasets
# Focus on memory efficiency and computational speed

# Exercise 5: Create a complete analysis pipeline
# Build a full ecological analysis workflow
# Include data validation, processing, analysis, and reporting

