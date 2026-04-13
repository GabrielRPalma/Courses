####################################################################################################
###
### File:    04_Data_Structures_&_File_Handling.R
### Purpose: Examples and exercises for Data structures and file handling
### Authors: Gabriel Rodrigues Palma
### Date:    17/06/25
###
####################################################################################################

# load packeges -----
source('00_source.r')

################################################################################
####################### Data Structures & File Handling #####################
################################################################################

# Example 1: Species Occurrence Vector
species_list <- c("Quercus alba", "Acer rubrum", "Pinus strobus", 
                  "Betula papyrifera", "Fagus grandifolia")
abundance_counts <- c(45, 32, 28, 15, 12)
names(abundance_counts) <- species_list
print(abundance_counts)

# Example 2: Ecological Survey Data Frame
survey_data <- data.frame(
  site_id = c("SITE_01", "SITE_02", "SITE_03", "SITE_04", "SITE_05"),
  latitude = c(45.2386, 45.2401, 45.2358, 45.2425, 45.2390),
  longitude = c(-121.7849, -121.7832, -121.7865, -121.7820, -121.7855),
  elevation = c(1250, 1280, 1200, 1320, 1235),
  species_count = c(24, 28, 19, 31, 22),
  canopy_cover = c(85.5, 78.2, 92.1, 71.8, 88.7)
)
head(survey_data)

# Example 3: Nested List for Ecosystem Data
ecosystem_data <- list(
  forest = list(
    dominant_species = c("Oak", "Maple", "Pine"),
    avg_height = 25.5,
    biomass = 350.2
  ),
  grassland = list(
    dominant_species = c("Fescue", "Clover", "Ryegrass"),
    avg_height = 0.8,
    biomass = 12.5
  ),
  wetland = list(
    dominant_species = c("Cattail", "Sedge", "Bulrush"),
    avg_height = 2.1,
    biomass = 85.3
  )
)

# Example 4: Reading CSV Data
# biodiversity_data <- read.csv("biodiversity_survey.csv", 
#                               stringsAsFactors = FALSE)
# # Simulated data structure
biodiversity_data <- data.frame(
  plot_id = paste0("PLOT_", 1:10),
  species_richness = c(15, 18, 12, 22, 16, 19, 14, 21, 17, 20),
  shannon_diversity = c(2.1, 2.4, 1.8, 2.7, 2.2, 2.5, 1.9, 2.6, 2.3, 2.4)
)

# Example 5: Matrix for Species Interaction Data
species_names <- c("Species_A", "Species_B", "Species_C", "Species_D")
interaction_matrix <- matrix(
  c(0, 1, 0, 1,
    1, 0, 1, 0,
    0, 1, 0, 1,
    1, 0, 1, 0),
  nrow = 4, ncol = 4,
  dimnames = list(species_names, species_names)
)
print(interaction_matrix)

################################################################################
################ Data Structures & File Handling Exercises ##################
################################################################################

# Exercise 1: Create a bird migration data frame
# Include species, departure_date, arrival_date, distance_km
# Add at least 5 species with realistic data

# Exercise 2: Build a list structure for climate data
# Include temperature, precipitation, humidity for different seasons
# Organize by season (spring, summer, fall, winter)

# Exercise 3: Create a species abundance matrix
# Rows: different sites, Columns: different species
# Fill with realistic abundance values

# Exercise 4: Design a water quality monitoring data frame
# Include site_name, date, pH, dissolved_oxygen, temperature
# Create data for multiple sampling dates

# Exercise 5: Build a food web adjacency matrix
# Create a 6x6 matrix representing predator-prey relationships
# Use binary values (0 = no interaction, 1 = interaction)

################################################################################
############### Data Structures & File Handling - Advanced ##################
################################################################################

# Example 1: Complex Ecological Survey Data Structure
field_survey <- data.frame(
  transect_id = rep(paste0("T", 1:3), each = 10),
  point_number = rep(1:10, 3),
  species_observed = c("CARO", "PECO", "SCAU", "MIAM", "CARO",
                       "PECO", "SCAU", "MIAM", "CARO", "PECO",
                       "SCAU", "MIAM", "CARO", "PECO", "SCAU",
                       "MIAM", "CARO", "PECO", "SCAU", "MIAM",
                       "CARO", "PECO", "SCAU", "MIAM", "CARO",
                       "PECO", "SCAU", "MIAM", "CARO", "PECO"),
  abundance = c(5, 3, 8, 2, 4, 6, 7, 1, 3, 5,
                4, 6, 2, 8, 3, 5, 7, 4, 6, 2,
                6, 4, 5, 3, 7, 2, 8, 4, 5, 3),
  habitat_type = rep(c("Forest", "Grassland", "Wetland"), each = 10)
)

# Example 2: Writing and Reading Ecological Data
write.csv(field_survey, "field_survey_data.csv", row.names = FALSE)
loaded_data <- read.csv("field_survey_data.csv")

# Example 3: Multi-dimensional Array for Temporal Data
# Array dimensions: sites x species x time_periods
temporal_data <- array(
  data = sample(0:50, 60, replace = TRUE),
  dim = c(5, 4, 3),
  dimnames = list(
    sites = paste0("Site_", 1:5),
    species = c("Species_A", "Species_B", "Species_C", "Species_D"),
    time = c("2022", "2023", "2024")
  )
)

# Example 4: List of Data Frames for Multi-site Study
study_sites <- list(
  alpine = data.frame(
    species = c("Pinus flexilis", "Picea engelmannii", "Abies lasiocarpa"),
    density = c(120, 95, 80),
    dbh_mean = c(25.3, 22.8, 18.5)
  ),
  montane = data.frame(
    species = c("Pseudotsuga menziesii", "Pinus ponderosa", "Quercus gambelii"),
    density = c(200, 150, 180),
    dbh_mean = c(35.2, 42.1, 28.7)
  ),
  riparian = data.frame(
    species = c("Populus tremuloides", "Salix spp.", "Alnus incana"),
    density = c(300, 250, 200),
    dbh_mean = c(18.9, 12.4, 15.6)
  )
)

# Example 5: Data Manipulation and Aggregation
# Calculate summary statistics by habitat type
habitat_summary <- aggregate(abundance ~ habitat_type, 
                             data = field_survey, 
                             FUN = function(x) c(mean = mean(x), 
                                                 sd = sd(x), 
                                                 n = length(x)))
print(habitat_summary)
################################################################################
############# Data Structures & File Handling - Advanced Exercises ###########
################################################################################

# Exercise 1: Create a comprehensive biodiversity database
# Multiple data frames linked by site_id
# Include species_data, environmental_data, and sampling_effort

# Exercise 2: Build a time series data structure
# Monthly wildlife observations over 3 years
# Include seasonal patterns and trend analysis preparation

# Exercise 3: Design a genetic diversity data structure
# Nested lists containing population data, genetic markers
# Include geographic coordinates and sample sizes

# Exercise 4: Create a conservation priority matrix
# Species (rows) by threats (columns)
# Include threat severity scores and conservation actions

# Exercise 5: Build a metapopulation data structure
# Multiple populations with connectivity matrix
# Include population sizes, distances, and migration rates

## Additional exercises
survey_data <- data.frame(
  site = c("Alpha", "Beta", "Gamma"),
  abundance = c(32, 45, 27),
  mean_temp = c(18.2, 19.7, 17.9)
)
abund_sub <- survey_data$abundance[2:3]
row_gamma <- survey_data[survey_data$site == "Gamma", ]
col_temp <- survey_data$mean_temp
cat("Abundance subset:", abund_sub, "\n")
print(row_gamma)
cat("Mean temperature:", col_temp, "\n")

biomass_matrix <- matrix(c(5.7, 8.4, 6.2, 9.1, 7.6, 8.0), nrow = 2, ncol = 3,
                         dimnames = list(c("SiteA", "SiteB"),
                                         c("Species1", "Species2", "Species3")))
row1 <- biomass_matrix[1, ]
col3 <- biomass_matrix[, 3]
submatrix <- biomass_matrix[1:2, 2:3]
cat("Row 1:", row1, "\n")
cat("Col 3:", col3, "\n")
print(submatrix)

count_vector <- c(12, 25, 17, 45, 8, 33)
third_item <- count_vector[3]
select_some <- count_vector[c(2, 4, 6)]
not_first <- count_vector[-1]
greater_than_20 <- count_vector[count_vector > 20]
cat("Third item:", third_item, "\n")
cat("Some elements:", select_some, "\n")
cat("Exclude 1st:", not_first, "\n")
cat(">20:", greater_than_20, "\n")
