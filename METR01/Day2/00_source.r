####################################################################################################
###
### File:    00_source.R
### Purpose: Load required packages and helper functions for
###          Day 2: Time Series Fundamentals I — Ecological Applications
### Authors: Gabriel Rodrigues Palma
### Date:    17/06/25
###
####################################################################################################

# packages required ---------------------------

packages <- c("fpp3", "tidyverse", "patchwork", "RColorBrewer",
              "tsibble", "feasts", "fable", "aimsir17",
              "slider", "seasonal", "forecast")
# install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

# Analysis configuration constants ---------------------------

# plot settings ---------------------------
pallete <- RColorBrewer::brewer.pal(9, "Set1")[ c(3, 1, 9, 6, 8, 5, 2) ]

theme_new <- function(base_size = 15, base_family = "Arial"){
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text = element_text(size = 15, colour = "grey30"),
      legend.key=element_rect(colour=NA, fill =NA),
      axis.line = element_line(colour = 'black'),
      axis.ticks =         element_line(colour = "grey20"),
      plot.title.position = 'plot',
      legend.position = "bottom"
    )
}

# Main functions ---------------------------
print_output <- function(result, label = "Result") {
  # Prints a labelled result to the console
  # Use this to display intermediate results during teaching and exercises.
  #
  # Args:
  #   result : any R object to print
  #   label  : character string used as the section header in the output
  #
  # Returns:
  #   Invisibly returns result (side-effect: console output)
  if (!is.character(label) || length(label) != 1) {
    stop("label must be a single character string")
  }
  cat("\n---", label, "---\n")
  print(result)
  cat("\n")
  invisible(result)
}

load_ecological_tsibble <- function(file_path, index_col, index_type,
                                    key_col = NULL) {
  # Reads a CSV and converts to a tsibble with the
  # specified index type. Centralises the common pattern across all Day 2 scripts.
  #
  # Args:
  #   file_path  : character path to the CSV file
  #   index_col  : character name of the date column to convert and use as index
  #   index_type : character; one of "yearmonth", "yearweek", "yearquarter", "date"
  #   key_col    : character or NULL; column name for the tsibble key variable
  #
  # Returns:
  #   A tsibble object with the specified index and optional key
  if (!file.exists(file_path)) {
    stop(paste("File not found:", file_path))
  }
  if (!index_type %in% c("yearmonth", "yearweek", "yearquarter", "date")) {
    stop("index_type must be one of: yearmonth, yearweek, yearquarter, date")
  }

  raw_data <- read.csv(file_path, stringsAsFactors = FALSE)

  # Apply the correct temporal index conversion
  raw_data <- tryCatch({
    switch(index_type,
           "yearmonth"   = dplyr::mutate(raw_data,
                                         !!index_col := yearmonth(.data[[index_col]])),
           "yearweek"    = dplyr::mutate(raw_data,
                                         !!index_col := yearweek(.data[[index_col]])),
           "yearquarter" = dplyr::mutate(raw_data,
                                         !!index_col := yearquarter(.data[[index_col]])),
           "date"        = dplyr::mutate(raw_data,
                                         !!index_col := as.Date(.data[[index_col]]))
    )
  }, error = function(e) {
    stop(paste("Failed to convert index column:", e$message))
  })

  if (is.null(key_col)) {
    result_ts <- as_tsibble(raw_data, index = !!rlang::sym(index_col))
  } else {
    result_ts <- as_tsibble(raw_data,
                             index = !!rlang::sym(index_col),
                             key   = !!rlang::sym(key_col))
  }

  return(result_ts)
}


calculate_shannon_diversity <- function(species_counts) {
  # Computes Shannon-Wiener diversity index
  # for an ecological community.
  #
  # Args:
  #   species_counts : numeric vector of non-negative abundance counts per species
  #
  # Returns:
  #   A single numeric value (Shannon H'), rounded to 4 decimal places
  if (!is.numeric(species_counts)) {
    stop("species_counts must be a numeric vector")
  }
  if (length(species_counts) == 0) {
    stop("species_counts cannot be empty")
  }
  if (any(species_counts < 0, na.rm = TRUE)) {
    stop("species_counts must contain non-negative values only")
  }

  total_individuals <- sum(species_counts, na.rm = TRUE)
  if (total_individuals == 0) {
    return(0)
  }

  proportions <- species_counts / total_individuals
  # Remove zero proportions to avoid log(0) = -Inf
  proportions <- proportions[proportions > 0]
  shannon_index <- -sum(proportions * log(proportions))

  return(round(shannon_index, 4))
}


summarise_ts_features <- function(ts_data, value_col) {
  # Extracts key statistical features from a tsibble column
  # to help characterise an ecological time series before modelling.
  #
  # Args:
  #   ts_data   : a tsibble object
  #   value_col : character name of the numeric column to summarise
  #
  # Returns:
  #   A named list with mean, sd, min, max, and the result of unitroot_kpss
  if (!is_tsibble(ts_data)) {
    stop("ts_data must be a tsibble object")
  }
  if (!value_col %in% names(ts_data)) {
    stop(paste("Column", value_col, "not found in ts_data"))
  }

  values <- ts_data[[value_col]]

  summary_stats <- list(
    mean   = round(mean(values, na.rm = TRUE), 3),
    sd     = round(sd(values, na.rm = TRUE), 3),
    min    = round(min(values, na.rm = TRUE), 3),
    max    = round(max(values, na.rm = TRUE), 3),
    n_obs  = sum(!is.na(values))
  )

  return(summary_stats)
}


assess_forecast_accuracy <- function(forecast_list, actual_ts,
                                     sort_metric = "RMSE") {
  # Compares multiple forecast objects against actual data
  # and returns an ordered accuracy table.
  #
  # Args:
  #   forecast_list  : named list of fable forecast objects
  #   actual_ts      : tsibble of actual observations (full series)
  #   sort_metric    : character; the accuracy metric to sort by (default "RMSE")
  #
  # Returns:
  #   A data frame of accuracy metrics sorted by sort_metric (ascending)
  if (!is.list(forecast_list) || is.null(names(forecast_list))) {
    stop("forecast_list must be a named list of forecast objects")
  }
  if (!is_tsibble(actual_ts)) {
    stop("actual_ts must be a tsibble object")
  }

  accuracy_results <- tryCatch({
    all_fc <- bind_rows(forecast_list)
    accuracy(all_fc, actual_ts) %>%
      dplyr::select(.model, RMSE, MAE, MAPE, MASE) %>%
      dplyr::arrange(!!rlang::sym(sort_metric))
  }, error = function(e) {
    stop(paste("Failed to compute accuracy:", e$message))
  })

  return(accuracy_results)
}

make_climate_scenario <- function(training_ts, horizon, monthly_temp,
                                  monthly_rain, temp_offset = 0,
                                  rain_scale = 1) {
  # Generates a new_data tsibble for scenario forecasting
  # by offsetting temperature and scaling rainfall relative to a training tsibble.
  #
  # Args:
  #   training_ts   : tsibble used to fit the model (provides the time index base)
  #   horizon       : integer number of future time steps to generate
  #   monthly_temp  : numeric vector of length 12, historical monthly mean temperatures
  #   monthly_rain  : numeric vector of length 12, historical monthly mean rainfall
  #   temp_offset   : numeric value added to monthly_temp (e.g., +2 for warming)
  #   rain_scale    : numeric multiplier applied to monthly_rain (e.g., 0.7 for drought)
  #
  # Returns:
  #   A new_data tsibble with columns temperature_c and rainfall_mm populated
  if (!is_tsibble(training_ts)) {
    stop("training_ts must be a tsibble object")
  }
  if (horizon < 1 || !is.numeric(horizon)) {
    stop("horizon must be a positive integer")
  }
  if (length(monthly_temp) != 12 || length(monthly_rain) != 12) {
    stop("monthly_temp and monthly_rain must each have exactly 12 values")
  }

  n_reps <- ceiling(horizon / 12)
  scenario <- new_data(training_ts, horizon) %>%
    dplyr::mutate(
      temperature_c = rep(monthly_temp, n_reps)[seq_len(horizon)] + temp_offset,
      rainfall_mm   = rep(monthly_rain, n_reps)[seq_len(horizon)] * rain_scale
    )

  return(scenario)
}
