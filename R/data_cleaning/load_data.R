library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)

# Load each column as the specified type
coerce_types <- function(df, type_map, options_map = NULL) {
  for (col in names(df)) {
    if (col %in% names(type_map)) {
      target_type <- type_map[[col]]
      options_str <- if (!is.null(options_map)) options_map[[col]] else NA
      factor_levels <- if (!is.na(options_str) && nzchar(options_str)) str_split(options_str, ",\\s*")[[1]] else NULL
      
      df[[col]] <- switch(target_type,
                          "Date"    = as.Date(df[[col]]),
                          "POSIXct" = ymd_hms(df[[col]], tz = "UTC"),
                          "Integer" = as.integer(as.numeric(df[[col]])),
                          "Numeric" = as.numeric(df[[col]]),
                          "Factor"  = if (!is.null(factor_levels)) factor(df[[col]], levels = factor_levels) else as.factor(df[[col]]),
                          "Blank"   = df[[col]],
                          as.character(df[[col]])
      )
    }
  }
  return(df)
}

load_and_prepare_data <- function(data_path, label_path) {
  
  # Load data, start with all columns as Character type
  raw_data <- read_csv2(data_path, col_types = cols(.default = col_character()))
  labels <- read_excel(label_path, skip = 1) # ignores the first row, which holds comments
  
  # Remap labels
  rename_map <- as.list(labels$`Labels`)
  names(rename_map) <- labels$`Simple Labels`
  raw_data <- raw_data %>% rename(!!!rename_map)
  
  # Make sure each column is correct type (as specified in label overview sheets)
  type_map <- labels$`Variable Type`
  names(type_map) <- labels$`Simple Labels`
  
  # Build factor options maps (as specified in label overview sheets)
  options_map <- labels$`Options`
  names(options_map) <- labels$`Simple Labels`
  
  # Apply correct types and factor values to data
  clean_data <- coerce_types(raw_data, type_map, options_map)
  clean_data <- clean_data[, names(clean_data)[type_map[names(clean_data)] != "Blank"] ]
  labels_cleaned <- labels %>% filter(`Variable Type` != "Blank")
  
  return(list(data = clean_data, labels = labels_cleaned))
}
