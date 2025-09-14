library(dplyr)
library(stringr)

# standardize the data
standardize_units <- function(value, unit, type) {
  unit <- tolower(unit)
  if (type == "Time") {
    if (unit %in% c("days")) return(value)
    if (unit %in% c("weeks")) return(value * 7)
    if (unit %in% c("months")) return(value * 30)
    if (unit %in% c("years")) return(value * 365)
  }
  if (type == "Weight") {
    if (unit %in% c("grams", "grams (g)")) return(value / 1000)
    if (unit %in% c("kilograms", "kilograms (kg)")) return(value)
  }
  if (type == "Temp") {
    if (unit %in% c("celsius", "celcius (c)", "celsius", "celcius (c)")) return(value)
    if (unit %in% c("fahrenheit", "fahrenheit (f)")) return((value - 32) * 5/9)
  }
  return(NA)
}


convert_units <- function(data, labels) {
  
  # get the rows that need unit conversion
  unit_rows <- which(grepl("^Single select, unit", labels$`Option Type`))
  
  # Build the mapping by pairing each with the previous row
  value_unit_map <- data.frame(
    value_field = labels$`Simple Labels`[unit_rows - 1],
    unit_field  = labels$`Simple Labels`[unit_rows],
    unit_type   = labels$`Option Type`[unit_rows - 1]
  )
  
  # Loop over each value-unit pair and standardize data
  for (i in seq_len(nrow(value_unit_map))) {
    val_col <- value_unit_map$value_field[i]
    unit_col <- value_unit_map$unit_field[i]
    unit_type <- value_unit_map$unit_type[i]
    
    # Skip if value or unit field is missing from the data TODO can remove
    if (!(val_col %in% names(data)) | !(unit_col %in% names(data))) next
    
    # Build new column name based on type
    suffix <- switch(unit_type,
                     "Time" = "_days",
                     "Weight" = "_kg",
                     "Temp" = "_c")
    
    new_col <- paste0(val_col, suffix)
    
    # overwrite original column with new column
    data[[val_col]] <- mapply(standardize_units,
                              value = data[[val_col]],
                              unit = data[[unit_col]],
                              MoreArgs = list(type = gsub("Single select, unit ", "", unit_type)))
    
    # Rename the value column and remove the old
    names(data)[names(data) == val_col] <- new_col
    data[[unit_col]] <- NULL
    
    # Update cleaned labels sheet too
    row_to_keep <- which(labels$`Simple Labels` == val_col)[1]
    labels$`Simple Labels`[row_to_keep] <- new_col
    labels$`Option Type`[row_to_keep] <- paste0("Converted, ", gsub("Single select, unit ", "", unit_type))
    # Leave Variable Type untouched or set manually if needed
    labels <- labels[-which(labels$`Simple Labels` == unit_col), ]
  }
  
  return(list(data = data, labels = labels))
}