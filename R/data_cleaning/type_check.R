# ------
# Check that each column has the correct type
# ------
check_types <- function(df, type_map) {
  mismatches <- sapply(names(type_map), function(col) {
    if (col %in% names(df)) {
      actual <- class(df[[col]])[1]
      expected <- switch(type_map[[col]],
                         "Date" = "Date",
                         "POSIXct" = "POSIXct",
                         "Integer" = "integer",
                         "Numeric" = "numeric",
                         "Factor" = "factor",
                         "Blank" = "character",  # left as character until dropped
                         "character")
      return(actual != expected)
    } else {
      return(NA)  # Column not found
    }
  })
  mismatches[mismatches == TRUE]
}
# Confirm for both admission and discharge
check_types(discharge_clean, discharge_type_map)
check_types(admission_clean, admission_type_map)

#-----
# Check factor levels for one example column (if you have known levels)
#------
check_factor_levels <- function(df, colname) {
  if (colname %in% names(df) && is.factor(df[[colname]])) {
    levels(df[[colname]])
  } else {
    NULL
  }
}
#
check_factor_levels(admission_clean, "dx_1")
