library(stringr)

clean_issues <- function(df, labels) {
  for (i in seq_len(nrow(labels))) {
    col <- labels$`Simple Labels`[i]
    
    # skip if column not in data
    if (!(col %in% names(df))) next
    
    # get flags
    flag_string <- labels$`Flag Issues`[i]
    specific_flags <- labels$`Specific Flags`[i]
    
    # Parse thresholds
    min_val <- as.numeric(str_match(flag_string, "min=([0-9\\.]+)")[,2])
    max_val <- as.numeric(str_match(flag_string, "max=([0-9\\.]+)")[,2])
    
    min_len <- as.numeric(str_match(flag_string, "minlength=([0-9]+)")[,2])
    max_len <- as.numeric(str_match(flag_string, "maxlength=([0-9]+)")[,2])
    
    remove_cond <- str_match(flag_string, "remove=([a-zA-Z]+)")[,2]
    
    mindate_str <- str_match(flag_string, "mindate=([0-9\\-]+)")[,2]
    maxdate_str <- str_match(flag_string, "maxdate=([a-zA-Z0-9\\-]+)")[,2]
    mindate <- if (!is.na(mindate_str) && nzchar(mindate_str)) as.Date(mindate_str) else NA
    maxdate <- if (!is.na(maxdate_str) && tolower(maxdate_str) == "today") Sys.Date() else if (!is.na(maxdate_str) && nzchar(maxdate_str)) as.Date(maxdate_str) else NA
    
    # Clean: min/max numeric checks
    vals_num <- suppressWarnings(as.numeric(df[[col]]))
    if (!all(is.na(vals_num))) {
      if (!is.na(min_val)) df[[col]][!is.na(vals_num) & vals_num < min_val] <- NA
      if (!is.na(max_val)) df[[col]][!is.na(vals_num) & vals_num > max_val] <- NA
    }
    # Clean: character length checks
    vals_chr <- as.character(df[[col]])
    if (!is.na(min_len)) df[[col]][!is.na(vals_chr) & nchar(vals_chr) < min_len] <- NA
    if (!is.na(max_len)) df[[col]][!is.na(vals_chr) & nchar(vals_chr) > max_len] <- NA
    
    # Clean: date checks
    vals_date <- suppressWarnings(parse_date_time(df[[col]], orders = c("ymd", "Ymd HMS", "Ymd HM", "dmy", "mdy")))
    vals_date <- as.Date(vals_date)
    if (!is.na(mindate)) df[[col]][!is.na(vals_date) & vals_date < mindate] <- NA
    if (!is.na(maxdate)) df[[col]][!is.na(vals_date) & vals_date > maxdate] <- NA
    
    # Clean: remove by condition
    if (!is.na(remove_cond)) {
      if (remove_cond == "containsNumber") df[[col]][str_detect(vals_chr, "\\d")] <- NA
      if (remove_cond == "containsLetter") df[[col]][str_detect(vals_chr, "[A-Za-z]")] <- NA
      if (remove_cond == "noNumber") df[[col]][!str_detect(vals_chr, "\\d")] <- NA
      if (remove_cond == "noLetter") df[[col]][!str_detect(vals_chr, "[A-Za-z]")] <- NA
    }
    
    # Clean: specific flags for removal
    if (!is.na(specific_flags) && nzchar(specific_flags)) {
      flags <- str_split(specific_flags, ",\\s*")[[1]]
      df[[col]][vals_chr %in% flags] <- NA
    }
  }
  return(df)
}

# Also a verbose version if want to track changes
clean_issues_verbose <- function(data, labels) {
  
  # Extract flag and specific flag maps
  flag_map <- setNames(labels$`Flag Issues`, labels$`Simple Labels`)
  specific_map <- setNames(labels$`Specific Flags`, labels$`Simple Labels`)
  
  # start log of cleaning changes
  cleaning_log <- list()
  
  for (col in names(data)) {
    
    # Skip columns with no flags at all
    flag_string <- flag_map[[col]]
    specific_string <- specific_map[[col]]
    original_vals <- data[[col]]
    
    if ((is.na(flag_string) || flag_string == "") && 
        (is.na(specific_string) || specific_string == "")) {
      next
    }
    
    issue_flags <- rep(FALSE, length(original_vals))  # logical vector
    
    # 1. Specific Flags removal
    if (!is.na(specific_string) && nzchar(specific_string)) {
      bad_values <- str_split(specific_string, ",\\s*")[[1]]
      issue_flags <- issue_flags | original_vals %in% bad_values
    }
    
    # 2. Flag Issues checks (if any)
    if (!is.na(flag_string) && nzchar(flag_string)) {
      
      # Parse thresholds
      min_val <- as.numeric(str_match(flag_string, "min=([0-9\\.]+)")[,2])
      max_val <- as.numeric(str_match(flag_string, "max=([0-9\\.]+)")[,2])
      
      min_len <- as.numeric(str_match(flag_string, "minlength=([0-9]+)")[,2])
      max_len <- as.numeric(str_match(flag_string, "maxlength=([0-9]+)")[,2])
      
      remove_cond <- str_match(flag_string, "remove=([a-zA-Z]+)")[,2]
      
      mindate_str <- str_match(flag_string, "mindate=([0-9\\-]+)")[,2]
      maxdate_str <- str_match(flag_string, "maxdate=([a-zA-Z0-9\\-]+)")[,2]

      mindate <- if (!is.na(mindate_str) && nzchar(mindate_str)) as.Date(mindate_str) else NA
      maxdate <- if (!is.na(maxdate_str) && tolower(maxdate_str) == "today") Sys.Date() else if (!is.na(maxdate_str) && nzchar(maxdate_str)) as.Date(maxdate_str) else NA
      
      # ------- checks ------ (add here if want to add more)
      # Numeric checks
      suppressWarnings({
        numeric_vals <- as.numeric(original_vals)
      })
      issue_flags <- issue_flags | (!is.na(min_val) & numeric_vals < min_val)
      issue_flags <- issue_flags | (!is.na(max_val) & numeric_vals > max_val)
      
      # Date checks
      vals_date <- suppressWarnings(parse_date_time(original_vals, orders = c("ymd", "Ymd HMS", "Ymd HM", "dmy", "mdy")))
      vals_date <- as.Date(vals_date)
      issue_flags <- issue_flags | (!is.na(mindate) & vals_date < mindate)
      issue_flags <- issue_flags | (!is.na(maxdate) & vals_date > maxdate)
      
      # Length checks
      str_lens <- nchar(original_vals)
      issue_flags <- issue_flags | (!is.na(min_len) & str_lens < min_len)
      issue_flags <- issue_flags | (!is.na(max_len) & str_lens > max_len)
      
      # Remove condition checks
      if (!is.na(remove_cond)) {
        cond <- tolower(remove_cond)
        if (cond == "containsnumber") {
          issue_flags <- issue_flags | str_detect(original_vals, "\\d")
        } 
        if (cond == "containsletter") {
          issue_flags <- issue_flags | str_detect(original_vals, "[A-Za-z]")
        }
        if (cond == "nonumber") {
          issue_flags <- issue_flags | !str_detect(original_vals, "\\d")
        }
        if (cond == "noletter") {
          issue_flags <- issue_flags | !str_detect(original_vals, "[A-Za-z]")
        }
      }
    }
    
    # Apply cleaning: set flagged values to NA
    cleaned_vals <- original_vals
    cleaned_vals[issue_flags] <- NA
    data[[col]] <- cleaned_vals
    
    # Log summary
    n_removed <- sum(issue_flags, na.rm = TRUE)
    if (n_removed > 0) {
      cleaning_log[[col]] <- list(
        removed_n = n_removed,
        total_n = length(original_vals),
        flagged_vals <- unique(na.omit(original_vals[issue_flags])),
        flagged_examples = head(flagged_vals, 5),
        rules_applied = c(flag_string, specific_string)
      )
      message(sprintf("Cleaned %d/%d entries in column '%s'. Example bad values: %s",
                      n_removed, length(original_vals), col,
                      paste(flagged_vals[1:min(3, length(flagged_vals))], collapse = ", ")))
    }
  }
  return(data)
}