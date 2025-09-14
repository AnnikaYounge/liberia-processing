# ----- UNMATCHED DATA PROCESSING ----- 
# we have now dealt with matching for the unique IDs, but we now have to deal with duplicates.
# That now includes duplicates because of form resubmission, bad data entry, twins, readmissions over time, andtransfers

# ----- helper functions----
`%||%` <- function(a,b) if (!is.null(a)) a else b

first_non_missing <- function(v) {
  if (is.list(v)) return(v[1])
  if (is.character(v)) {
    idx <- which(!is.na(v) & v != "")[1]
  } else {
    idx <- which(!is.na(v))[1]
  }
  if (is.na(idx)) return(v[1]) else return(v[[idx]])
}

safe_min_date <- function(x) {
  if (all(is.na(x))) as.Date(NA) else min(x, na.rm = TRUE)
}

stop_if_missing <- function(df, cols, nm) {
  miss <- setdiff(cols, names(df)); if (length(miss)) stop(sprintf("Missing in %s: %s", nm, paste(miss, collapse=", ")))
}

# Readmissions
# --- We check if there is a gap in time between the listed admission dates for the entries ---- 
add_readmit_seq <- function(adm_df, readmit_gap_days = 15L, transfer_gap_max = 2L) {
  stop_if_missing(adm_df, c("patient_id","ad_date","ad_location"), "add_readmit_seq_guard")
  adm_df %>%
    arrange(patient_id, ad_date) %>%
    group_by(patient_id) %>%
    mutate(
      prev_date = lag(ad_date),
      prev_loc  = lag(ad_location),
      gap_days  = as.integer(ad_date - prev_date),
      looks_like_transfer = !is.na(prev_date) & gap_days > 0 & gap_days <= transfer_gap_max & !is.na(prev_loc) & prev_loc != ad_location,
      new_episode = case_when(
        is.na(prev_date) ~ 1L,
        looks_like_transfer ~ 0L, # so we exclude transfers on the same day
        gap_days > readmit_gap_days ~ 1L, 
        TRUE ~ 0L
      ),
      readmit_seq = cumsum(coalesce(new_episode, 0L)) + 1L
    ) %>%
    ungroup()
}

add_readmit_seq_generic <- function(df, id_col="patient_id", date_col="ad_date",
                                    loc_col="discharge_location",
                                    readmit_gap_days=15L, transfer_gap_max=2L) {
  stop_if_missing(df, c(id_col, date_col, loc_col), "add_readmit_seq_generic")
  id <- rlang::sym(id_col); dt <- rlang::sym(date_col); lc <- rlang::sym(loc_col)
  
  df %>%
    dplyr::arrange(!!id, !!dt) %>%
    dplyr::group_by(!!id) %>%
    dplyr::mutate(
      prev_date = dplyr::lag(!!dt),
      prev_loc  = dplyr::lag(!!lc),
      gap_days  = as.integer(!!dt - prev_date),
      looks_like_transfer = !is.na(prev_date) & gap_days > 0 & gap_days <= transfer_gap_max &
        !is.na(prev_loc)  & prev_loc != !!lc,
      new_episode = dplyr::case_when(
        is.na(prev_date) ~ 1L,
        looks_like_transfer ~ 0L,
        gap_days > readmit_gap_days ~ 1L,
        TRUE ~ 0L
      ),
      readmit_seq = cumsum(coalesce(new_episode, 0L)) + 1L
    ) %>%
    dplyr::ungroup()
}

# Twins
# we now deal with twins: we start by splitting IDs using roman numerals / mixed sex
split_twins_consistent <- function(df) {
  stop_if_missing(df,
                  c("patient_id","patient_birthdate","patient_name_first",
                    "patient_name_last","patient_sex","ad_date"),
                  "split_twins_consistent")
  roman_re <- "\\b(I|II|III|IV|V|1|2|3)\\b"
  
  # flag twin-ish id+dob groups
  twinish <- df %>%
    dplyr::group_by(patient_id, patient_birthdate) %>%
    dplyr::summarise(
      mixed_sex = dplyr::n_distinct(patient_sex, na.rm = TRUE) > 1,
      has_roman = any(stringr::str_detect(patient_name_first %||% "", regex(roman_re, TRUE)), na.rm = TRUE) |
        any(stringr::str_detect(patient_name_last  %||% "", regex(roman_re, TRUE)),  na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::filter(mixed_sex | has_roman)
  
  # stable ordering by (last, first, sex)
  ordering <- df %>%
    dplyr::semi_join(twinish, by = c("patient_id","patient_birthdate")) %>%
    dplyr::distinct(patient_id, patient_birthdate,
                    patient_name_last, patient_name_first, patient_sex) %>%
    dplyr::arrange(patient_id, patient_birthdate,
                   patient_name_last, patient_name_first, patient_sex) %>%
    dplyr::group_by(patient_id, patient_birthdate) %>%
    dplyr::mutate(twin_number = if (dplyr::n() > 1L) dplyr::row_number() else NA_integer_) %>%
    dplyr::ungroup()
  
  df %>%
    dplyr::left_join(
      ordering,
      by = c("patient_id","patient_birthdate","patient_name_last","patient_name_first","patient_sex")
    ) %>%
    dplyr::mutate(
      patient_id_twin = dplyr::if_else(!is.na(twin_number),
                                       paste0(patient_id, "_T", twin_number),
                                       patient_id)
    )
}

# Transfers (same patient between two hospital locations)
tag_transfers <- function(adm_df, max_gap_days = 2L) {
  # assumes adm_df has: patient_id, ad_date, ad_location, and row index (ie, form_index or form_index_adm)
  stop_if_missing(adm_df, c("patient_id","ad_date","ad_location"), "tag_transfers")
  
  adm_df %>%
    arrange(patient_id, ad_date) %>%
    group_by(patient_id) %>%
    mutate(
      prev_date = lag(ad_date),
      prev_loc  = lag(ad_location),
      gap_days  = as.integer(ad_date - prev_date),
      is_transfer_edge = !is.na(prev_date) & gap_days > 0 & gap_days <= max_gap_days & !is.na(prev_loc) & prev_loc != ad_location
    ) %>%
    # build a block id where consecutive transfer edges chain together; non-transfer rows break the chain
    mutate(
      start_new_block = if_else(is.na(is_transfer_edge) | !is_transfer_edge, 1L, 0L),
      transfer_block  = cumsum(coalesce(start_new_block, 1L))
    ) %>%
    ungroup() %>%
    group_by(patient_id, transfer_block) %>%
    mutate(
      n_rows_block = n(),
      n_locs_block = n_distinct(ad_location),
      block_kind   = case_when(
        n_rows_block >= 2 & n_locs_block >= 2 ~ "transfer", # real transfers (loc change)
        TRUE                                 ~ "other" # singles & same-loc repeats
      )
    ) %>%
    ungroup()
}
# collapse the transfers into a single entry for one person
collapse_transfers_only <- function(tagged_df, index_col = NULL) {
  required <- c("patient_id","ad_date","ad_location","block_kind","transfer_block",
                "patient_birthdate","patient_sex","patient_name_first","patient_name_last",
                "admission_weightkg","readmit_seq","patient_id_twin")
  stop_if_missing(tagged_df, required, "collapse_transfers_only")
  
  idx_sym <- if (!is.null(index_col)) rlang::sym(index_col) else NULL
  
  transfers_only <- tagged_df %>%
    dplyr::filter(block_kind == "transfer") %>%
    dplyr::arrange(patient_id, ad_date, !!idx_sym) %>%
    dplyr::group_by(patient_id, transfer_block) %>%
    dplyr::summarise(
      form_index_chain   = if (!is.null(idx_sym)) paste(sort(unique(!!idx_sym)), collapse = ";") else NA_character_,
      chain_len          = dplyr::n(),
      ad_date_first      = dplyr::first(ad_date),
      ad_date_last       = dplyr::last(ad_date),
      ad_location_chain  = paste0(ad_location, collapse = "→"),
      ad_date            = ad_date_first,
      ad_location        = dplyr::last(ad_location),
      # carry key person/episode fields
      patient_id         = dplyr::first(patient_id),
      patient_id_twin    = dplyr::first(patient_id_twin),
      readmit_seq        = dplyr::first(readmit_seq),
      patient_birthdate  = dplyr::first(patient_birthdate),
      patient_sex        = dplyr::first(patient_sex),
      patient_name_first = dplyr::first(patient_name_first),
      patient_name_last  = dplyr::first(patient_name_last),
      admission_weightkg = dplyr::first(admission_weightkg),
      form_index_adm     = if (!is.null(idx_sym)) dplyr::first(!!idx_sym) else NA_integer_,
      .groups = "drop"
    )
  
  passthrough <- tagged_df %>%
    dplyr::filter(block_kind != "transfer") %>%
    dplyr::mutate(
      form_index_chain  = NA_character_,
      chain_len         = 1L,
      ad_date_first     = ad_date,
      ad_date_last      = ad_date,
      ad_location_chain = ad_location,
      form_index_adm    = if (!is.null(idx_sym)) !!idx_sym else NA
    ) %>%
    dplyr::select(names(transfers_only))  # keeps patient_id_twin + readmit_seq, too
  
  dplyr::bind_rows(transfers_only, passthrough) %>%
    dplyr::arrange(patient_id, ad_date, ad_location)
}


# Collapse duplicates
# --- collapse obvious duplicate re-entries within an episode (admissions)
collapse_strict_dups_adm <- function(adm_ready,
                                     use_vitals = TRUE) {
  required <- c("form_index_adm","patient_id_twin","patient_id",
                "readmit_seq","ad_date","ad_location",
                "patient_name_first","patient_name_last","patient_sex",
                "patient_birthdate","admission_weightkg")
  stop_if_missing(adm_ready, required, "collapse_strict_dups_adm")
  
  # vitals that strengthen uniqueness if present
  vitals_candidates <- c("admission_heartratebpm",
                         "admission_respiratoryratecpm",
                         "admission_temp_c",
                         "admission_oxygensat")
  vitals <- if (use_vitals) intersect(vitals_candidates, names(adm_ready)) else character(0)
  
  # grouping keys = person + episode + location + strict fields (+ vitals if available)
  grp <- c("patient_id_twin","readmit_seq","ad_date","ad_location",
           "patient_name_first","patient_name_last","patient_sex",
           "patient_birthdate","admission_weightkg", vitals)
  
  # optional pass-through (for context)
  pass_through <- intersect(c("patient_town","patient_communityname"), names(adm_ready))
  
  collapsed <- adm_ready %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) %>%
    dplyr::arrange(ad_date, form_index_adm, .by_group = TRUE) %>%
    dplyr::summarise(
      n_rows_collapsed    = dplyr::n(),
      form_index_adm      = dplyr::first(form_index_adm),
      form_index_dup_list = paste(sort(unique(form_index_adm)), collapse = ";"),
      dplyr::across(dplyr::all_of(pass_through), first_non_missing),
      .groups = "drop"
    )
  
  # diagnostics
  n_before <- nrow(adm_ready)
  n_after  <- nrow(collapsed)
  n_groups_gt1 <- sum(collapsed$n_rows_collapsed > 1)
  message(sprintf("[Admission dup collapse] rows: %d → %d | groups collapsed (size>1): %d",
                  n_before, n_after, n_groups_gt1))
  
  collapsed
}
collapse_strict_dups_dis <- function(dis_df, use_vitals = TRUE) {
  required <- c("form_index","patient_id","patient_id_twin","readmit_seq",
                "ad_date","discharge_location",
                "patient_name_first","patient_name_last","patient_sex",
                "patient_birthdate","admission_weightkg")
  stop_if_missing(dis_df, required, "collapse_strict_dups_dis")
  
  vitals_candidates <- c("discharge_temp_c","discharge_heartratebpm",
                         "discharge_respiratoryratecpm","discharge_oxygensat")
  vitals <- if (use_vitals) intersect(vitals_candidates, names(dis_df)) else character(0)
  
  grp <- c("patient_id_twin","readmit_seq","ad_date","discharge_location",
           "patient_name_first","patient_name_last","patient_sex",
           "patient_birthdate","admission_weightkg", vitals)
  
  pass_through <- intersect(c("patient_town","patient_communityname","patient_disposition"), names(dis_df))
  
  collapsed <- dis_df %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(grp))) %>%
    dplyr::arrange(ad_date, form_index, .by_group = TRUE) %>%
    dplyr::summarise(
      n_rows_collapsed     = dplyr::n(),
      form_index_dis       = dplyr::first(form_index),
      form_index_dup_list  = paste(sort(unique(form_index)), collapse = ";"),
      dplyr::across(dplyr::all_of(pass_through), first_non_missing),
      .groups = "drop"
    )
  
  message(sprintf("[Discharge dup collapse] rows: %d → %d | groups collapsed (size>1): %d",
                  nrow(dis_df), nrow(collapsed), sum(collapsed$n_rows_collapsed > 1)))
  collapsed
}
