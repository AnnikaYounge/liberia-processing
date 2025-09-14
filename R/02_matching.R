# ---- SETUP ----

# identify unique patient IDs in both datasets (to first match unique IDs)
admission_id_counts <- table(admission_data$patient_id)
discharge_id_counts <- table(discharge_data$patient_id)

unique_adm_ids <- names(admission_id_counts[admission_id_counts == 1])
unique_dis_ids <- names(discharge_id_counts[discharge_id_counts == 1])

# gets matching unique IDs
valid_ids <- intersect(unique_adm_ids, unique_dis_ids)

adm_pool <- admission_data %>% filter(patient_id %in% valid_ids)
dis_pool <- discharge_data %>% filter(patient_id %in% valid_ids)

initial_n <- nrow(adm_pool)
matched_sets <- list()

# set up the matching and resolve function
# --- lets you progressively match between admission and discharge based on a set of fields that need to match
# --- resolve_prefer lets you specify whether to take the admission or discharge value if they both have non-empty conflicting values

perform_match_resolve <- function(adm,
                                  dis,
                                  key_fields,
                                  by_cols,
                                  resolve_prefer = "discharge",
                                  id_col = "patient_id",
                                  label = NULL,
                                  preview_n = 0) {
  
  temp <- inner_join(adm, dis, by = by_cols, suffix = c("_adm", "_dis"))
  
  # get the matched rows from the two pools
  if (!is.null(id_col) && id_col %in% names(temp)) {
    matched_ids   <- unique(temp[[id_col]])
    adm_remaining <- adm %>% filter(!( .data[[id_col]] %in% matched_ids))
    dis_remaining <- dis %>% filter(!( .data[[id_col]] %in% matched_ids))
  } else {
    matched_ids   <- nrow(temp)                              # just for logging
    adm_remaining <- anti_join(adm, temp, by = by_cols)
    dis_remaining <- anti_join(dis, temp, by = by_cols)
  }
  
  # resolve fields
  resolve_fields <- setdiff(key_fields, by_cols)
  
  if (preview_n > 0 && nrow(temp) > 0 && length(resolve_fields) > 0) {
    pre_cols <- unlist(lapply(resolve_fields, \(f) paste0(f, c("_adm", "_dis"))))
    pre_cols <- pre_cols[pre_cols %in% names(temp)]
    message(sprintf(
      "%s: matched %d rows. Remaining adm=%d, dis=%d. Resolving %s",
      label, length(matched_ids), nrow(adm_remaining), nrow(dis_remaining),
      paste(resolve_fields, collapse = ", ")
    ))
    print(temp %>% select(all_of(pre_cols)) %>% head(preview_n))
  }
  
  for (f in resolve_fields) {
    adm_col <- paste0(f, "_adm")
    dis_col <- paste0(f, "_dis")
    if (adm_col %in% names(temp) && dis_col %in% names(temp)) {
      temp[[f]] <- dplyr::case_when(
        !is.na(temp[[adm_col]]) &  is.na(temp[[dis_col]])                       ~ temp[[adm_col]],
        is.na(temp[[adm_col]]) & !is.na(temp[[dis_col]])                       ~ temp[[dis_col]],
        !is.na(temp[[adm_col]]) & !is.na(temp[[dis_col]]) & resolve_prefer == "admission" ~ temp[[adm_col]],
        !is.na(temp[[adm_col]]) & !is.na(temp[[dis_col]]) & resolve_prefer == "discharge" ~ temp[[dis_col]],
        TRUE                                                                    ~ temp[[adm_col]]
      )
      temp[[adm_col]] <- NULL
      temp[[dis_col]] <- NULL
    }
  }
  
  # tidy output
  keep_cols <- unique(c(by_cols, resolve_fields, id_col))
  keep_cols <- keep_cols[keep_cols %in% names(temp)]
  temp_clean <- temp %>% select(all_of(keep_cols), everything())
  
  if (preview_n > 0 && nrow(temp_clean) > 0) {
    message("POST-RESOLUTION (sample):")
    print(temp_clean %>% head(preview_n))
  }
  
  list(
    matched       = temp_clean,
    adm_remaining = adm_remaining,
    dis_remaining = dis_remaining
  )
}

# ------ matching steps for the unique IDs -----------
# can be simplified into just the most flexible steps (because the stricter matches then will still be matched)
# but breaking it down into steps can let you examine exactly the subset of individuals who match/don't match on certain characteristics

# we match people based on key fields, which are entered both in the admission and discharge forms.
key_fields <- c("patient_id", "patient_birthdate", "patient_sex", "patient_name_first", "patient_name_last", "admission_weightkg", "ad_date", "patient_town", "patient_communityname")

# the match steps give the list of characteristics that need to match between patients for them to be joined. And the excluded variables from key_fields are resolved by picking the non-empty/preferred side.
match_steps <- list(
  list(by = c("patient_id", "patient_birthdate", "patient_sex", "patient_name_first", "patient_name_last", "admission_weightkg", "ad_date", "patient_town", "patient_communityname"), prefer = "discharge", label = "Step 1: all"),
  list(by = c("patient_id", "patient_birthdate", "patient_sex", "patient_name_first", "patient_name_last", "admission_weightkg", "ad_date", "patient_town"), prefer = "admission", label = "Step 2"),
  list(by = c("patient_id", "patient_birthdate", "patient_sex", "patient_name_first", "patient_name_last", "admission_weightkg", "ad_date"), prefer = "admission", label = "Step 3"),
  list(by = c("patient_id", "patient_birthdate", "patient_sex", "patient_name_first", "admission_weightkg", "ad_date"), prefer = "discharge", label = "Step 4"),
  list(by = c("patient_id", "patient_birthdate", "patient_sex", "patient_name_last", "admission_weightkg", "ad_date"), prefer = "discharge", label = "Step 5"),
  list(by = c("patient_id", "patient_birthdate", "patient_sex", "patient_name_first", "patient_name_last", "ad_date"), prefer = "admission", label = "Step 6"),
  list(by = c("patient_id", "patient_birthdate", "patient_sex", "patient_name_first", "ad_date"), prefer = "discharge", label = "Step 7"),
  list(by = c("patient_id", "patient_birthdate", "patient_sex", "patient_name_last", "admission_weightkg"), prefer = "discharge", label = "Step 8"),
  list(by = c("patient_id", "patient_birthdate", "patient_sex", "patient_name_first", "admission_weightkg"), prefer = "discharge", label = "Step 9"),
  list(by = c("patient_id", "patient_birthdate", "patient_sex", "admission_weightkg", "ad_date"), prefer = "discharge", label = "Step 10"),
  list(by = c("patient_id", "patient_birthdate", "patient_sex", "patient_name_last", "ad_date"), prefer = "discharge", label = "Step 11"),
  list(by = c("patient_id", "patient_birthdate", "patient_sex", "patient_name_last", "ad_date"), prefer = "discharge", label = "Step 12"),
  list(by = c("patient_id", "patient_birthdate", "patient_name_last", "ad_date", "admission_weightkg"), prefer = "discharge", label = "Step 13"),
  list(by = c("patient_id", "patient_birthdate", "patient_name_first", "ad_date", "admission_weightkg"), prefer = "discharge", label = "Step 14"),
  list(by = c("patient_id", "ad_date", "patient_sex", "patient_name_first", "patient_name_last", "admission_weightkg"), prefer = "discharge", label = "Step 15"),
  list(by = c("patient_id", "ad_date", "patient_sex", "patient_name_first", "admission_weightkg"), prefer = "discharge", label = "Step 16"),
  list(by = c("patient_id", "ad_date", "patient_sex", "patient_name_last", "admission_weightkg"), prefer = "discharge", label = "Step 17"),
  list(by = c("patient_id", "ad_date", "patient_name_first", "patient_name_last", "admission_weightkg"), prefer = "discharge", label = "Step 18"),
  list(by = c("patient_id", "patient_birthdate", "patient_sex", "patient_name_last", "patient_town"), prefer = "discharge", label = "Step 19"),
  list(by = c("patient_id", "patient_sex", "patient_name_first", "patient_name_last", "admission_weightkg"), prefer = "discharge", label = "Step 20"),
  list(by = c("patient_id", "patient_sex", "patient_name_first", "ad_date"), prefer = "discharge", label = "Step 21"),
  list(by = c("patient_id", "patient_birthdate", "patient_name_first", "patient_name_last", "ad_date"), prefer = "discharge", label = "Step 22"),
  list(by = c("patient_id", "ad_date", "patient_sex", "patient_name_last", "patient_town"), prefer = "discharge", label = "Step 23"),
  list(by = c("patient_id", "patient_birthdate", "patient_sex", "patient_name_last"), prefer = "discharge", label = "Step 24"),
  list(by = c("patient_id", "ad_date", "patient_name_last"), prefer = "discharge", label = "Step 25"),
  list(by = c("patient_id", "patient_birthdate", "patient_name_last"), prefer = "discharge", label = "Step 26"),
  list(by = c("patient_id", "patient_sex", "ad_date"), prefer = "discharge", label = "Step 27"),
  list(by = c("patient_id", "patient_birthdate", "ad_date"), prefer = "discharge", label = "Step 28"),
  list(by = c("patient_id", "patient_name_first", "patient_birthdate"), prefer = "discharge", label = "Step 29")
)

# we then go through each fo the steps and track who was matched
for (i in seq_along(match_steps)) {
  step <- perform_match_resolve(
    adm_pool, dis_pool, 
    key_fields,
    by_cols        = match_steps[[i]]$by, 
    resolve_prefer = match_steps[[i]]$prefer, 
    label          = match_steps[[i]]$label,
    id_col         = "patient_id",
    preview_n      = 0  # no previews unless debugging
  )
  
  matched_sets[[paste0("step", i)]] <- step$matched
  
  message(sprintf("[%-10s] matched: %-4d | admission unique remaining: %-4d | discharge unique remaining: %-4d",
                  match_steps[[i]]$label,
                  nrow(step$matched),
                  nrow(step$adm_remaining),
                  nrow(step$dis_remaining)))
  
  adm_pool <- step$adm_remaining
  dis_pool <- step$dis_remaining
}

# final output after matching unique IDs
final_matched <- bind_rows(matched_sets)
final_unmatched_adm <- adm_pool
final_unmatched_dis <- dis_pool

message(sprintf("(DONE) Final joined dataset: %d rows", nrow(final_matched)))
message(sprintf("(LEFTOVER) Remaining unmatched in admission: %d", nrow(final_unmatched_adm)))
message(sprintf("(LEFTOVER) Remaining unmatched in discharge: %d", nrow(final_unmatched_dis)))
rm(final_unmatched_adm)
rm(final_unmatched_dis)

# --------- Next main step: matching mistakenly entered IDs for unique IDs---------
# we now find the IDs that were unique per patient but didn't have a direct single match
asym_ids <- setdiff(
  union(unique_adm_ids, unique_dis_ids),
  intersect(unique_adm_ids, unique_dis_ids)
)

# and we go back through the matching steps, but we now match on characteristics that aren't the main ID
adm_pool <- admission_data %>%
  filter(patient_id %in% asym_ids & patient_id %in% unique_adm_ids)
dis_pool <- discharge_data %>%
  filter(patient_id %in% asym_ids & patient_id %in% unique_dis_ids)

msg <- function(...) message(sprintf(...))

msg("Entries whose ID appears on only one side – Admission: %d   Discharge: %d",
    nrow(adm_pool), nrow(dis_pool))

# get the steps for matching bad ids
bad_id_steps <- list(
  list(by = c("patient_birthdate", "patient_sex", "patient_name_first", "patient_name_last", "admission_weightkg", "ad_date", "patient_town", "patient_communityname"), prefer = "discharge", label = "Step 1: all"),
  list(by = c("patient_birthdate", "patient_sex", "patient_name_first", "patient_name_last", "admission_weightkg", "ad_date", "patient_town"), prefer = "admission", label = "Step 2"),
  list(by = c("patient_birthdate", "patient_sex", "patient_name_first", "patient_name_last", "admission_weightkg", "ad_date"), prefer = "admission", label = "Step 3"),
  list(by = c("patient_birthdate", "patient_sex", "patient_name_first", "admission_weightkg", "ad_date"), prefer = "discharge", label = "Step 4"),
  list(by = c("patient_birthdate", "patient_sex", "patient_name_last", "admission_weightkg", "ad_date"), prefer = "discharge", label = "Step 5"),
  list(by = c("patient_birthdate", "patient_sex", "patient_name_first", "patient_name_last", "ad_date"), prefer = "discharge", label = "Step 6"),
  list(by = c("patient_birthdate", "patient_sex", "patient_name_last", "patient_town", "ad_date"), prefer = "discharge", label = "Step 7"),
  list(by = c("patient_sex", "patient_name_first", "patient_name_last", "admission_weightkg", "ad_date"), prefer = "discharge", label = "Step 8"),
  list(by = c("patient_sex", "patient_name_last", "admission_weightkg", "ad_date"), prefer = "discharge", label = "Step 9"),
  list(by = c("patient_sex", "patient_name_first", "patient_name_last", "ad_date"), prefer = "discharge", label = "Step 10")
  )

matched_bad_id_sets <- list()

for (i in seq_along(bad_id_steps)) {
  step <- perform_match_resolve(adm_pool, dis_pool, 
                                key_fields,
                                by_cols = bad_id_steps[[i]]$by, 
                                resolve_prefer = bad_id_steps[[i]]$prefer, 
                                label = bad_id_steps[[i]]$label, 
                                id_col = NULL,
                                preview_n = 0)
  matched_bad_id_sets[[i]] <- step$matched
  adm_pool <- step$adm_remaining
  dis_pool <- step$dis_remaining
  msg("%s – cumulative bad-ID links recovered: %d",
      bad_id_steps[[i]]$label, nrow(bind_rows(matched_bad_id_sets)))
}

# final output after matching unique IDs
bad_id_matches <- bind_rows(matched_bad_id_sets)
final_matched <- bind_rows(final_matched, bad_id_matches)

# get rid of extra variables
rm(bad_id_matches)
rm(step)
rm(match_steps)
rm(matched_bad_id_sets)
rm(matched_sets)
rm(adm_pool)
rm(dis_pool)
rm(bad_id_steps)


message(sprintf("Perfect ID matches: %d rows", nrow(final_matched)))

used_adm_idx <- unique(final_matched$form_index_adm)
used_dis_idx <- unique(final_matched$form_index_dis)

unmatched_adm <- admission_data %>% 
  filter(!(form_index %in% used_adm_idx))
unmatched_dis <- discharge_data %>%
  filter(!(form_index %in% used_dis_idx))

message(sprintf("Remaining unmatched counts: Admission = %d | Discharge = %d",
                nrow(unmatched_adm), nrow(unmatched_dis)))

message(sprintf("(LEFTOVER) Remaining unmatched in admission: %d", nrow(unmatched_adm)))
message(sprintf("(LEFTOVER) Remaining unmatched in discharge: %d", nrow(unmatched_dis)))
