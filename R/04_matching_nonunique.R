# MATCHING CALLS
# 1 - readmissions
adm_readm <- add_readmit_seq(unmatched_adm, readmit_gap_days = 15L, transfer_gap_max = 2L)
dis_readm <- add_readmit_seq_generic(unmatched_dis, readmit_gap_days = 15L,
                                     transfer_gap_max = 2L,
                                     id_col="patient_id", date_col="ad_date", loc_col="discharge_location")
# 2 - twins (in both admission and discharge)
adm_twins <- split_twins_consistent(adm_readm)
dis_twins <- split_twins_consistent(dis_readm)

# 3 - transfers (admissions only)
tagged    <- tag_transfers(adm_twins, max_gap_days = 2L)
adm_trans <- collapse_transfers_only(tagged, index_col = "form_index")

# 4 - strict duplicate collapse (transfer + twin aware on both sides)
adm_ready_strict <- collapse_strict_dups_adm(adm_trans, use_vitals = TRUE)
dis_ready_strict <- collapse_strict_dups_dis(dis_twins,  use_vitals = TRUE)

# 5 - relaxed duplicate collapse
# now that we've dealt with readmissions, transfers, and twins, we also do another duplicate matching
adm_pool <- adm_ready_strict
dis_pool <- dis_ready_strict %>% dplyr::mutate(ad_location = discharge_location)

key_fields_ep <- c(
  "patient_id_twin", "patient_birthdate", "patient_sex",
  "patient_name_first", "patient_name_last", "admission_weightkg",
  "ad_date", "ad_location", "patient_town", "patient_communityname"
)

episode_steps <- list(
  list(
    by     = c("patient_id_twin","ad_date","patient_sex","patient_name_first",
               "patient_name_last","admission_weightkg","ad_location"),
    prefer = "discharge", label = "E1: twinID + date + strict + loc"
  ),
  list(
    by     = c("patient_id_twin","ad_date","patient_sex","patient_name_first",
               "patient_name_last","admission_weightkg"),
    prefer = "discharge", label = "E2: twinID + date + strict (no loc)"
  )
)

matched_episode_sets <- list()
for (i in seq_along(episode_steps)) {
  st <- episode_steps[[i]]
  step <- perform_match_resolve(
    adm_pool, dis_pool,
    key_fields      = key_fields_ep,
    by_cols        = st$by,
    resolve_prefer = st$prefer,
    id_col         = NULL,                  # IMPORTANT: row-level carve out (not whole patient)
    label          = st$label,
    preview_n      = 0
  )
  matched_episode_sets[[paste0("E", i)]] <- step$matched
  message(sprintf("[%-28s] matched: %-4d | adm_remaining: %-4d | dis_remaining: %-4d",
                  st$label, nrow(step$matched), nrow(step$adm_remaining), nrow(step$dis_remaining)))
  adm_pool <- step$adm_remaining
  dis_pool <- step$dis_remaining
}

# Bad-ID matching (ignore ID entirely, same-day strict with names/sex/weight)
bad_id_ep_steps <- list(
  list(
    by     = c("ad_date","patient_sex","patient_name_first",
               "patient_name_last","admission_weightkg","ad_location"),
    prefer = "discharge", label = "B1: no ID + date + strict + loc"
  ),
  list(
    by     = c("ad_date","patient_sex","patient_name_first",
               "patient_name_last","admission_weightkg"),
    prefer = "discharge", label = "B2: no ID + date + strict (no loc)"
  )
)

matched_bad_id_ep_sets <- list()
for (i in seq_along(bad_id_ep_steps)) {
  st <- bad_id_ep_steps[[i]]
  step <- perform_match_resolve(
    adm_pool, dis_pool,
    key_fields      = key_fields_ep,
    by_cols        = st$by,
    resolve_prefer = st$prefer,
    id_col         = NULL,                 # row-level carve out
    label          = st$label,
    preview_n      = 0
  )
  matched_bad_id_ep_sets[[paste0("B", i)]] <- step$matched
  message(sprintf("[%-28s] matched: %-4d | adm_remaining: %-4d | dis_remaining: %-4d",
                  st$label, nrow(step$matched), nrow(step$adm_remaining), nrow(step$dis_remaining)))
  adm_pool <- step$adm_remaining
  dis_pool <- step$dis_remaining
}

# bind new matches and append to the previous final_matched
new_episode_matches <- dplyr::bind_rows(
  c(matched_episode_sets, matched_bad_id_ep_sets)
)

# check that we have the form indices
stopifnot("form_index_adm" %in% names(new_episode_matches))
stopifnot("form_index_dis" %in% names(new_episode_matches))

# append to existing final matched set
final_matched <- dplyr::bind_rows(final_matched, new_episode_matches)


used_adm_idx <- unique(final_matched$form_index_adm)
used_dis_idx <- unique(final_matched$form_index_dis)

unmatched_adm <- admission_data %>% dplyr::filter(!(form_index %in% used_adm_idx))
unmatched_dis <- discharge_data %>% dplyr::filter(!(form_index %in% used_dis_idx))

message(sprintf("[COMPLETED] new matches: %d | total final_matched: %d",
                nrow(new_episode_matches), nrow(final_matched)))
message(sprintf("(LEFTOVER) unmatched now — Admission: %d | Discharge: %d",
                nrow(unmatched_adm), nrow(unmatched_dis)))
rm(new_episode_matches)
# feature list from final_matched
feature_list <- tibble::tibble(
  variable = names(final_matched),
  type     = vapply(final_matched, function(x) class(x)[1], character(1))
)

# Write final dataset
readr::write_csv(final_matched, file.path("data","processed","final_matched.csv"))