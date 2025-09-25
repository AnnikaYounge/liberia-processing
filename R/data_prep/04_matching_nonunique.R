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

# ------ enrich the new episode matches with original rows -----

# keep original columns you want to propagate
pass_through_cols <- unique(c("form_index", key_fields_ep))

# admission and discharge originals with suffixed names
adm_original <- admission_data |>
  dplyr::select(dplyr::any_of(pass_through_cols), dplyr::everything()) |>
  dplyr::rename(form_index_adm = form_index) |>
  dplyr::rename_with(~ paste0(.x, "_adm"), -form_index_adm)

dis_original <- discharge_data |>
  dplyr::select(dplyr::any_of(pass_through_cols), dplyr::everything()) |>
  dplyr::rename(form_index_dis = form_index) |>
  dplyr::rename_with(~ paste0(.x, "_dis"), -form_index_dis)

# attach original payload to the episode matches
new_episode_matches <- new_episode_matches |>
  dplyr::left_join(adm_original, by = "form_index_adm") |>
  dplyr::left_join(dis_original, by = "form_index_dis")

# build unified columns by coalescing into the canonical names
# existing canonical value wins, then _adm, then _dis (type-safe; no placeholder NA columns)
for (f in key_fields_ep) {
  fa <- paste0(f, "_adm")
  fd <- paste0(f, "_dis")
  
  sources <- list()
  if (f  %in% names(new_episode_matches))  sources <- c(sources, list(new_episode_matches[[f]]))
  if (fa %in% names(new_episode_matches)) sources <- c(sources, list(new_episode_matches[[fa]]))
  if (fd %in% names(new_episode_matches)) sources <- c(sources, list(new_episode_matches[[fd]]))
  
  if (length(sources) > 0L) {
    out <- sources[[1L]]
    if (length(sources) > 1L) {
      for (k in 2:length(sources)) out <- dplyr::coalesce(out, sources[[k]])
    }
    new_episode_matches[[f]] <- out
  }
}

# optional: drop the suffixed copies for those key fields to keep the schema tidy
drop_suffixed <- c(paste0(key_fields_ep, "_adm"), paste0(key_fields_ep, "_dis"))
new_episode_matches <- dplyr::select(new_episode_matches, -dplyr::any_of(drop_suffixed))

# keep only rows that reference at least one form index
new_episode_matches <- new_episode_matches |>
  dplyr::filter(!is.na(form_index_adm) | !is.na(form_index_dis))

# deduplicate new matches on their key pair
new_episode_matches <- new_episode_matches |>
  dplyr::distinct(form_index_adm, form_index_dis, .keep_all = TRUE)

# ensure we don't re-append pairs already in final_matched
new_episode_matches <- dplyr::anti_join(
  new_episode_matches,
  dplyr::select(final_matched, form_index_adm, form_index_dis),
  by = c("form_index_adm","form_index_dis")
)

# now append
final_matched <- dplyr::bind_rows(final_matched, new_episode_matches)

# get a patient_key value
final_matched <- final_matched |>
  dplyr::mutate(patient_key = dplyr::coalesce(patient_id, patient_id_twin))

# ensure episodes are unique by pair of indices
stopifnot(!any(duplicated(final_matched[c("form_index_adm","form_index_dis")])))

# ---- integrity checks / cleanup: drop rows with all substantive fields empty --

empty_check_fields <- c(
  "patient_birthdate", "patient_sex",
  "patient_name_first", "patient_name_last",
  "ad_date", "ad_location",
  "patient_town", "patient_communityname"
)

# confirm they all exist
stopifnot(all(empty_check_fields %in% names(final_matched)))

# define "present": characters/factors are non-NA and non-empty; numerics/dates are non-NA
present_vec <- function(x) {
  if (is.character(x) || is.factor(x)) {
    v <- trimws(as.character(x))
    !is.na(v) & v != ""
  } else {
    !is.na(x)
  }
}

# count rows where ALL substantive fields are empty
to_drop_n <- final_matched |>
  dplyr::mutate(.all_empty = dplyr::if_all(dplyr::all_of(empty_check_fields), ~ !present_vec(.))) |>
  dplyr::summarise(n = sum(.all_empty)) |>
  dplyr::pull(n)

if (to_drop_n > 0) {
  message(sprintf("removing %d rows where all substantive fields are empty.", to_drop_n))
} else {
  message("no rows removed for all-empty substantive fields.")
}

# keep rows where ANY substantive field is present
final_matched <- final_matched |>
  dplyr::filter(dplyr::if_any(dplyr::all_of(empty_check_fields), present_vec))

# outputs (recompute after cleanup so counts match the csv)
used_adm_idx <- unique(final_matched$form_index_adm)
used_dis_idx <- unique(final_matched$form_index_dis)

unmatched_adm <- admission_data %>% dplyr::filter(!(form_index %in% used_adm_idx))
unmatched_dis <- discharge_data %>% dplyr::filter(!(form_index %in% used_dis_idx))

message(sprintf("[completed] new matches: %d | total final_matched: %d",
                nrow(new_episode_matches), nrow(final_matched)))
message(sprintf("(leftover) unmatched now — admission: %d | discharge: %d",
                nrow(unmatched_adm), nrow(unmatched_dis)))

# write final datasets
readr::write_csv(final_matched, file.path("data","processed","final_matched.csv"))
readr::write_csv(unmatched_adm, file.path("data","processed","unmatched_admissions.csv"))
readr::write_csv(unmatched_dis, file.path("data","processed","unmatched_discharges.csv"))

# feature list from final_matched (optional; remove if unused downstream)
feature_list <- tibble::tibble(
  variable = names(final_matched),
  type     = vapply(final_matched, function(x) class(x)[1], character(1))
)

# remove variables from environment (safe: only remove if they exist)
to_rm <- c("adm_pool","adm_readm","adm_ready_strict","adm_trans","adm_twins",
           "bad_id_ep_steps","dis_pool","dis_readm","dis_ready_strict","dis_twins",
           "episode_steps","matched_bad_id_ep_sets","matched_episode_sets",
           "st","step","tagged","new_episode_matches")
to_rm <- intersect(to_rm, ls())
if (length(to_rm)) rm(list = to_rm)
