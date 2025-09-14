library(dplyr)
library(readr)
# ---- We can generate a validation sample of a random 5% of the matched data to see if it looks correct ---- 

make_validation_sample <- function(final_matched,
                                   admission_data,
                                   discharge_data,
                                   prop = 0.05,
                                   idx_adm = "form_index_adm",
                                   idx_dis = "form_index_dis",
                                   out_csv = "validation_sample_5pct.csv") {
  stopifnot(idx_adm %in% names(final_matched), idx_dis %in% names(final_matched))
  
  # sample ~5%
  sampled <- final_matched %>%
    slice_sample(prop = prop) %>%
    select(all_of(c(idx_adm, idx_dis)))
  
  key_fields <- c(
    "patient_id","patient_birthdate","patient_sex",
    "patient_name_first","patient_name_last",
    "admission_weightkg","ad_date","patient_town","patient_communityname"
  )
  
  # originals (pre-resolution) from the cleaned sources
  adm_original <- admission_data %>%
    select(any_of(c("form_index","ad_location", key_fields))) %>%
    rename(!!idx_adm := form_index)
  
  dis_original <- discharge_data %>%
    select(any_of(c("form_index","discharge_location", key_fields))) %>%
    rename(!!idx_dis := form_index)
  
  # join admission, suffix the columns that came from admission
  adm_cols_to_suffix <- setdiff(names(adm_original), idx_adm)
  
  with_adm <- sampled %>%
    left_join(adm_original, by = setNames(idx_adm, idx_adm)) %>%
    rename_at(vars(all_of(adm_cols_to_suffix)), ~ paste0(.x, "_adm"))
  
  # join discharge, suffix the columns that came from discharge
  dis_cols_to_suffix <- setdiff(names(dis_original), idx_dis)
  
  validation_sample <- with_adm %>%
    left_join(dis_original, by = setNames(idx_dis, idx_dis)) %>%
    rename_at(vars(all_of(dis_cols_to_suffix)), ~ paste0(.x, "_dis"))
  
  col_order <- c(
    idx_adm, idx_dis,
    "patient_id_adm","patient_id_dis",
    "patient_name_first_adm","patient_name_first_dis",
    "patient_name_last_adm","patient_name_last_dis",
    "patient_sex_adm","patient_sex_dis",
    "patient_birthdate_adm","patient_birthdate_dis",
    "admission_weightkg_adm","admission_weightkg_dis",
    "ad_date_adm","ad_date_dis",
    "ad_location_adm","discharge_location_dis",
    "patient_town_adm","patient_town_dis",
    "patient_communityname_adm","patient_communityname_dis"
  )
  validation_sample <- validation_sample %>% select(any_of(col_order), everything())
  
  message(sprintf("Validation sample: %d rows → %s", nrow(validation_sample), out_csv))
  
  validation_sample
}

# choice of random seed
set.seed(80)
validation_set <- make_validation_sample(final_matched, admission_data, discharge_data, prop = 0.05)

rm(adm_pool, adm_readm, adm_ready_strict, adm_trans, adm_twins, bad_id_ep_steps, dis_pool, dis_readm, dis_ready_strict, dis_twins, episode_steps, matched_bad_id_ep_sets, matched_episode_sets, st, step, tagged)

readr::write_csv(validation_set, file.path("data","processed","validation_sample_5pct.csv"))