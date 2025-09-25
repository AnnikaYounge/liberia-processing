# 02_derive_core.R
# create a single analysis dataframe from final_matched and write it to disk

# expects final_matched in the global env
stopifnot(exists("final_matched"))

# derive core fields we use everywhere
derive_core <- function(df) {
  has_dispo <- "patient_disposition" %in% names(df)
  has_ref   <- "patient_referred_ind" %in% names(df)
  has_age_days_col <- "patient_age_days_adm" %in% names(df)
  
  df %>%
    mutate(
      # twin-aware id we use throughout
      patient_id_final = ifelse(!is.na(patient_id_twin) & patient_id_twin != "",
                                patient_id_twin, patient_id),
      # age (days), prefer recorded, else compute from birthdate
      age_days   = if (has_age_days_col) {
        coalesce(patient_age_days_adm, as.integer(ad_date - patient_birthdate))
      } else {
        as.integer(ad_date - patient_birthdate)
      },
      age_months = ifelse(!is.na(age_days), age_days/30.44, NA_real_),
      # los and simple mortality flag
      los_days   = as.integer(dc_date - ad_date),
      mortality  = (!is.na(discharge_death_date)) |
        (has_dispo && patient_disposition == "Death"),
      referred   = if (has_ref) patient_referred_ind else NA,
      # episode key and age band
      episode_key = ifelse(!is.na(ad_date), paste(patient_id_final, ad_date, sep = "|"), NA_character_),
      age_bin_adm = age_bins(age_days)
    )
}

df <- derive_core(final_matched)

# drop rows that are completely blank in core signals
core_signals <- intersect(c(
  "patient_id_final","ad_date","dc_date","patient_sex","admission_weightkg",
  "admission_temp_c","admission_heartratebpm","ad_location",
  "dx_1_adm","dx_1_dis","patient_disposition","discharge_death_date"
), names(df))

df <- df %>%
  mutate(.all_blank = rowSums(across(all_of(core_signals), ~ !is.na(.))) == 0) %>%
  filter(!.all_blank) %>%
  select(-.all_blank)

# quick cohort counts
n_rows <- nrow(df)
n_patients_base <- if ("patient_id" %in% names(df))  n_distinct(df$patient_id) else NA_integer_
n_patients_twin <- if ("patient_id_twin" %in% names(df)) n_distinct(na.omit(df$patient_id_twin)) else NA_integer_
n_patients_final <- n_distinct(df$patient_id_final)
n_episodes <- n_distinct(na.omit(df$episode_key))
message(glue("rows={n_rows} | base_ids={n_patients_base} | twin_only_ids={n_patients_twin} | final_ids={n_patients_final} | episodes={n_episodes}"))

# write once; all other scripts read this
saveRDS(df, file.path(OUT_DIR, "df_core.rds"))

# also save the variable→domain map for missingness plots
var_domains <- tibble(variable = names(df)) %>% mutate(domain = tag_domain(variable))
saveRDS(var_domains, file.path(OUT_DIR, "var_domains.rds"))
