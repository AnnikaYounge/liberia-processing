# 03_table1_overview.R
# basic Table 1 outputs (counts, age, sex, referral, location, outcomes)

df <- readRDS(file.path(OUT_DIR, "df_core.rds"))

# get table 1
tab1 <- list(
  counts = tibble(
    metric = c("Rows",
               "Unique patients (base ID)",
               "Unique patients (twin-only, non-NA)",
               "Unique patients (FINAL ID used)",
               "Episodes (FINAL ID | ad_date)"),
    value  = c(
      nrow(df),
      if ("patient_id" %in% names(df)) n_distinct(df$patient_id) else NA_integer_,
      if ("patient_id_twin" %in% names(df)) n_distinct(na.omit(df$patient_id_twin)) else NA_integer_,
      n_distinct(df$patient_id_final),
      n_distinct(na.omit(df$episode_key))
    )
  ),

  age = df %>%
    summarise(n_with_age  = sum(!is.na(age_days)),
              median_days = median(age_days, na.rm = TRUE),
              IQR_days    = IQR(age_days, na.rm = TRUE)),
  age_bins = df %>%
    count(age_bin_adm, name = "n") %>%
    mutate(pct = n / sum(n)),
  sex = if ("patient_sex" %in% names(df))
    df %>% count(patient_sex, name="n") %>% mutate(pct=n/sum(n)) else NULL,
  referral = if ("patient_referred_ind" %in% names(df))
    df %>% count(patient_referred_ind, name="n") %>% mutate(pct=n/sum(n)) else NULL,
  location_adm = if ("ad_location" %in% names(df))
    df %>% count(ad_location, name="n") %>% mutate(pct=n/sum(n)) else NULL,
  outcome = df %>% summarise(
    mortality  = mean(mortality, na.rm = TRUE) %>% {ifelse(is.nan(.), NA, .)},
    median_los = median(los_days, na.rm = TRUE),
    IQR_los    = IQR(los_days, na.rm = TRUE)
  )
)

write_csv(tab1$counts,        file.path(OUT_DIR, "table1_counts.csv"))
write_csv(tab1$age,           file.path(OUT_DIR, "table1_age_summary.csv"))
write_csv(tab1$age_bins,      file.path(OUT_DIR, "table1_age_bins.csv"))
if (!is.null(tab1$sex))          write_csv(tab1$sex,          file.path(OUT_DIR, "table1_sex.csv"))
if (!is.null(tab1$referral))     write_csv(tab1$referral,     file.path(OUT_DIR, "table1_referral.csv"))
if (!is.null(tab1$location_adm)) write_csv(tab1$location_adm, file.path(OUT_DIR, "table1_adm_location.csv"))
write_csv(tab1$outcome,       file.path(OUT_DIR, "table1_outcomes.csv"))
