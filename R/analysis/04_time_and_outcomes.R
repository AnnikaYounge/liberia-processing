# 04_time_and_outcomes.R
# monthly admissions; LOS summaries + figures; mortality summaries + figures

df <- readRDS(file.path(OUT_DIR, "df_core.rds"))

# monthly admissions
if ("ad_date" %in% names(df)) {
  p_ts <- df %>%
    filter(!is.na(ad_date)) %>%
    mutate(month = floor_date(ad_date, "month")) %>%
    count(month) %>%
    ggplot(aes(month, n)) +
    geom_line(linewidth = 1, colour = pal$blue) +
    geom_point(size = 1.5, colour = pal$blue) +
    labs(x = "month", y = "admissions", title = "monthly admissions") +
    scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  save_plot(p_ts, file.path(OUT_DIR, "fig_monthly_admissions.png"), 10, 4.5)
}

# LOS overall
if ("los_days" %in% names(df)) {
  los_df <- df %>% filter(!is.na(los_days), dplyr::between(los_days, 0, 60))
  
  p_hist <- ggplot(los_df, aes(los_days)) +
    geom_histogram(bins = 60, fill = "#9ecae1", color = "#6baed6") +
    labs(x = "LOS (days)", y = "count", title = "length of stay — overall (≤60d)")
  save_plot(p_hist, file.path(OUT_DIR, "fig_los_overall_hist.png"), 8, 4.8)
  
  p_ecdf <- ggplot(los_df, aes(los_days)) +
    stat_ecdf(geom = "step", linewidth = 1, color = "#3182bd") +
    labs(x = "LOS (days)", y = "cumulative proportion", title = "length of stay — ECDF")
  save_plot(p_ecdf, file.path(OUT_DIR, "fig_los_overall_ecdf.png"), 8, 4.2)
  
  # LOS by location
  if ("ad_location" %in% names(df)) {
    p_los_loc <- los_df %>%
      mutate(ad_location = fct_na_value_to_level(as.factor(ad_location), "Unknown")) %>%
      ggplot(aes(x = ad_location, y = los_days)) +
      geom_violin(fill = pal$teal, alpha = 0.35, colour = pal$teal, linewidth = 0.6, trim = TRUE) +
      geom_boxplot(width = 0.12, outlier.size = 0.7, colour = pal$grey) +
      labs(x = "admission location", y = "LOS (days)", title = "length of stay by admission location") +
      theme(legend.position = "none")
    save_plot(p_los_loc, file.path(OUT_DIR, "fig_los_by_location.png"), 9, 5.2)
  }
}

# los by admission location — violin/box + median labels
if (all(c("los_days","ad_location") %in% names(df))) {
  los_loc <- df %>%
    filter(!is.na(los_days), los_days >= 0, los_days <= 60) %>%
    mutate(ad_location = forcats::fct_na_value_to_level(as.factor(ad_location), "Unknown"))
  
  med_loc <- los_loc %>% group_by(ad_location) %>%
    summarise(median_los = median(los_days), .groups = "drop")
  
  p <- ggplot(los_loc, aes(x = ad_location, y = los_days)) +
    geom_violin(fill = "#b2dfdb", color = "#26a69a", alpha = 0.6, trim = TRUE) +
    geom_boxplot(width = 0.15, outlier.size = 0.7) +
    geom_text(data = med_loc, aes(y = median_los, label = round(median_los,1)),
              nudge_x = 0.18, size = 3.2) +
    labs(x = "Admission location", y = "LOS (days)",
         title = "Length of stay by admission location (medians shown)")
  save_plot(p, file.path(out_dir, "fig_los_by_location_median.png"), width = 8, height = 5)
}


# age pyramid — fine neonatal split
if (all(c("patient_sex","age_days") %in% names(df))) {
  df_age <- df %>%
    mutate(
      age_years = age_days/365.25,
      age_band_fine = case_when(
        is.na(age_days)        ~ NA_character_,
        age_days <= 6          ~ "0–6d",
        age_days <= 28         ~ "7–28d",
        age_days <= 182        ~ "29–182d (≈0–6m)",
        age_days <= 365        ~ "183–365d (≈6–12m)",
        age_years > 20         ~ "20+",
        TRUE ~ paste0(floor(age_years), "–", floor(age_years)+1, "y")
      ),
      sex2 = if_else(patient_sex %in% c("Male","M","male"), "Male", "Female")
    ) %>%
    tidyr::drop_na(age_band_fine)
  
  d_pyr <- df_age %>% count(sex2, age_band_fine) %>%
    group_by(sex2) %>% mutate(n = if_else(sex2=="Male", -n, n)) %>% ungroup()
  
  p <- ggplot(d_pyr, aes(x = age_band_fine, y = n, fill = sex2)) +
    geom_col(width = 0.92) + coord_flip() +
    scale_fill_manual(values = c(Female = pal$orange, Male = pal$teal), name = "Sex") +
    scale_y_continuous(labels = abs) +
    labs(x = "Age (fine bands)", y = "Count (negative = Male)",
         title = "Age pyramid (fine neonatal split)")
  save_plot(p, file.path(out_dir, "fig_age_pyramid_fine.png"), width = 10, height = 6)
}

# age pyramid — fine neonatal split
if (all(c("patient_sex","age_days") %in% names(df))) {
  df_age <- df %>%
    mutate(
      age_years = age_days/365.25,
      age_band_fine = case_when(
        is.na(age_days)        ~ NA_character_,
        age_days <= 6          ~ "0–6d",
        age_days <= 28         ~ "7–28d",
        age_days <= 182        ~ "29–182d (≈0–6m)",
        age_days <= 365        ~ "183–365d (≈6–12m)",
        age_years > 20         ~ "20+",
        TRUE ~ paste0(floor(age_years), "–", floor(age_years)+1, "y")
      ),
      sex2 = if_else(patient_sex %in% c("Male","M","male"), "Male", "Female")
    ) %>%
    tidyr::drop_na(age_band_fine)
  
  d_pyr <- df_age %>% count(sex2, age_band_fine) %>%
    group_by(sex2) %>% mutate(n = if_else(sex2=="Male", -n, n)) %>% ungroup()
  
  p <- ggplot(d_pyr, aes(x = age_band_fine, y = n, fill = sex2)) +
    geom_col(width = 0.92) + coord_flip() +
    scale_fill_manual(values = c(Female = pal$orange, Male = pal$teal), name = "Sex") +
    scale_y_continuous(labels = abs) +
    labs(x = "Age (fine bands)", y = "Count (negative = Male)",
         title = "Age pyramid (fine neonatal split)")
  save_plot(p, file.path(out_dir, "fig_age_pyramid_fine.png"), width = 10, height = 6)
}

# --- mortality ---
# mortality by referral
if (all(c("mortality","patient_referred_ind") %in% names(df))) {
  p_mref <- df %>%
    filter(!is.na(mortality)) %>%
    mutate(ref = fct_na_value_to_level(as.factor(patient_referred_ind), "Unknown")) %>%
    group_by(ref) %>%
    summarise(mort = mean(mortality), n = n(), .groups="drop") %>%
    ggplot(aes(reorder(ref, mort), mort)) +
    geom_col(fill = pal$blue, alpha = 0.8) +
    geom_text(aes(label = percent(mort, 0.1)), vjust = -0.25, size = 3.3) +
    scale_y_continuous(labels = percent, limits = c(0, NA)) +
    labs(x = "referred?", y = "mortality", title = "mortality by referral (Unknown shown)") +
    theme(legend.position = "none")
  save_plot(p_mref, file.path(OUT_DIR, "fig_mortality_by_referral.png"), 7, 4.5)
}

# mortality by admission location
if (all(c("mortality","ad_location") %in% names(df))) {
  p_mloc <- df %>%
    filter(!is.na(mortality)) %>%
    mutate(ad_location = fct_na_value_to_level(as.factor(ad_location), "Unknown")) %>%
    group_by(ad_location) %>%
    summarise(mort = mean(mortality), n = n(), .groups="drop") %>%
    mutate(ad_location = fct_reorder(ad_location, mort)) %>%
    ggplot(aes(ad_location, mort)) +
    geom_col(fill = pal$orange, alpha = 0.85) +
    geom_text(aes(label = percent(mort, 0.1)), vjust = -0.25, size = 3.3) +
    scale_y_continuous(labels = percent, limits = c(0, NA)) +
    labs(x = "admission location", y = "mortality", title = "mortality by admission location") +
    theme(legend.position = "none")
  save_plot(p_mloc, file.path(OUT_DIR, "fig_mortality_by_adm_location.png"), 8, 4.8)
}

# mortality by age band
if (all(c("age_bin_adm","mortality") %in% names(df))) {
  p_mage <- df %>%
    filter(!is.na(age_bin_adm), !is.na(mortality)) %>%
    group_by(age_bin_adm) %>%
    summarise(mort = mean(mortality), n = n(), .groups="drop") %>%
    ggplot(aes(age_bin_adm, mort)) +
    geom_col(fill = pal$purple, alpha = 0.85) +
    geom_text(aes(label = percent(mort, 0.1)), vjust = -0.25, size = 3.2) +
    scale_y_continuous(labels = percent, limits = c(0, NA)) +
    labs(x = "age band (at admission)", y = "mortality", title = "mortality by age band") +
    theme(legend.position = "none")
  save_plot(p_mage, file.path(OUT_DIR, "fig_mortality_by_ageband.png"), 8, 4.8)
}


# outcome documentation (overall) + mortality by location with n labels
if (all(c("mortality","ad_location") %in% names(df))) {
  outc <- df %>% mutate(outcome = case_when(
    isTRUE(mortality) ~ "Died",
    isFALSE(mortality) ~ "Survived",
    TRUE ~ "Unknown"
  ))
  
  p1 <- outc %>% count(outcome) %>% mutate(pct = n/sum(n)) %>%
    ggplot(aes(outcome, pct, fill = outcome)) +
    geom_col(width = 0.8) +
    geom_text(aes(label = scales::percent(pct, 0.1)), vjust = -0.3, size = 4) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    labs(x = NULL, y = "Share", title = "Outcome documentation (overall)") +
    theme(legend.position = "none")
  save_plot(p1, file.path(out_dir, "fig_outcome_completeness.png"), width = 6, height = 4)
  
  p2 <- outc %>% filter(!is.na(ad_location)) %>%
    group_by(ad_location) %>%
    summarise(mort = mean(mortality, na.rm = TRUE), n = n(), .groups = "drop") %>%
    ggplot(aes(ad_location, mort)) +
    geom_col(width = 0.8, alpha = 0.9) +
    geom_text(aes(label = paste0(scales::percent(mort, 0.1), "\n(n=", n, ")")),
              vjust = -0.3, size = 3.7) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 0.45)) +
    labs(x = "Admission location", y = "Mortality",
         title = "Mortality by admission location")
  save_plot(p2, file.path(out_dir, "fig_mortality_by_location_with_n.png"), width = 7.5, height = 4.5)
}


