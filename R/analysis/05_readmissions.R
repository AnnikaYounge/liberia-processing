# 05_readmissions.R
# 30-day readmissions (episode + patient views) + summary figure

df <- readRDS(file.path(OUT_DIR, "df_core.rds"))

if (all(c("patient_id_final","ad_date") %in% names(df))) {
  readmit_calc <- df %>%
    arrange(patient_id_final, ad_date) %>%
    group_by(patient_id_final) %>%
    mutate(next_ad   = lead(ad_date),
           days_next = as.numeric(next_ad - ad_date),
           readmit_30d = !is.na(days_next) & days_next <= 30) %>%
    ungroup()
  
  # episode-level: share of index admissions with a ≤30d readmit
  ep_rate <- mean(readmit_calc$readmit_30d, na.rm = TRUE)
  
  # patient-level: share of patients with any <=30d readmit
  pt_rate <- readmit_calc %>%
    group_by(patient_id_final) %>%
    summarise(any30 = any(readmit_30d, na.rm = TRUE), .groups="drop") %>%
    summarise(rate = mean(any30)) %>% pull(rate)
  
  write_csv(readmit_calc %>% select(patient_id_final, ad_date, next_ad, days_next, readmit_30d),
            file.path(OUT_DIR, "overview_readmit_30d.csv"))
  
  rates <- tibble(
    metric = c("index admissions with <=30d readmit","patients with any ≤30d readmit"),
    rate   = c(ep_rate, pt_rate)
  )
  p <- ggplot(rates, aes(metric, rate)) +
    geom_col(width = 0.65) +
    geom_text(aes(label = percent(rate, 0.1)), vjust = -0.2) +
    scale_y_continuous(labels = percent, limits = c(0, max(rates$rate, na.rm=TRUE)*1.15),
                       expand = expansion(mult = c(0, .02))) +
    labs(x = NULL, y = "rate", title = "30-day readmission (episode vs patient level)")
  save_plot(p, file.path(OUT_DIR, "fig_readmission_30d_ep_vs_patient.png"), 8, 5)
}

# readmission sequence summary + rowwise days since previous
if (all(c("patient_id_final","ad_date","dc_date") %in% names(df))) {
  seq_df <- df %>%
    arrange(patient_id_final, ad_date) %>%
    group_by(patient_id_final) %>%
    mutate(prev_dc = lag(dc_date),
           days_since_prev = as.integer(ad_date - prev_dc),
           readmit_30 = !is.na(days_since_prev) & days_since_prev >= 0 & days_since_prev <= 30) %>%
    ungroup()
  
  readr::write_csv(
    seq_df %>% select(patient_id_final, ad_date, dc_date, days_since_prev, readmit_30),
    file.path(out_dir, "overview_readmissions_rowwise.csv")
  )
  
  d_readm <- seq_df %>%
    mutate(readmit_seq = dplyr::row_number(), .by = patient_id_final) %>%
    summarise(
      zero    = sum(readmit_seq == 1, na.rm = TRUE),
      one     = sum(readmit_seq == 2, na.rm = TRUE),
      two_plus= sum(readmit_seq >= 3, na.rm = TRUE),
      .by = patient_id_final
    ) %>%
    summarise(
      zero = sum(zero), one = sum(one), two_plus = sum(two_plus)
    ) %>%
    tidyr::pivot_longer(everything(), names_to = "group", values_to = "n") %>%
    mutate(pct = n / sum(n))
  
  readr::write_csv(d_readm, file.path(out_dir, "overview_readmissions_summary.csv"))
  
  p <- ggplot(d_readm, aes(group, pct)) +
    geom_col(fill = "#80b1d3") +
    geom_text(aes(label = scales::percent(pct, 0.1)), vjust = -0.25, size = 3.6) +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Episode sequence group", y = "Share of episodes",
         title = "Readmissions (episode sequence)")
  save_plot(p, file.path(out_dir, "fig_readmissions_summary.png"), width = 7, height = 4)
}


