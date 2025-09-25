# 08_utilization.R
# therapies, radiology, and consult utilization summaries

df <- readRDS(file.path(OUT_DIR, "df_core.rds"))

# therapies prevalence
tx_cols <- c(
  "oxygentx_ind",
  "bloodtransfusion_ind",
  "exchangetransfusion_ind",
  "malnutritiontx_ind",
  "nebulizationtx",
  "ordered_treatments_antibiotics",
  "ordered_treatments_ivfluids",
  "ordered_treatments_antimalarial",
  "ordered_treatments_feverorpain",
  "ordered_treatments_antiseizure",
  "ordered_treatments_resuscitation"
)
existing_tx <- intersect(tx_cols, names(df))
existing_tx <- existing_tx[vapply(df[existing_tx], function(z) is.atomic(z) || is.factor(z), logical(1))]

if (length(existing_tx) > 0) {
  df_tx <- df %>% mutate(across(all_of(existing_tx), as_flag))
  tab_tx <- df_tx %>%
    summarise(across(all_of(existing_tx), ~ mean(.x, na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "therapy", values_to = "prevalence") %>%
    arrange(desc(prevalence))
  write_csv(tab_tx, file.path(OUT_DIR, "overview_therapies.csv"))
  
  p <- ggplot(tab_tx, aes(reorder(therapy, prevalence), prevalence)) +
    geom_col(fill = pal$green, alpha = 0.85) +
    coord_flip() +
    scale_y_continuous(labels = percent) +
    geom_text(aes(label = percent(prevalence, 0.1)), hjust = -0.05, size = 3.5) +
    labs(x = NULL, y = "prevalence", title = "inpatient therapies — prevalence") +
    theme(plot.margin = margin(r = 30))
  save_plot(p, file.path(OUT_DIR, "fig_therapies_prevalence.png"), 7, 5)
}

util_flags <- c(
  "ordered_radiology",
  "ordered_radiology_x-ray",
  "ordered_radiology_ultrasound",
  "ordered_radiology_ct_scan",
  "ordered_radiology_mri",
  "ordered_consultation_ind"
)
util_flags <- util_flags[vapply(df[util_flags], function(z) is.atomic(z) || is.factor(z), logical(1))]

if (length(util_flags) > 0) {
  df_util <- df %>% mutate(across(all_of(util_flags), as_flag))
  util_prev <- df_util %>%
    summarise(
      across(all_of(util_flags),
             list(N = ~ sum(!is.na(.)), yes = ~ sum(., na.rm = TRUE), prev = ~ mean(., na.rm = TRUE)),
             .names = "{.col}.{.fn}")
    ) %>%
    pivot_longer(everything(), names_to = c("variable",".value"), names_pattern = "^(.*)\\.(N|yes|prev)$") %>%
    arrange(desc(prev))
  write_csv(util_prev, file.path(OUT_DIR, "overview_utilization_prevalence.csv"))
}
