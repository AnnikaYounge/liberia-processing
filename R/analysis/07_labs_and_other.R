# 07_labs_and_other.R
# lab utilization and malaria positivity + malnutrition prevalence + sepsis

df <- readRDS(file.path(OUT_DIR, "df_core.rds"))

# lab utilization (admission order flags)
lab_flag_cols <- names(df)[str_detect(names(df), "^ordered_lab_.*_adm$")]
if (length(lab_flag_cols) > 0) {
  tab_lab_use <- df %>%
    summarise(across(all_of(lab_flag_cols), ~ mean(. %in% c(TRUE,1), na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "lab_flag", values_to = "utilization")
  write_csv(tab_lab_use, file.path(OUT_DIR, "overview_lab_utilization_adm.csv"))
}

# selected lab value distributions (first recorded)
lab_keep <- intersect(c("lab_hemoglobin_result_1","lab_wbc_result_1",
                        "lab_platelet_result_1","lab_creactiveprotein_result_1"),
                      names(df))
if (length(lab_keep) > 0) {
  lab_long <- df %>%
    select(all_of(lab_keep)) %>%
    tidyr::pivot_longer(everything(), names_to = "lab", values_to = "value") %>%
    filter(!is.na(value))
  
  if (nrow(lab_long) > 0) {
    p <- ggplot(lab_long, aes(value)) +
      geom_histogram(bins = 40, alpha = 0.85) +
      facet_wrap(~ lab, scales = "free_x") +
      labs(x = "Value", y = "Count", title = "Selected lab distributions")
    save_plot(p, file.path(out_dir, "fig_labs_distributions.png"), width = 10, height = 6)
  }
}

# malaria positivity (any RDT/smear result positive)
rt_res <- intersect(c("lab_malariardt_result_1","lab_malariardt_result_2","lab_malariardt_result_3"), names(df))
sm_res <- intersect(c("lab_malariasmear_result_1","lab_malariasmear_result_2","lab_malariasmear_result_3"), names(df))
if ((length(rt_res)+length(sm_res)) > 0) {
  is_pos <- function(x) tolower(as.character(x)) %in% c("positive","pos","1","true","t","yes","y")
  
  out <- tibble(type = character(), N = integer(), pos = integer(), pct = double())
  if (length(rt_res) > 0) {
    vals <- df %>% select(all_of(rt_res))
    N <- sum(rowSums(!is.na(vals)) > 0)
    pos <- sum(apply(vals, 1, function(r) any(is_pos(r), na.rm = TRUE)))
    out <- bind_rows(out, tibble(type="RDT", N=N, pos=pos, pct=pos/N))
  }
  if (length(sm_res) > 0) {
    vals <- df %>% select(all_of(sm_res))
    N <- sum(rowSums(!is.na(vals)) > 0)
    pos <- sum(apply(vals, 1, function(r) any(is_pos(r), na.rm = TRUE)))
    out <- bind_rows(out, tibble(type="Smear", N=N, pos=pos, pct=pos/N))
  }
  
  write_csv(out, file.path(OUT_DIR, "overview_malaria_positivity.csv"))
  
  p <- ggplot(out, aes(type, pct)) +
    geom_col(fill = "#fdd49e", color = "#fc8d59") +
    geom_text(aes(label = paste0(percent(pct, 0.1)," (", pos, "/", N, ")")), vjust = -0.25, size = 3.6) +
    scale_y_continuous(labels = percent, limits = c(0, max(out$pct)*1.15)) +
    labs(x = NULL, y = "positivity", title = "malaria test positivity")
  save_plot(p, file.path(OUT_DIR, "fig_malaria_positivity.png"), 6.4, 4)
}


# malnutrition prevalence (+mortality table)
mal_cols <- intersect(c("malnutrition_normal","malnutrition_mild","malnutrition_moderate",
                        "malnutrition_severe","malnutrition_underweight",
                        "malnutrition_wasted","malnutrition_kwashiorkor"), names(df))
if (length(mal_cols) > 0) {
  mal_long <- df %>%
    mutate(across(all_of(mal_cols), as_flag)) %>%
    select(all_of(c("mortality", mal_cols))) %>%
    pivot_longer(all_of(mal_cols), names_to = "mal_flag", values_to = "val") %>%
    filter(val == TRUE) %>%
    group_by(mal_flag) %>%
    summarise(n = n(), deaths = sum(mortality, na.rm = TRUE), mort = deaths / n, .groups = "drop") %>%
    arrange(desc(n))
  write_csv(mal_long, file.path(OUT_DIR, "overview_malnutrition.csv"))
  
  p_mal <- ggplot(mal_long, aes(reorder(mal_flag, n), n)) +
    geom_col(fill = "#74c476") +
    coord_flip() +
    labs(x = NULL, y = "count", title = "malnutrition flags — prevalence")
  save_plot(p_mal, file.path(OUT_DIR, "fig_malnutrition_prevalence.png"), 8, 5)
}

# neonatal sepsis bundle proxy (antibiotics, blood culture, fluids)
dx_any <- function(prefix) {
  greps <- names(df)[stringr::str_detect(names(df), paste0("^dx_\\d+_.*(", prefix, ").*_(adm|dis)$"))]
  if (length(greps) == 0) return(rep(FALSE, nrow(df)))
  rowSums(dplyr::select(df, dplyr::all_of(greps)) %>%
            dplyr::mutate(dplyr::across(everything(), ~ . %in% c(TRUE,1)))) > 0
}
has_neonatal_sepsis <- dx_any("neonatalsepsis")

if (any(has_neonatal_sepsis)) {
  bundle <- df %>%
    mutate(dx_neonatal_sepsis = has_neonatal_sepsis,
           abx   = "ordered_treatments_antibiotics" %in% names(df) & as_flag(ordered_treatments_antibiotics),
           blood = "ordered_lab_bloodculture_adm"   %in% names(df) & as_flag(ordered_lab_bloodculture_adm),
           fluid = "ordered_treatments_ivfluids"    %in% names(df) & as_flag(ordered_treatments_ivfluids)) %>%
    filter(dx_neonatal_sepsis) %>%
    summarise(n = n(),
              abx_rate = mean(abx, na.rm = TRUE),
              blood_cx_rate = mean(blood, na.rm = TRUE),
              fluids_rate = mean(fluid, na.rm = TRUE),
              full_bundle = mean(abx & blood & fluid, na.rm = TRUE))
  
  readr::write_csv(bundle, file.path(out_dir, "overview_sepsis_bundle_neonatal.csv"))
  
  p <- bundle %>%
    tidyr::pivot_longer(cols = everything(), names_to = "metric", values_to = "val") %>%
    filter(metric != "n") %>%
    ggplot(aes(reorder(metric, val), val)) +
    geom_col() +
    geom_text(aes(label = scales::percent(val, 0.1)), vjust = -0.3) +
    coord_flip() +
    scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
    labs(x = NULL, y = "Rate", title = "Neonatal sepsis — bundle proxy")
  save_plot(p, file.path(out_dir, "fig_sepsis_bundle_neonatal.png"), width = 7, height = 5)
}

