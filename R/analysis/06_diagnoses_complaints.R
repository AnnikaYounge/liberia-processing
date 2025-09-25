# 06_diagnoses_complaints.R
# top diagnoses (volume + mortality) and complaint summaries (prevalence/mortality/LOS)

df <- readRDS(file.path(OUT_DIR, "df_core.rds"))

# diagnoses: top 15 by count + overlay mortality
diag_flags <- names(df)[str_detect(names(df), "^dx_\\d+_.+_(adm|dis)$") &
                          !str_detect(names(df), "other|describe")]
if (length(diag_flags) > 0 && "mortality" %in% names(df)) {
  dx_long <- df %>%
    select(all_of(c("mortality", diag_flags))) %>%
    pivot_longer(all_of(diag_flags), names_to = "dx_flag", values_to = "val") %>%
    mutate(val = as_flag(val)) %>%
    filter(val) %>%
    mutate(dx_name = dx_flag %>% str_replace("^dx_\\d+_", "") %>% str_replace("_(adm|dis)$","")) %>%
    group_by(dx_name) %>%
    summarise(n = n(), mort = mean(mortality, na.rm = TRUE), .groups="drop") %>%
    arrange(desc(n)) %>% slice_head(n = 15) %>%
    mutate(dx_name = fct_reorder(dx_name, n))
  
  p_dx <- ggplot(dx_long, aes(dx_name)) +
    geom_col(aes(y = n), fill = pal$green, alpha = 0.8) +
    geom_point(aes(y = mort * max(n)), colour = pal$red, size = 2.4) +
    scale_y_continuous(
      name = "count",
      sec.axis = sec_axis(~ . / max(dx_long$n), labels = percent, name = "mortality")
    ) +
    coord_flip() +
    labs(x = "diagnosis (any flag)",
         title = "top diagnoses — volume (bars) and mortality (points)")
  save_plot(p_dx, file.path(OUT_DIR, "fig_top15_dx_count_mortality.png"), 10, 7)
}

# complaints: map free text to short groups and summarise
complaint_cols <- names(df)[str_detect(names(df), "^complaint_\\d+_describe$")]
if (length(complaint_cols) > 0) {
  norm_txt <- function(x) {
    x %>% str_to_lower() %>% str_replace_all("[^a-z0-9\\s]", " ") %>% str_squish()
  }
  map_complaint <- function(x) {
    case_when(
      str_detect(x, "fever|pyrex")                     ~ "fever",
      str_detect(x, "cough")                           ~ "cough",
      str_detect(x, "breath|dyspn|respirat|fast breath|difficulty breath") ~ "breathing_difficulty",
      str_detect(x, "convuls|seizure|fit")             ~ "seizure",
      str_detect(x, "vomit|emesis")                    ~ "vomiting",
      str_detect(x, "diarr")                           ~ "diarrhea",
      str_detect(x, "jaund|yellow")                    ~ "jaundice",
      str_detect(x, "abdom|stomach pain|tummy pain")   ~ "abdominal_pain",
      str_detect(x, "malnut|kwashi|wast")              ~ "malnutrition",
      str_detect(x, "burn")                            ~ "burn",
      str_detect(x, "injur|trauma|fractur|cut")        ~ "injury",
      str_detect(x, "anemi")                           ~ "anemia",
      str_detect(x, "sepsis")                          ~ "sepsis",
      str_detect(x, "asphyx")                          ~ "birth_asphyxia",
      TRUE ~ "other"
    )
  }
  
  comp_long <- df %>%
    select(episode_key, mortality, los_days, age_days, any_of(c("illness_time_days", complaint_cols))) %>%
    pivot_longer(all_of(complaint_cols), names_to = "slot", values_to = "complaint") %>%
    mutate(complaint = norm_txt(complaint),
           complaint_group = map_complaint(complaint)) %>%
    filter(!is.na(complaint), complaint != "") %>%
    distinct(episode_key, complaint_group, .keep_all = TRUE)
  
  top_comp <- comp_long %>% count(complaint_group, sort = TRUE) %>% slice_head(n = 12) %>% pull(complaint_group)
  
  comp_summary <- comp_long %>%
    filter(complaint_group %in% top_comp) %>%
    group_by(complaint_group) %>%
    summarise(n = n(),
              mort = mean(mortality, na.rm = TRUE),
              med_los = median(los_days, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(desc(n)) %>%
    mutate(pct = n / sum(n))
  write_csv(comp_summary, file.path(OUT_DIR, "overview_complaints_summary.csv"))
  
  # prevalence
  p_prev <- ggplot(comp_summary, aes(y = reorder(complaint_group, n), x = pct)) +
    geom_col() +
    geom_text(aes(label = percent(pct, 0.1)), hjust = -0.1, size = 3.5) +
    scale_x_continuous(labels = percent, limits = c(0, max(comp_summary$pct) * 1.15)) +
    labs(x = "share of episodes", y = "complaint group", title = "chief complaints — prevalence (top 12)")
  save_plot(p_prev, file.path(OUT_DIR, "fig_complaints_prevalence.png"), 8, 5)
  
  # mortality
  p_mort <- ggplot(comp_summary, aes(y = reorder(complaint_group, mort), x = mort)) +
    geom_col() +
    geom_text(aes(label = percent(mort, 0.1)), hjust = -0.1, size = 3.5) +
    scale_x_continuous(labels = percent, limits = c(0, max(comp_summary$mort, na.rm = TRUE)*1.15)) +
    labs(x = "mortality", y = "complaint group", title = "mortality by chief complaint (top 12)")
  save_plot(p_mort, file.path(OUT_DIR, "fig_complaints_mortality.png"), 8, 5)
  
  # LOS
  p_los <- comp_long %>%
    filter(complaint_group %in% top_comp) %>%
    mutate(complaint_group = fct_reorder(complaint_group, los_days, .fun = median, .desc = TRUE)) %>%
    ggplot(aes(complaint_group, los_days)) +
    geom_violin(fill = NA, color = "grey60") +
    geom_boxplot(width = 0.2, outlier.size = 0.7) +
    coord_flip() +
    labs(x = "complaint group", y = "LOS (days)", title = "length of stay by chief complaint (top 12)")
  save_plot(p_los, file.path(OUT_DIR, "fig_complaints_los.png"), 8, 6)
}

# chief complaint co-occurrence heatmap
complaint_cols <- names(df)[stringr::str_detect(names(df), "^complaint_\\d+_describe$")]
if (length(complaint_cols) > 0) {
  norm_txt <- function(x) x %>% stringr::str_to_lower() %>% stringr::str_replace_all("[^a-z0-9\\s]", " ") %>% stringr::str_squish()
  map_group <- function(x) dplyr::case_when(
    stringr::str_detect(x, "fever") ~ "fever",
    stringr::str_detect(x, "cough") ~ "cough",
    stringr::str_detect(x, "breath|dyspn|respirat") ~ "breathing_difficulty",
    stringr::str_detect(x, "seiz|convuls") ~ "seizure",
    stringr::str_detect(x, "vomit") ~ "vomiting",
    stringr::str_detect(x, "diarr") ~ "diarrhea",
    TRUE ~ "other"
  )
  
  comp_wide <- df %>%
    select(episode_key, all_of(complaint_cols)) %>%
    tidyr::pivot_longer(-episode_key, names_to = "slot", values_to = "complaint") %>%
    filter(!is.na(complaint), complaint != "") %>%
    mutate(group = map_group(norm_txt(complaint))) %>%
    distinct(episode_key, group) %>%
    mutate(flag = 1L) %>%
    tidyr::pivot_wider(id_cols = episode_key, names_from = group, values_from = flag, values_fill = 0)
  
  M <- as.matrix(select(comp_wide, -episode_key))
  co <- t(M) %*% M; diag(co) <- 0
  co_df <- as.data.frame(as.table(co)); names(co_df) <- c("complaint1","complaint2","count")
  co_df <- dplyr::filter(co_df, count > 0)
  
  p <- ggplot(co_df, aes(complaint1, complaint2, fill = count)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(trans = "log1p") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = NULL, y = NULL, fill = "Co-occurrence", title = "Chief complaint co-occurrence")
  save_plot(p, file.path(out_dir, "fig_complaints_cooccurrence.png"), width = 8, height = 7)
}

# complaint vs diagnosis heatmap (top 12 x top 12)
dx_flags <- names(df)[stringr::str_detect(names(df), "^dx_\\d+_.+_(adm|dis)$") &
                        !stringr::str_detect(names(df), "other|describe")]
if (length(complaint_cols) > 0 && length(dx_flags) > 0) {
  norm_complaint <- function(x){
    x <- tolower(trimws(x))
    dplyr::case_when(
      stringr::str_detect(x, "fev") ~ "fever",
      stringr::str_detect(x, "cough") ~ "cough",
      stringr::str_detect(x, "breath|dyspn|respirat") ~ "breathing_difficulty",
      stringr::str_detect(x, "vomit") ~ "vomiting",
      stringr::str_detect(x, "diarr") ~ "diarrhea",
      stringr::str_detect(x, "seiz") ~ "seizure",
      TRUE ~ "other"
    )
  }
  
  comp_long <- df %>%
    mutate(.row = dplyr::row_number()) %>%
    select(.row, all_of(complaint_cols)) %>%
    tidyr::pivot_longer(-.row, names_to = "slot", values_to = "complaint") %>%
    filter(!is.na(complaint), complaint != "") %>%
    mutate(complaint_norm = norm_complaint(complaint)) %>%
    distinct(.row, complaint_norm)
  
  dx_long <- df %>%
    mutate(.row = dplyr::row_number()) %>%
    select(.row, all_of(dx_flags)) %>%
    tidyr::pivot_longer(-.row, names_to = "dx_flag", values_to = "val") %>%
    filter(val %in% c(TRUE, 1, "Yes", "YES", "yes")) %>%
    mutate(dx_name = dx_flag %>% stringr::str_remove("^dx_\\d+_") %>% stringr::str_remove("_(adm|dis)$")) %>%
    distinct(.row, dx_name)
  
  top_comp <- comp_long %>% count(complaint_norm, sort = TRUE) %>% slice_head(n = 12) %>% pull(complaint_norm)
  top_dx   <- dx_long %>% count(dx_name, sort = TRUE) %>% slice_head(n = 12) %>% pull(dx_name)
  
  cross <- comp_long %>%
    filter(complaint_norm %in% top_comp) %>%
    inner_join(dx_long %>% filter(dx_name %in% top_dx), by = ".row") %>%
    count(complaint_norm, dx_name, name = "n") %>%
    group_by(complaint_norm) %>% mutate(pct_with_dx = n / sum(n)) %>% ungroup()
  
  readr::write_csv(cross, file.path(out_dir, "complaint_vs_dx_top.csv"))
  
  p <- ggplot(cross, aes(complaint_norm, dx_name, fill = pct_with_dx)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(labels = scales::percent, option = "plasma") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = "Complaint (top 12)", y = "Diagnosis (top 12)",
         fill = "% within complaint",
         title = "Complaint → diagnosis mapping")
  save_plot(p, file.path(out_dir, "fig_complaint_vs_dx.png"), width = 9, height = 6)
}

