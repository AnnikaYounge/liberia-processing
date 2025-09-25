# 09_geo_and_missing.R
# simple geography (top towns/communities) + missingness by domain

df <- readRDS(file.path(OUT_DIR, "df_core.rds"))
var_domains <- readRDS(file.path(OUT_DIR, "var_domains.rds"))

# top towns / communities
if ("patient_town" %in% names(df)) {
  write_csv(
    df %>% count(patient_town, name = "n") %>% arrange(desc(n)) %>% slice_head(n = 20),
    file.path(OUT_DIR, "overview_top20_towns.csv")
  )
}
if ("patient_communityname" %in% names(df)) {
  write_csv(
    df %>% count(patient_communityname, name = "n") %>% arrange(desc(n)) %>% slice_head(n = 20),
    file.path(OUT_DIR, "overview_top20_communities.csv")
  )
}

# missing by variable domain
pct_nonmissing <- map_dbl(names(df), ~ mean(!is.na(df[[.x]])))
tab_missing <- tibble(variable = names(df), pct_nonmissing = pct_nonmissing) %>%
  left_join(var_domains, by = "variable") %>%
  group_by(domain) %>%
  summarise(mean_nonmissing = mean(pct_nonmissing, na.rm = TRUE),
            median_nonmissing = median(pct_nonmissing, na.rm = TRUE),
            n_vars = n(), .groups = "drop")
write_csv(tab_missing, file.path(OUT_DIR, "overview_missingness_by_domain.csv"))

