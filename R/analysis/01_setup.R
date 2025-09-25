# 01_setup.R
# sets up packages, folders, theme, palette, and tiny helpers

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(stringr); library(lubridate)
  library(tidyr); library(forcats); library(ggplot2); library(purrr)
  library(scales); library(glue)
})

# where to write png/csv
OUT_DIR <- "R/analysis/analysis_outputs"
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

# seaborn-ish (based on seaborn in python) theme
theme_seabornish <- function(base_size = 12){
  theme_minimal(base_size = base_size) +
    theme(
      panel.grid.major = element_line(linewidth = 0.4, colour = "#E5E5E5"),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      axis.title = element_text(face = "bold"),
      plot.title = element_text(face = "bold", hjust = 0, margin = margin(b = 6)),
      legend.position = "right",
      strip.text = element_text(face = "bold")
    )
}
theme_set(theme_seabornish(12))

# muted, colorblind-friendly palette
pal <- list(
  blue = "#4C72B0",
  orange = "#DD8452",
  green = "#55A868",
  red = "#C44E52",
  purple = "#8172B2",
  brown = "#937860",
  teal = "#64B5CD",
  grey = "#8C8C8C"
)

# helpers -----

# save plots (ensures folder exists)
save_plot <- function(p, path, width = 8, height = 4.8, dpi = 220) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  ggsave(path, p, width = width, height = height, dpi = dpi)
}

# coerce various yes/true codes to logical TRUE/FALSE (else NA)
as_flag <- function(x) {
  if (is.logical(x)) return(x)
  if (is.numeric(x)) return(ifelse(is.na(x), NA, x > 0))
  if (inherits(x, "Date") || inherits(x, "POSIXt")) return(!is.na(x))
  if (is.factor(x)) x <- as.character(x)
  if (is.character(x)) {
    y <- tolower(trimws(x))
    out <- rep(NA, length(y))
    pos <- y %in% c("1","y","yes","true","t","present","positive","given","done","ordered")
    neg <- y %in% c("0","n","no","false","f","absent","none","not given","missing")
    out[pos] <- TRUE; out[neg] <- FALSE
    return(out)
  }
  rep(NA, length(x))
}

# age bands used across figures
age_bins <- function(days){
  brks <- c(-Inf, 29, 365, 5*365, 12*365, 18*365, Inf)
  cut(days, brks,
      labels = c("Neonate (<29d)","Infant (29d–1y)","1–4y","5–11y","12–17y","18+"),
      right = TRUE, include.lowest = TRUE)
}

tag_domain <- function(v, overrides = NULL){
  stopifnot(length(v) == 1L)
  v_raw <- v
  v <- tolower(v) |> trimws()
  
  if (!is.null(overrides) && v_raw %in% names(overrides)) {
    return(overrides[[v_raw]])
  }
  
  dplyr::case_when(
    # Admin/meta first (prevents collisions with "form_date_*" etc)
    str_detect(v, "^form_(id|uuid|uploadtime|version|index)$") ~ "form_meta",
    v %in% c("form_date_adm","form_date_dis")                  ~ "form_meta",
    
    # Core identity & demo
    v %in% c("patient_id","patient_id_twin")                   ~ "identifiers",
    v %in% c("patient_name_first","patient_name_last")         ~ "demographics",
    v %in% c("patient_birthdate","patient_sex")                ~ "demographics",
    
    # Dates / times
    v %in% c("ad_date","dc_date","ad_time")                    ~ "dates_times",
    
    # Locations (facility & patient geography)
    v %in% c("ad_location","discharge_location")               ~ "hospital_location",
    v %in% c("patient_town","patient_communityname")           ~ "geo_location",
    
    # Readmission sequencing keys
    str_starts(v, "readmit_seq")                               ~ "readmission",
    
    # Derived age & anthropometrics
    str_detect(v, "^patient_age_")                              ~ "derived_age",
    str_detect(v, "weight|height|muac|bmi|head\\s*circum|arm\\s*circum") ~ "anthropometry",
    
    # Triage / vitals / exam (broader net, but ordered before generic "ordered_")
    str_starts(v, "admission_") | str_starts(v, "physicalexam") ~ "adm_vitals_exam",
    str_detect(v, "^triage_|_triage$")                           ~ "adm_vitals_exam",
    
    # Clinical history & complaints
    str_detect(v, "^chief_") | str_detect(v, "chiefcomplaint|presenting_?complaint") ~ "chief_complaint",
    str_detect(v, "^history_|_history$|^hx_")                   ~ "history",
    
    # Diagnoses
    str_starts(v, "dx_")                                        ~ "diagnoses",
    
    # Orders / treatments / meds / allergies
    str_starts(v, "ordered_treatments") | str_starts(v, "antibiotics")               ~ "orders_treatments_flags",
    str_detect(v, "^meds?_") | str_detect(v, "medication")                            ~ "medications",
    str_detect(v, "allerg(y|ies)")                                                     ~ "allergies",
    
    # Orders / labs (flags vs results)
    str_starts(v, "ordered_lab")                                 ~ "orders_labs_flags",
    str_starts(v, "lab_")                                        ~ "lab_results",
    
    # Imaging / radiology orders
    str_starts(v, "ordered_radiology") |
      str_starts(v, "xray_") | str_starts(v, "ultrasound") |
      str_detect(v, "^ct(_|$)")                                  ~ "orders_radiology_flags",
    
    # Procedures & consults
    str_detect(v, "^procedure_|_procedure$")                     ~ "procedures",
    str_detect(v, "consult(_|$)|^consults?")                     ~ "consults",
    
    # Discharge & outcomes
    str_starts(v, "discharge_") |
      v %in% c("patient_disposition","discharge_death_date")     ~ "discharge_outcomes",
    
    TRUE ~ "other"
  )
}