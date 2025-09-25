# 01_data_load.R
# load, standardize, and clean admission/discharge data and labels

# helpers
source("R/data_prep/data_cleaning/load_data.R")
source("R/data_prep/data_cleaning/conversions.R")
source("R/data_prep/data_cleaning/clean_issues.R")

# show extra messages  or not
verbose <- FALSE

# inputs (adjust if file names change)
adm_path <- "data/raw/AdmissionForm.csv"
adm_dict <- "data/formatting/admission_label_overview.xlsx"
dis_path <- "data/raw/DischargeForm.csv"
dis_dict <- "data/formatting/discharge_label_overview.xlsx"

stopifnot(file.exists(adm_path), file.exists(adm_dict),
          file.exists(dis_path), file.exists(dis_dict))


# ------- admission form

# load data and get dictionary
admission_tmp <- load_and_prepare_data(adm_path, adm_dict)
admission_data <- admission_tmp$data
admission_labels <- admission_tmp$labels
rm(admission_tmp)

# convert units as given by the label dictionary
admission_tmp <- convert_units(admission_data, admission_labels)
admission_data <- admission_tmp$data
admission_labels <- admission_tmp$labels
rm(admission_tmp)

# clean issues given the listed options in the dictionary
admission_data <- if (verbose) {
  clean_issues_verbose(admission_data, admission_labels)
} else {
  clean_issues(admission_data, admission_labels)
}

# normalize key types
admission_data <- admission_data |>
  dplyr::mutate(ad_date = as.Date(ad_date))


# ------- discharge form

# load data and get dictionary
discharge_tmp    <- load_and_prepare_data(dis_path, dis_dict)
discharge_data   <- discharge_tmp$data
discharge_labels <- discharge_tmp$labels
rm(discharge_tmp)

# convert units as given by the label dictionary
discharge_tmp    <- convert_units(discharge_data, discharge_labels)
discharge_data   <- discharge_tmp$data
discharge_labels <- discharge_tmp$labels
rm(discharge_tmp)

# clean issues given the listed options in the dictionary
discharge_data <- if (verbose) {
  clean_issues_verbose(discharge_data, discharge_labels)
} else {
  clean_issues(discharge_data, discharge_labels)
}

# normalize key types
discharge_data <- discharge_data |>
  dplyr::mutate(ad_date = as.Date(ad_date))


# ------- output

# check for issues
required_adm <- c("form_index", "ad_date")
required_dis <- c("form_index", "ad_date")
stopifnot(all(required_adm %in% names(admission_data)))
stopifnot(all(required_dis %in% names(discharge_data)))

message(sprintf("loaded: admissions %d rows, discharges %d rows",
                nrow(admission_data), nrow(discharge_data)))
