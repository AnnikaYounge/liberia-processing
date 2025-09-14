library(readr)
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(scales)
 
source("R/data_cleaning/load_data.R")
source("R/data_cleaning/conversions.R")
source("R/data_cleaning/clean_issues.R")

# display text or not
verbose <- FALSE

# load data
admission_tmp <- load_and_prepare_data("data/raw/AdmissionForm.csv", "data/formatting/admission_label_overview.xlsx")
admission_data <- admission_tmp$data
admission_labels <- admission_tmp$labels
rm(admission_tmp) # remove variable after unpacking into data and labels

discharge_tmp <- load_and_prepare_data("data/raw/DischargeForm.csv", "data/formatting/discharge_label_overview.xlsx")
discharge_data <- discharge_tmp$data
discharge_labels <- discharge_tmp$labels
rm(discharge_tmp)

# standardize quantities
admission_tmp <- convert_units(admission_data, admission_labels)
admission_data <- admission_tmp$data
admission_labels <- admission_tmp$labels
rm(admission_tmp)

discharge_tmp <- convert_units(discharge_data, discharge_labels)
discharge_data <- discharge_tmp$data
discharge_labels <- discharge_tmp$labels
rm(discharge_tmp)

# clean issues using data dictionary
if (verbose) {
  admission_data <- clean_issues_verbose(admission_data, admission_labels)
} else {
  admission_data <- clean_issues(admission_data, admission_labels)
}
if (verbose) {
  discharge_data <- clean_issues_verbose(discharge_data, discharge_labels)
} else {
  discharge_data <- clean_issues(discharge_data, discharge_labels)
}

