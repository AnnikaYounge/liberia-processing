# run_analysis.R
# runs the analysis pipeline

# assumes we have final_matched via the cleaning/matching code

source("analysis/10_setup.R")
source("analysis/20_derive_core.R")

# main analysis modules
source("analysis/30_table1_overview.R")
source("analysis/40_time_and_outcomes.R")
source("analysis/50_readmissions.R")
source("analysis/60_diagnoses_complaints.R")
source("analysis/70_malnutrition_labs.R")
source("analysis/80_utilization.R")
source("analysis/90_geo_and_missing.R")

# done
message("analysis complete. outputs in: ", OUT_DIR)