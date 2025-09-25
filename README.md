# Liberia – code for data processing & analysis

The repository contains a reproducible R pipeline for cleaning, matching, and validating pediatric hospital **admission** and **discharge** records in Liberia. The pipeline links admissions to discharges, resolves **duplicate** cases (readmissions, twins, transfers), and produces a single dataset. The analysis code then provides starting tables and figures.

---

## quick start

1. **install packages**

   ```r
   source("R/00_setup.R")
   ```

2. **prepare the dataset (cleaning/matching)**

   ```r
   source("run_all.R")
   ```

   writes:

   * `data/processed/final_matched.csv`
   * `data/processed/unmatched_admissions.csv`
   * `data/processed/unmatched_discharges.csv`
   * `data/processed/validation_sample_5pct.csv`

3. **run the analysis (tables + figures)**

   ```r
   source("analysis/run_analysis.R")
   ```

   outputs go to `analysis_outputs/`.

---

## input data

Place the raw **admission** and **discharge** files in `data/raw/`.

* Recommended names: `AdmissionForm.csv`, `DischargeForm.csv`.
* From Kobo, use CSV with “_labels” (recommended). XML and excel also works.

---

## repository structure

```
R/                               # cleaning + matching
  00_setup.R                     # installs/loads packages
  01_data_load.R                 # read raw data, standardize, basic cleaning
  02_matching.R                  # unique-ID matching
  03_matching_nonunique_functions.R  # helpers for complex cases
  04_matching_nonunique.R        # readmissions, twins, transfers; final joins
  05_validation.R                # builds validation sample

analysis/                        # analysis (tables + figures)
  10_setup.R
  20_derive_core.R
  30_table1_overview.R
  40_time_and_outcomes.R
  50_readmissions.R
  60_diagnoses_complaints.R
  70_malnutrition_labs.R
  80_utilization.R
  90_geo_and_missing.R
  run_analysis.R
  analysis_outputs/              # PNGs + CSVs from the analysis

data/
  raw/         # put the raw data files here (not versioned)
  processed/   # final outputs from the pipeline
  formatting/  # data dictionaries / label/type specifications


run_all.R
README.md
```

---

## data processing overview

1. **data load** (`01_data_load.R`) — reads inputs, standardizes types using the
   excel formatting files, and cleans fields
2. **unique matches** (`02_matching.R`) — links admissions to discharges where
   the patient id is 1:1, and includes matching across misentered patient ids
3. **duplicate helpers** (`03_matching_nonunique_functions.R`) — utilities for
   ambiguous cases
4. **duplicate matches** (`04_matching_nonunique.R`) — handles readmissions,
   twins, and transfers; consolidates episodes, and links the admission and discharge information
5. **validation** (`05_validation.R`) — creates a 5% sample for manual review

---

## analysis overview
### key derived fields for analysis

* `patient_id_final` — twin-aware ID (`patient_id_twin` if present, else `patient_id`)
* `episode_key` — unique episode key (e.g., `patient_id_final | ad_date`)
* `age_days`, `age_months`, `age_bin_adm` — age measures at admission
* `los_days` — length of stay (`dc_date - ad_date`)
* `mortality` — `TRUE` if death recorded at discharge or death date present
* `patient_referred_ind` — referral indicator (if available)

---

### analysis outputs (selected)

**table 1 / overview CSVs**

* `table1_counts.csv`, `table1_age_summary.csv`, `table1_age_bins.csv`
* `table1_sex.csv`, `table1_referral.csv`, `table1_adm_location.csv`
* `table1_outcomes.csv`

**time + outcomes**

* `fig_monthly_admissions.png`
* `fig_los_overall_hist.png`, `fig_los_overall_ecdf.png`
* `fig_los_by_location.png`, `fig_los_by_location_median.png`
* `fig_mortality_by_referral.png`, `fig_mortality_by_adm_location.png`, `fig_mortality_by_ageband.png`
* `fig_outcome_completeness.png`, `fig_mortality_by_location_with_n.png`

**readmissions**

* `overview_readmit_30d.csv`, `fig_readmission_30d_ep_vs_patient.png`
* `overview_readmissions_rowwise.csv`, `overview_readmissions_summary.csv`, `fig_readmissions_summary.png`

**diagnoses & complaints**

* `fig_top15_dx_count_mortality.png`
* `overview_complaints_summary.csv`, `fig_complaints_prevalence.png`, `fig_complaints_mortality.png`,
  `fig_complaints_los.png`, `fig_complaints_cooccurrence.png`
* `complaint_vs_dx_top.csv`, `fig_complaint_vs_dx.png`

**labs, malaria, malnutrition**

* `overview_lab_utilization_adm.csv`, `fig_labs_distributions.png`
* `overview_malaria_positivity.csv`, `fig_malaria_positivity.png`
* `overview_malnutrition.csv`, `fig_malnutrition_prevalence.png`
* `overview_sepsis_bundle_neonatal.csv`, `fig_sepsis_bundle_neonatal.png`

**utilization**

* `overview_therapies.csv`, `fig_therapies_prevalence.png`
* `overview_utilization_prevalence.csv` (radiology + consult flags)

**geography + missingness**

* `overview_top20_towns.csv`, `overview_top20_communities.csv`
* `overview_missingness_by_domain.csv`

---

## usage notes

* You can re-run the pipeline any time; outputs are overwritten.
* If you maintain separate raw drops, keep them in dated subfolders under
  `data/raw/` and update paths in `R/01_data_load.R`.
* Some figures may be blank if a site doesn’t collect certain variables;
  check the corresponding CSV to confirm counts.

---

## data protection

The processed dataset includes sensitive fields (names, hospital ids, medical data). Keep the
repository private and share outputs only with authorized collaborators.
---

*Last updated: September 25, 2025*

---
