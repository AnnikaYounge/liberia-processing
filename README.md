# Liberia - Code for Data Processing

The repository contains a reproducible R pipeline for cleaning, matching, and validating pediatric hospital **admission** and **discharge** records in Liberia. The pipeline links admissions to discharges, resolves **duplicate** cases (readmissions, twins, transfers), and produces a single dataset.

**Overview:** Download the folder structure and files, open R, add the data into the data/raw folder locally, and run the main script create the final dataset.

## Quick Start

1.  **Install R**. Recommended: RStudio.
2.  **Download or clone** this repository.
3.  **Place raw input files** in `data/raw/`.
4.  From R/RStudio, **run**:

``` r
source("run_all.R")
% This will install/load packages (via `R/00_setup.R`), run the scripts in
order, and write outputs to `data/processed/`.
```

------------------------------------------------------------------------

## Input Data

-   Put the raw **admission** and **discharge** files in `data/raw/`.
-   The data can be downloaded from Kobo as any of the offered options, but I suggest .csv with labels for the download specifications. (XML or other file types also work)

------------------------------------------------------------------------

## Output Data

After running the pipeline, `data/processed/` will contain the following files: - `final_matched.csv` – the cleaned, combined dataset linking admissions and discharges. - `validation_sample_5pct.csv` – 5% random sample for manual spot checks.

------------------------------------------------------------------------

## Repository Structure

```         
R/
  00_setup.R                         # installs/loads required packages
  01_data_load.R                     # reads raw data, basic cleaning
  02_matching.R                      # unique-ID matching
  03_matching_nonunique_functions.R  # helper functions for complex cases
  04_matching_nonunique.R            # readmissions, twins, transfers handling
  05_validation.R                    # builds validation sample

data/
  raw/         # put the raw data files here from Kobo
  processed/   # will hold a .csv file of the final output

run_all.R       # runs the full pipeline
README.md
```

------------------------------------------------------------------------

## General overview

1.  **Data load** (`01_data_load.R`): Reads inputs, standardizes column types (based on the given excel formatting files), and cleans data
2.  **Unique matches** (`02_matching.R`): Links admissions and discharges where identifiers map 1:1
3.  **Duplicate helpers** (`03_matching_nonunique_functions.R`): Utilities for resolving ambiguous cases
4.  **Duplicate matches** (`04_matching_nonunique.R`): Handles readmissions, twins, and transfers; consolidates episodes
5.  **Validation** (`05_validation.R`): Creates a 5% sample for manual review

------------------------------------------------------------------------

## Usage Notes

-   You can re‑run the pipeline at any time; outputs will be overwritten.
-   If you maintain separate data uploads, keep them in dated subfolders under `data/raw/` and update paths in `01_data_load.R` accordingly.
-   For deterministic sampling in validation, a fixed seed is set inside `05_validation.R`. Change for different validation sets.

*Last updated: September 14, 2025*
