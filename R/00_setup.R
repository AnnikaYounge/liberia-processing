# 00_setup.R
# Installs (if needed) and loads the packages required by the pipeline.

# --- Minimal R version check ---
req_version <- "4.1.0"
if (getRversion() < req_version) {
  stop(sprintf("This project requires R >= %s (you have %s).",
               req_version, getRversion()))
}

# --- Packages actually used across the scripts ---
pkgs <- c(
  "dplyr", "tidyr", "readr", "readxl",
  "stringr", "lubridate",
  "ggplot2", "scales",
  "rlang", "tibble"
)

install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) {
    message(sprintf("Installing '%s' from CRAN...", p))
    install.packages(p, repos = "https://cloud.r-project.org")
  }
  invisible(TRUE)
}

# Install then load
invisible(vapply(pkgs, install_if_missing, logical(1)))
invisible(lapply(pkgs, library, character.only = TRUE))

message("All required packages are installed and loaded.")
