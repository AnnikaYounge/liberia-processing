# 00_setup.R
# install (if needed) and load the packages used by the pipeline

# r version check
req_version <- "4.1.0"
if (getRversion() < req_version) {
  stop(sprintf("this project requires R >= %s (you have %s)",
               req_version, getRversion()))
}

# packages used across the scripts
pkgs <- c(
  "dplyr", "tidyr", "readr", "readxl",
  "stringr", "lubridate",
  "ggplot2", "scales",
  "rlang", "tibble"
)

# install any missing packages, then load all
install_if_missing <- function(p) {
  if (!requireNamespace(p, quietly = TRUE)) install.packages(p)
  invisible(TRUE)
}

invisible(lapply(pkgs, install_if_missing))

loaded <- sapply(
  pkgs,
  function(p) suppressPackageStartupMessages(require(p, character.only = TRUE))
)

if (!all(loaded)) {
  stop(sprintf("could not load packages: %s", paste(pkgs[!loaded], collapse = ", ")))
}

message("packages installed and loaded")
