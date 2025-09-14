# run_all.R

message("Running 00_setup.R (packages)...")
source(file.path("R", "00_setup.R"))

scripts <- c(
  "01_data_load.R",
  "02_matching.R",
  "03_matching_nonunique_functions.R",
  "04_matching_nonunique.R",
  "05_validation.R"
)

for (s in scripts) {
  p <- file.path("R", s)
  message(sprintf("Sourcing %s ...", p))
  source(p, echo = TRUE, max.deparse.length = Inf)
}
message("All steps completed.")
