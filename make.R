#!/usr/bin/env Rscript
# Check for GCS
if (Sys.getenv("GCS_AUTH_FILE") != "") {
  usethis::ui_warn(
    c(
      "GCS_AUTH_FILE not set, using local storage.
      Please do not commit {usethis::ui_path('_targets/meta/meta')}."
    )
  )
}

# Easier running of the pipeline
library(targets)

# Start a UI to monitor pipeline
tar_watch(seconds = 45, targets_only = TRUE, outdated = TRUE, display = "graph")

# Run everything up to imputation checks sequentially
tar_make(c(imputation_checks, multiverse_file))

# Run everything else in parallel
# No advantage after 14 cores
tar_make_future(workers = min(parallel::detectCores(), 14))
