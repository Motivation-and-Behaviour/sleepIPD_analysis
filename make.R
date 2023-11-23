#!/usr/bin/env Rscript
# Easier running of the pipeline
library(targets)

# Start a UI to monitor pipeline
tar_watch(seconds = 45, targets_only = TRUE, outdated = TRUE, display = "graph")

# Run everything up to imputation checks sequentially
tar_make(c(imputation_checks, multiverse_file))

# Run everything else in parallel
# No advantage after 14 cores
tar_make_future(workers = min(parallel::detectCores(), 14))
