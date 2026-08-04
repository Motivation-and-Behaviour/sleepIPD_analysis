#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)

# Easier running of the pipeline
library(targets)

if ("--watch" %in% args) {
  # Start a UI to monitor pipeline
  tar_watch(
    seconds = 45,
    targets_only = TRUE,
    outdated = TRUE,
    display = "graph"
  )
}

if ("--manuscript-only" %in% args) {
  # Only update manuscript and direct dependencies
  targets::tar_make(c(references, manuscript), shortcut = TRUE)
  quit(status = 0)
}

if ("--parallel" %in% args) {
  # Run everything up to imputation checks sequentially
  tar_make(c(imputation_checks, multiverse_file))

  # Run everything else in parallel
  # No advantage after 14 cores
  tar_make(-manuscript, workers = min(parallel::detectCores(), 8))

  targets::tar_make(manuscript, shortcut = TRUE)
} else {
  # Run everything sequentially
  tar_make()
}
