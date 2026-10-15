#!/usr/bin/env Rscript
args <- commandArgs(trailingOnly = TRUE)

# Easier running of the pipeline
library(targets)

if ("--watch" %in% args) {
  # Start a UI to monitor the pipeline. This launches a Shiny app and blocks
  # until the app is closed, so it is a standalone command: run it in a second
  # session alongside a running `make.R`, not as a prefix to one.
  tar_watch(
    seconds = 45,
    targets_only = TRUE,
    outdated = TRUE,
    display = "graph"
  )
  quit(status = 0)
}

if ("--manuscript-only" %in% args) {
  # Only update manuscript and direct dependencies
  targets::tar_make(c(references, manuscript), shortcut = TRUE)
  quit(status = 0)
}

if ("--parallel" %in% args) {
  # Worker count is not set here: parallelism comes from the crew controller in
  # `_targets.R` (`tar_make()` has no `workers` argument). This branch only
  # stages the build so the cheap data/imputation targets finish before the
  # expensive model targets fan out.
  tar_make(c(imputation_checks, multiverse_file))

  tar_make(!manuscript)

  targets::tar_make(manuscript, shortcut = TRUE)
} else {
  # Run everything sequentially
  tar_make()
}
