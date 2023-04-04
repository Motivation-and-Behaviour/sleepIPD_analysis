# Allow multible R sessions to be used
library(targets)
library(tarchetypes)
library(future)

plan(future.callr::callr)

# Load required functions and packages
lapply(list.files("./R", full.names = TRUE), source)
tar_option_set(packages = c("data.table", "magrittr", "readr"))

# Check if GCS is available to use
if (Sys.getenv("GCS_AUTH_FILE") != "") {
  tar_option_set(
    resources = tar_resources(
      gcp = tar_resources_gcp(bucket = "sleepipdtargets")
    )
  )

  format <- "qs"
  repository <- "gcp"
} else {
  format <- targets::tar_option_get("format")
  repository <- targets::tar_option_get("repository")
}

moderator_variables <- c(
  "age",
  "bmi",
  "ses",
  "sex",
  "weekday_x",
  "season",
  "region",
  "daylight_hours",
  "accelerometer_wear_location",
  "pa_mostactivehr"
)

# Pipeline
list(
  ##################################################################
  ##                           CLEANING                           ##
  ##################################################################
  tar_files_input(datasets, list.files("data", full.names = TRUE)),
  tar_target(data_raw,
    readr::read_csv(datasets,
      col_types = list(.default = "c"),
      id = "studyid"
    ),
    pattern = map(datasets), iteration = "list"
  ),

  # Data targets
  tar_target(data_joined, dplyr::bind_rows(data_raw), pattern = map(data_raw)),
  tar_target(data_clean, clean_data(data_joined, region_lookup)),
  tar_target(data_holdout, make_data_holdout(data_clean)),
  tar_target(participant_summary, make_participant_summary(data_clean)),
  tar_target(region_lookup, make_region_lookup()),
  tar_target(demog_table, make_demog_table(participant_summary)),
  tar_target(
    data_imp_raw, make_data_imp(data_holdout, n_imps = 3),
    deployment = "main",
    format = format, repository = repository
  ),
  tar_target(data_imp, format_imp_vars(data_imp_raw)),
  tar_target(data_imp_log, format_imp_vars(data_imp_raw, use_log = TRUE)),

  tar_target(imputation_checks, check_imps(data_imp),
    format = "file",
    priority = 1
  ),

  ##################################################################
  ##                        ASSET CREATION                        ##
  ##################################################################

  tar_target(explore_img, make_explore_img_list(data_holdout)),

  #################################################################
  ##                          MODELLING                          ##
  #################################################################

  target_factory(moderators = c(moderator_variables), data = "data_imp"),
  target_factory(moderators = moderator_variables, data = "data_imp_log")


)
