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

# Model parameters
dflt_pa <- c("log_pa_volume", "scale_pa_intensity")
scale_pa <- c("scale_pa_volume", "scale_pa_intensity")
dflt_sleep <- c(
  "scale_sleep_duration", "scale_sleep_efficiency", "scale_sleep_onset",
  "scale_sleep_regularity"
)
dflt_ranef <- "(1|studyid) + (1|participant_id)"
fixedef <- "(1|measurement_day) + (1|participant_id)"
dflt_con <- c("ses", "age", "sex", "bmi")

# nolint start styler: off
models_df <- dplyr::tribble(
  ~model_name,          ~moderator,        ~mod_term,        ~mod_formal,        ~pa_vars, ~sleep_vars, ~ranef,     ~cont_vars,
  "by_age",             "age",             "11, 18, 35, 65", "age",              dflt_pa,  dflt_sleep,  dflt_ranef, dflt_con,
  "by_age_fixedef",     "age",             "11, 18, 35, 65", "age",              dflt_pa,  dflt_sleep,  fixedef,    c(dflt_con, "studyid"),
  "by_age_nolog",       "age",             "11, 18, 35, 65", "age",              scale_pa, dflt_sleep,  dflt_ranef, dflt_con,
  "by_bmi",             "bmi",             "18, 22, 25, 30", "BMI",              dflt_pa,  dflt_sleep,  dflt_ranef, dflt_con,
  "by_ses",             "ses",             "all",            "SES",              dflt_pa,  dflt_sleep,  dflt_ranef, dflt_con,
  "by_sex",             "sex",             "all",            "sex",              dflt_pa,  dflt_sleep,  dflt_ranef, dflt_con,
  "by_weekday",         "weekday",         "all",            "weekday",          dflt_pa,  dflt_sleep,  dflt_ranef, dflt_con,
  "by_season",          "season",          "all",            "season",           dflt_pa,  dflt_sleep,  dflt_ranef, dflt_con,
  "by_region",          "region",          "all",            "region",           dflt_pa,  dflt_sleep,  dflt_ranef, dflt_con,
  "by_daylight",        "daylight_hours",  "8, 10, 12, 14",  "daylight hours",   dflt_pa,  dflt_sleep,  dflt_ranef, dflt_con,
  "by_wear_location",   "acc_wear_loc",    "all",            "wear location",    dflt_pa,  dflt_sleep,  dflt_ranef, dflt_con,
  "by_pa_mostactivehr", "pa_mostactivehr", "5, 9, 14, 19",   "most active hour", dflt_pa,  dflt_sleep,  dflt_ranef, dflt_con
)
# nolint end styler: on

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
    data_imp, make_data_imp(data_holdout, n_imps = 3),
    deployment = "main",
    format = format, repository = repository
  ),
  tar_target(imputation_checks, check_imps(data_imp),
    format = "file",
    priority = 1
  ),

  #################################################################
  ##                          MODELLING                          ##
  #################################################################
  tar_map(
    values = models_df,
    names = model_name,
    tar_target(
      model_list,
      make_model_list(data_imp,
        moderator = moderator, moderator_term = mod_term, pa_vars = pa_vars,
        sleep_vars = sleep_vars, control_vars = cont_vars, ranef = ranef
      )
    ),
    tar_target(model_tables, make_model_tables(model_list)),
    tar_target(
      purdy_pictures,
      produce_purdy_pictures(model_list, model_tables)
    )
  ),

  ##################################################################
  ##                        ASSET CREATION                        ##
  ##################################################################

  tar_target(explore_img, make_explore_img_list(data_holdout)),
  # Output results section
  tar_render(manuscript, "doc/manuscript.Rmd", output_format = c(
    "papaja::apa6_docx",
    "papaja::apa6_pdf"
  )),

  #################################################################
  ##                   SUPPLEMENTARY MATERIALS                   ##
  #################################################################
  tar_target(
    model_diagnostics,
    make_model_diagnostics(
      model_list_by_age,
      model_list_by_age_fixedef,
      model_list_by_age_nolog,
      model_list_by_bmi,
      model_list_by_ses,
      model_list_by_weekday,
      model_list_by_season,
      model_list_by_region,
      model_list_by_daylight,
      model_list_by_wear_location,
      model_list_by_pa_mostactivehr
    ),
    deployment = "main"
  ),
  tar_target(multiverse_skeleton, "doc/multiverse_skeleton.Rmd",
    format = "file"
  ),
  tar_target(multiverse_chunk, "doc/results_chunk.md", format = "file"),
  tar_target(multiverse_file,
    make_multiverse_file(multiverse_skeleton, multiverse_chunk, models_df),
    format = "file"
  ),
  ### Produce supplementary material
  tar_render(multiverse, "doc/multiverse.Rmd", output_format = c(
    "papaja::apa6_pdf"
  ))
)
