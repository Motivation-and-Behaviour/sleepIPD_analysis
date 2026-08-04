# Allow multible R sessions to be used
library(targets)
library(tarchetypes)

set.seed(42)

# Load required functions and packages
tar_option_set(
  packages = c("data.table", "magrittr", "readr"),
  controller = crew::crew_controller_local(
    workers = max(1, min(parallel::detectCores() - 2, 20)),
    seconds_idle = 15
  ),
  format = "qs"
)
tar_source()

# Invalidate refactors target if sheet has been updated
if (tar_read(refactors_change) != sheet_last_modified()) {
  tar_invalidate(refactors)
}

# Pipeline
list(
  ##################################################################
  ##                           CLEANING                           ##
  ##################################################################
  tar_files_input(
    datasets,
    list.files("data", pattern = "\\d+_[A-Za-z]+\\.csv", full.names = TRUE)
  ),
  tar_target(
    data_raw,
    readr::read_csv(datasets, col_types = list(.default = "c"), id = "studyid"),
    pattern = map(datasets),
    iteration = "list"
  ),
  tar_target(refactors_change, sheet_last_modified()),
  tar_target(
    refactors,
    sapply(
      c("Sleep conditions", "Ethnicity", "SES"),
      sheet_read,
      simplify = FALSE,
      USE.NAMES = TRUE
    )
  ),
  # Data targets
  tar_target(
    data_joined,
    dplyr::bind_rows(data_raw),
    pattern = map(data_raw)
  ),
  tar_target(
    data_clean,
    clean_data(data_joined, region_lookup, refactors)
  ),
  tar_target(
    data_holdout,
    make_data_holdout(data_clean)
  ),
  tar_target(participant_summary, make_participant_summary(data_clean)),
  tar_target(region_lookup, make_region_lookup()),
  tar_target(demog_table, make_demog_table(participant_summary)),
  tar_target(
    data_imp,
    make_data_imp(data_clean, n_imps = 50),
    deployment = "main"
  ),
  tar_target(
    imputation_checks,
    check_imps(data_imp),
    format = "file"
  ),

  #################################################################
  ##                          MODELLING                          ##
  #################################################################
  tar_target(model_definitions, models_df),
  tar_map(
    values = models_df,
    names = model_name,
    tar_target(
      model_list,
      make_model_list(
        data_imp,
        moderator = moderator,
        moderator_term = mod_term,
        pa_vars = pa_vars,
        sleep_vars = sleep_vars,
        control_vars = cont_vars,
        ranef = ranef
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
  tar_target(ms_info, make_manuscript_info(data_clean, participant_summary)),
  tar_target(references, "doc/references.bib", format = "file"),
  tar_render(
    manuscript,
    "doc/manuscript.Rmd",
    output_format = c(
      "papaja::apa6_docx",
      "papaja::apa6_pdf",
      "md_document"
    )
  ),

  #################################################################
  ##                   SUPPLEMENTARY MATERIALS                   ##
  #################################################################
  tar_target(
    model_diagnostics,
    make_model_diagnostics(
      model_list_by_age,
      model_list_by_age_fixedef,
      model_list_by_age_log,
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
  tar_target(
    multiverse_skeleton,
    "doc/multiverse_skeleton.Rmd",
    format = "file"
  ),
  tar_target(multiverse_chunk, "doc/results_chunk.md", format = "file"),
  tar_target(
    multiverse_file,
    make_multiverse_file(
      multiverse_skeleton,
      multiverse_chunk,
      model_definitions
    ),
    format = "file"
  ),
  ### Produce supplementary material
  tar_render(
    multiverse,
    "doc/multiverse.Rmd",
    output_format = c(
      "papaja::apa6_pdf"
    )
  )
)
