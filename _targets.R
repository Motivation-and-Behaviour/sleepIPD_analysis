# Allow multible R sessions to be used
library(targets)
library(tarchetypes)

set.seed(42)

# Load required functions and packages
tar_option_set(
  packages = c("data.table", "magrittr", "readr"),
  controller = crew::crew_controller_local(
    workers = max(1, min(parallel::detectCores() - 4, 32)),
    seconds_idle = 15,
    garbage_collection = TRUE
  ),
  memory = "transient", # drops target values from the main process once done
  garbage_collection = 1,
  format = "qs"
)
tar_source()

# Check that the models won't die during a run
check_model_environment()

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
  # Recode tables, frozen from the Google Sheet. See R/refactors.R.
  tar_target(refactors_files, unname(refactor_files), format = "file"),
  tar_target(refactors, read_refactors(refactors_files)),
  # Data targets
  tar_target(data_joined, dplyr::bind_rows(data_raw)),
  tar_target(latlong_file, "data/latlong.csv", format = "file"),
  tar_target(
    data_clean,
    clean_data(data_joined, region_lookup, refactors, latlong_file)
  ),
  tar_target(
    data_holdout,
    make_data_holdout(data_clean)
  ),
  tar_target(participant_summary, make_participant_summary(data_clean)),
  tar_target(region_lookup, make_region_lookup()),
  tar_target(demog_table, make_demog_table(participant_summary)),
  tar_target(
    study_references_file,
    "data/study_references.csv",
    format = "file"
  ),
  tar_target(study_table, make_study_table(data_clean, study_references_file)),
  tar_target(ses_table, make_ses_table(refactors)),
  tar_target(
    bmi_z_ref,
    bmi_z_adult_ref(data_clean$bmi, data_clean$age, data_clean$participant_id)
  ),
  tar_target(
    data_imp,
    make_data_imp(data_clean, n_imps = 50, adult_ref = bmi_z_ref),
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
  tar_target(age_centre, compute_age_centre(data_clean)),
  tar_map(
    values = models_df,
    names = model_name,
    # One branch per model
    tar_target(
      model_specs,
      make_model_specs(moderator, sleep_vars, pa_vars, age_centre)
    ),
    tar_target(
      model_fits,
      fit_model_spec(
        data_imp,
        model_specs,
        moderator = moderator,
        moderator_term = mod_term,
        control_vars = cont_vars,
        ranef = ranef,
        min_wear_days = min_wear_days,
        age_centre = age_centre
      ),
      pattern = map(model_specs),
      iteration = "list"
    ),
    tar_target(
      model_list,
      assemble_model_list(
        model_fits,
        model_specs,
        data_imp,
        moderator = moderator,
        control_vars = cont_vars,
        sleep_vars = sleep_vars,
        pa_vars = pa_vars,
        min_wear_days = min_wear_days,
        age_centre = age_centre
      )
    ),
    tar_target(model_tables, make_model_tables(model_list)),
    tar_target(
      purdy_pictures,
      produce_purdy_pictures(model_list, model_tables)
    ),
    tar_target(
      purdy_picture_files,
      unname(unlist(purdy_pictures)),
      format = "file"
    )
  ),

  ##################################################################
  ##                        ASSET CREATION                        ##
  ##################################################################

  tar_target(explore_img, make_explore_img_list(data_holdout)),
  tar_target(explore_img_files, unname(unlist(explore_img)), format = "file"),
  # Output results section
  tar_target(
    ms_info,
    make_manuscript_info(
      data_clean,
      participant_summary,
      data_imp,
      model_list_by_age = model_list_by_age,
      model_list_by_age_7days = model_list_by_age_7days
    )
  ),
  tar_quarto(
    manuscript,
    "doc/manuscript.qmd",
    extra_files = c(
      "_quarto.yml",
      "_extensions",
      "doc/references.bib",
      "doc/vancouver-brackets-no-et-al.csl",
      "doc/reference.docx",
      "doc/typst-tables.typ",
      "doc/filters"
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
      model_list_by_age_allwear,
      model_list_by_age_7days,
      model_list_by_bmi,
      model_list_by_ses,
      model_list_by_weekday,
      model_list_by_season,
      model_list_by_region,
      model_list_by_daylight,
      model_list_by_wear_location,
      model_list_by_pa_mostactivehr,
      model_list_by_sex,
      model_list_by_ethnicity
    ),
    deployment = "main"
  ),
  tar_target(
    multiverse_skeleton,
    "doc/multiverse_skeleton.qmd",
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
  tar_quarto(
    multiverse,
    "doc/multiverse.qmd",
    extra_files = c("_quarto.yml", "doc/typst-tables.typ")
  )
)
