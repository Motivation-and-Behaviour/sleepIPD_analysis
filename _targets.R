# Allow multible R sessions to be used
library(targets)
library(tarchetypes)

set.seed(42)

# Load required functions and packages
tar_option_set(
  packages = c("data.table", "magrittr", "readr"),
  controller = crew::crew_controller_local(
    workers = max(1, min(parallel::detectCores() - 2, 16)),
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
  tar_change(
    refactors,
    command = sapply(
      c("Sleep conditions", "Ethnicity", "SES"),
      sheet_read,
      simplify = FALSE,
      USE.NAMES = TRUE
    ),
    change = sheet_last_modified()
  ),
  # Data targets
  tar_target(data_joined, dplyr::bind_rows(data_raw)),
  # Static reference data (61 city coordinates), tracked so a change to it
  # invalidates data_clean. See E4 in CODE_REVIEW.md.
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
        ranef = ranef,
        min_wear_days = min_wear_days
      )
    ),
    tar_target(model_tables, make_model_tables(model_list)),
    tar_target(
      purdy_pictures,
      produce_purdy_pictures(model_list, model_tables)
    ),
    # purdy_pictures itself cannot be format = "file": it returns a *named*
    # list, the Rmds index it by name (purdy_pictures_by_bmi$predictor_pa_volume),
    # and format = "file" strips names off the value it returns — verified. So
    # the list target stays as the interface and this companion target gives
    # targets the paths to hash. A deleted or hand-edited figure now shows up as
    # an outdated target instead of silently vanishing from the render; it is
    # detection, not self-healing, so fix one with
    # tar_invalidate(purdy_pictures_<family>). See D11 in CODE_REVIEW.md.
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
  # See the purdy_picture_files note above.
  tar_target(explore_img_files, unname(unlist(explore_img)), format = "file"),
  # Output results section
  # data_imp is passed only for its `m`, so the manuscript's imputation count
  # comes from the run rather than a hardcoded number (C26).
  tar_target(
    ms_info,
    make_manuscript_info(data_clean, participant_summary, data_imp)
  ),
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
      model_list_by_age_allwear,
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
    "doc/multiverse_skeleton.Rmd",
    format = "file"
  ),
  tar_target(multiverse_chunk, "doc/results_chunk.md", format = "file"),
  # ⚠️ Adding a moderator to models_df needs TWO tar_make() runs before its
  # targets enter the graph. make_multiverse_file() writes doc/multiverse.Rmd,
  # but tar_render(multiverse, ...) below scans that file for tar_load() calls
  # at pipeline-construction time — i.e. before this run regenerates it. The
  # first run writes the new chunks; the second picks up their dependencies.
  # Note make_multiverse_file() also filters out age-moderated families, so
  # by_age_fixedef, by_age_log and by_age_allwear are hand-written sections in
  # multiverse_skeleton.Rmd. See D10 in CODE_REVIEW.md.
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
