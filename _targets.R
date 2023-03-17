# Allow multible R sessions to be used
library(future)
plan("multisession")

# Load required functions and packages
lapply(list.files("./R", full.names = TRUE), source)
load_packages()

# Pipeline
list(
  tar_files_input(datasets, list.files("data", full.names = TRUE)),
  tar_target(data_raw,
    read_csv(datasets,
      col_types = list(.default = "c"),
      id = "studyid"
    ),
    pattern = map(datasets), iteration = "list"
  ),

  # Data targets
  tar_target(data_joined, bind_rows(data_raw), pattern = map(data_raw)),
  tar_target(data_clean, clean_data(data_joined, region_lookup)),
  tar_target(data_holdout, make_data_holdout(data_clean)),

  tar_target(participant_summary, make_participant_summary(data_clean)),
  tar_target(region_lookup, make_region_lookup()),
  tar_target(demog_table, make_demog_table(participant_summary)),
  tar_target(data_imp, make_data_imp(data_holdout, n_imps = 3)),

  # Modelling targets

  tar_target(
    model_list_by_age,
    make_model_list(data_imp, moderator = "age", moderator_term = "11, 18, 35, 65")
  ),
  tar_target(
    model_list_by_sex,
    make_model_list(
      data_imp,
      moderator = "sex",
      moderator_term = "all",
      control_vars = c("ses", "age", "bmi"))
    ),

  tar_target(model_diagnostics, make_model_diagnostics(model_list_by_age)),

  # Figures
  tar_target(explore_img, make_explore_img_list(data_holdout)),
  tar_target(
    purdy_pictures_by_age,
    produce_purdy_pictures(model_list_by_age,
                           paste_facet_labels = " years")
  ),
  tar_target(
    purdy_pictures_by_sex,
    produce_purdy_pictures(model_list_by_sex)
  ),

  # Tables
  tar_target(model_tables, make_model_tables(model_list_by_age)),

  # Output results section
  tar_render(manuscript, "doc/manuscript.Rmd", output_format = c(
    "papaja::apa6_docx",
    "papaja::apa6_pdf"
  )),

  ##################################################################
  ##                          Multiverse                          ##
  ##################################################################
  ### Using study id as a fixed effect

  ### Run models
  tar_target(
    model_list_by_age_fixedef,
    make_model_list(data_imp, moderator = "age", moderator_term = "11, 18, 35, 65",
                    ranef = "(1 | measurement_day) + (1 | participant_id)",
                    c("ses", "sex", "bmi", "studyid"))
  ),
  ### Produce tables
  tar_target(model_tables_fixedef, make_model_tables(model_list_by_age_fixedef)),

  ### Produce figures
  tar_target(
    purdy_pictures_by_age_fixedef,
    produce_purdy_pictures(model_list_by_age_fixedef,
                           paste_facet_labels = " years",
                           add_filename = "_fixedef")
  ),


  tar_render(multiverse, "doc/multiverse.Rmd", output_format = c(
    "papaja::apa6_pdf"
  ))
)
