# Allow multible R sessions to be used
library(future)
plan("multisession")

# Load required functions and packages
lapply(list.files("./R", full.names = TRUE), source)
load_packages()

# Pipeline
list(
  ##################################################################
  ##                           CLEANING                           ##
  ##################################################################
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

  #################################################################
  ##                          MODELLING                          ##
  #################################################################

  tar_target(
    model_list_by_age,
    make_model_list(data_imp, moderator = "age", moderator_term = "11, 18, 35, 65")
  ),
  tar_target(
    model_list_by_bmi,
    make_model_list(
      data_imp,
      moderator = "bmi",
      moderator_term = "18, 22, 25, 30",
      control_vars = c("ses", "age", "sex"))
  ),
  tar_target(
    model_list_by_ses,
    make_model_list(
      data_imp,
      moderator = "ses",
      moderator_term = "all",
      control_vars = c("bmi", "age", "sex"))
  ),
  tar_target(
    model_list_by_sex,
    make_model_list(
      data_imp,
      moderator = "sex",
      moderator_term = "all",
      control_vars = c("ses", "age", "bmi"))
  ),
  tar_target(
    model_list_by_weekday,
    make_model_list(
      data_imp,
      moderator = "weekday_x",
      moderator_term = "all",
      control_vars = c("bmi", "age", "sex"))
  ),
  tar_target(
    model_list_by_season,
    make_model_list(
      data_imp,
      moderator = "season",
      moderator_term = "all",
      control_vars = c("bmi", "age", "sex"))
  ),
  tar_target(
    model_list_by_region,
    make_model_list(
      data_imp,
      moderator = "region",
      moderator_term = "all",
      control_vars = c("ses", "age", "sex"))
  ),
  tar_target(
    model_list_by_daylight,
    make_model_list(
      data_imp,
      moderator = "daylight_hours",
      moderator_term = "8, 10, 12, 14",
      control_vars = c("bmi", "age", "sex"))
  ),
  tar_target(
    model_list_by_wear_location,
    make_model_list(
      data_imp,
      moderator = "accelerometer_wear_location",
      moderator_term = "all",
      control_vars = c("bmi", "age", "sex"))
  ),

  ##################################################################
  ##                        ASSET CREATION                        ##
  ##################################################################

  tar_target(explore_img, make_explore_img_list(data_holdout)),
  tar_target(
    purdy_pictures_by_age,
    produce_purdy_pictures(model_list_by_age,
                           paste_facet_labels = " years")
  ),
  tar_target(
    purdy_pictures_by_bmi,
    produce_purdy_pictures(model_list_by_bmi)
  ),
  tar_target(
    purdy_pictures_by_ses,
    produce_purdy_pictures(model_list_by_ses)
  ),
  tar_target(
    purdy_pictures_by_sex,
    produce_purdy_pictures(model_list_by_sex)
  ),
  tar_target(
    purdy_pictures_by_weekday,
    produce_purdy_pictures(model_list_by_weekday)
  ),
  tar_target(
    purdy_pictures_by_season,
    produce_purdy_pictures(model_list_by_season)
  ),
  tar_target(
    purdy_pictures_by_region,
    produce_purdy_pictures(model_list_by_region)
  ),
  tar_target(
    purdy_pictures_by_daylight,
    produce_purdy_pictures(model_list_by_daylight, paste_facet_labels = " hours")
  ),
  tar_target(
    purdy_pictures_by_wear_location,
    produce_purdy_pictures(model_list_by_wear_location)
  ),

  # Tables
  tar_target(model_tables_age, make_model_tables(model_list_by_age)),
  tar_target(model_tables_bmi, make_model_tables(model_list_by_bmi)),
  tar_target(model_tables_ses, make_model_tables(model_list_by_ses)),
  tar_target(model_tables_sex, make_model_tables(model_list_by_sex)),
  tar_target(model_tables_weekday, make_model_tables(model_list_by_weekday)),
  tar_target(model_tables_season, make_model_tables(model_list_by_season)),
  tar_target(model_tables_region, make_model_tables(model_list_by_region)),
  tar_target(model_tables_daylight, make_model_tables(model_list_by_daylight)),
  tar_target(model_tables_wear_location, make_model_tables(model_list_by_wear_location)),

  # Output results section
  tar_render(manuscript, "doc/manuscript.Rmd", output_format = c(
    "papaja::apa6_docx",
    "papaja::apa6_pdf"
  )),

  #################################################################
  ##                   SUPPLEMENTARY MATERIALS                   ##
  #################################################################
  ### Using study id as a fixed effect

  ### Run models
  ### Study ID is a fixed effect rather than a random effect
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

  tar_target(
    model_diagnostics,
    make_model_diagnostics(
      model_list_by_age,
      model_list_by_bmi,
      model_list_by_ses,
      model_list_by_sex,
      model_list_by_weekday,
      model_list_by_season,
      model_list_by_region,
      model_list_by_daylight,
      model_list_by_wear_location
    )
  ),

  ### Produce supplementary material
  tar_render(multiverse, "doc/multiverse.Rmd", output_format = c(
    "papaja::apa6_pdf"
  ))
)
