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
  tar_target(data_clean, clean_data(data_joined)),
  tar_target(data_holdout, make_data_holdout(data_clean)),
  tar_target(table_1, make_table1(data_clean)),
  tar_target(data_imp, make_data_imp(data_imp)),

  # Modelling targets
  tar_target(model_list, make_model_list(data_imp)),
  tar_target(model_diagnostics, make_model_diagnostics(model_list)),

  # Figure
  tar_target(explore_img, make_explore_img_list(data_clean)),
  tar_target(purdy_pictures, produce_purdy_pictures()),

  # Output manuscript
  tar_render(manuscript, "doc/manuscript.Rmd")
)
