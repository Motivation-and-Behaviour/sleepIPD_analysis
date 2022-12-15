# Packages
source("./packages.R")

# Load functions
lapply(list.files("./R", full.names = TRUE), source)

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
  tar_target(data_joined, bind_rows(data_raw), pattern = map(data_raw)),
  tar_target(data_clean, clean_data(data_joined)),
  tar_render(manuscript, "doc/manuscript.Rmd")
)
