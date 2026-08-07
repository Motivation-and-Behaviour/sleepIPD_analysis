#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param multiverse_skeleton
#' @param multiverse_chunk
#' @param model_definitions
#' @return
#' @export
make_multiverse_file <- function(
  multiverse_skeleton,
  multiverse_chunk,
  model_definitions
) {
  multiverse_skeleton_lines <- readLines(multiverse_skeleton)

  control_names <- c(
    ses = "SES",
    age = "age",
    sex = "sex",
    bmi_z = "BMI z-score",
    studyid = "study fixed effects"
  )

  moderator_df <- model_definitions %>%
    dplyr::select(model_name, moderator, mod_formal, cont_vars) %>%
    dplyr::filter(moderator != "age")

  pattern <- readLines(multiverse_chunk) |> paste(collapse = "\n")

  results_chunks <- lapply(seq_len(nrow(moderator_df)), function(r) {
    moderator <- moderator_df$moderator[r]
    moderator_fig <- gsub("_", "-", moderator)
    moderator_formal <- moderator_df$mod_formal[r]
    model_name <- moderator_df$model_name[r]

    # make_model_list() drops the moderator from the controls, so the sentence
    # has to be built per model rather than hard-coded
    controls <- setdiff(moderator_df$cont_vars[[r]], moderator)
    control_text <- glue::glue_collapse(
      dplyr::recode(controls, !!!as.list(control_names)),
      sep = ", ",
      last = " and "
    )

    glue::glue(pattern, .open = "_::", .close = "::_")
  })

  write(multiverse_skeleton_lines, "doc/multiverse.Rmd")
  write(unlist(results_chunks), "doc/multiverse.Rmd", append = TRUE)

  return("doc/multiverse.Rmd")
}
