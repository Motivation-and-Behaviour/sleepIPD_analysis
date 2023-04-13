#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_imp
#' @return
#' @author conig
#' @export
make_model_list <- function(data_imp,
                            moderator = "age",
                            moderator_term = "11, 18, 35, 65",
                            control_vars = c("ses", "age", "sex", "bmi"),
                            ranef = "(1|studyid) + (1|participant_id)",
                            sleep_vars = c(
                              "scale_sleep_duration", "scale_sleep_efficiency",
                              "scale_sleep_onset", "scale_sleep_regularity"
                            ),
                            pa_vars = c(
                              "log_pa_volume", "scale_pa_intensity"
                            )) {
  control_vars <- control_vars[!control_vars == moderator]

  sleep_lag_vars <- paste0(sleep_vars, "_lag")

  quadratic_pattern <- "{x} * {moderator} + I({x}^2) * {moderator}"
  make_quadratic <- function(x) glue::glue(quadratic_pattern)

  instructions_rq1 <-
    expand.grid(
      outcome = sleep_vars,
      predictors = make_quadratic(pa_vars),
      RQ = 1,
      stringsAsFactors = FALSE
    )

  instructions_rq3 <-
    expand.grid(
      outcome = pa_vars,
      predictors = make_quadratic(sleep_lag_vars),
      RQ = 3,
      stringsAsFactors = FALSE
    )

  instructions <- rbind(instructions_rq1, instructions_rq3)

  instructions$model_name <- with(
    instructions, glue::glue("{outcome} by {gsub(' .*', '', predictors)}")
  )

  out <- lapply(
    seq_len(length(instructions[, 1])),
    FUN = function(i) {
      model <- model_builder(
        data_imp,
        outcome = instructions[i, "outcome"],
        predictors = instructions[i, "predictors"],
        moderator = moderator,
        control_vars = control_vars,
        table_only = FALSE,
        ranef = ranef,
        terms = c(
          paste0(
            gsub(" .*", "", instructions[i, "predictors"]), "[-4:4 by = 0.1]"),
          glue::glue("{moderator} [{moderator_term}]")
        ),
        RQ = instructions[i, "RQ"]
      )

      attr(model, "RQ") <- instructions[i, "RQ"] # nolint: object_name_linter.
      attr(model, "moderator") <- moderator

      model
    }
  )

  # Add some attributes used in other targets
  names(out) <- instructions[, "model_name"]
  attr(out, "vars") <- list(sleep_vars = sleep_vars, pa_vars = pa_vars)
  attr(out, "filename_suffix") <-
    dplyr::case_when(
      "studyid" %in% control_vars ~ "_fixedef",
      "scale_pa_volume" %in% pa_vars ~ "_nolog",
      TRUE ~ ""
    )

  out
}
