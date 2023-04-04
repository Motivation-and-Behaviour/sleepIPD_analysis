#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param imp_data <- tar_read(data_imp_log)
#' @return
#' @author conig
#' @test data
#' @export
#'
make_model_list <- function(
    imp_data,
    moderator = "age",
    moderator_term = "11, 18, 35, 65",
    control_vars = c("ses", "sex", "bmi"),
    ranef = "(1|studyid) + (1|participant_id)") {

  # Declare base variables to analyse
  sleep_vars <- c(
    "sleep_duration",
    "sleep_efficiency",
    "sleep_onset",
    "sleep_regularity"
  )

  pa_vars <- c(
    "pa_volume", "pa_intensity"
  )

  # Find transformed variables for use (log or scaled)
  data_names <- names(imp_data$data)

  sleep_vars <- sapply(sleep_vars, function(x){
    grep(glue::glue("(scale|log)_{x}$"), data_names, value = TRUE)
  })
  names(sleep_vars) <- display_names(sleep_vars)

  pa_vars <- sapply(pa_vars, function(x){
    grep(glue::glue("(scale|log)_{x}$"), data_names, value = TRUE)
  })
  names(pa_vars) <- display_names(pa_vars)


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
    instructions,
    glue::glue("{outcome} by {gsub(' .*', '', predictors)}")
  )

  out <- lapply(
    seq_len(length(instructions[, 1])),
    FUN = function(i) {
      model <- model_builder(
        imp_data,
        outcome = instructions[i, "outcome"],
        predictors = instructions[i, "predictors"],
        moderator = moderator,
        control_vars = control_vars,
        table_only = FALSE,
        ranef = ranef,
        terms = c(
          paste0(
            gsub(" .*", "", instructions[i, "predictors"]), "[-4:4 by = 0.1]"
          ),
          glue::glue("{moderator} [{moderator_term}]")
        ),
        RQ = instructions[i, "RQ"]
      )

      attr(model, "RQ") <- instructions[i, "RQ"] # nolint: object_name_linter.
      attr(model, "moderator") <- moderator

      model
    }
  )

  names(out) <- instructions[, "model_name"]
  attr(out, "vars") <- list(sleep_vars = sleep_vars, pa_vars = pa_vars)
  out
}
