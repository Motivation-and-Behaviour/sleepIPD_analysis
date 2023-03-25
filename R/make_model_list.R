#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_imp
#' @return
#' @author conig
#' @export
make_model_list <- function(data_imp, moderator = "age", moderator_term = "11, 18, 35, 65", control_vars = c("ses", "sex", "bmi"), ranef  = "(1|studyid) + (1|measurement_day) + (1|participant_id)") {

  sleep_vars <- c("scale_sleep_duration",
                    "scale_sleep_efficiency",
                    "scale_sleep_onset",
                    "scale_sleep_regularity")

  sleep_lag_vars <- paste0(sleep_vars, "_lag")

  PA_vars <- c("scale_pa_volume", "scale_pa_intensity")

  make_quadratic <- function(x) glue::glue("{x} * {moderator} + I({x}^2) * {moderator}")

  instructions.rq1 <-
    expand.grid(
      outcome = sleep_vars,
      predictors = make_quadratic(PA_vars),
      RQ = 1,
      stringsAsFactors = FALSE
    )

  instructions.rq3 <-
    expand.grid(
      outcome = PA_vars,
      predictors = make_quadratic(sleep_lag_vars),
      RQ = 3,
      stringsAsFactors = FALSE
    )

  instructions <- rbind(instructions.rq1, instructions.rq3)

  instructions$model_name <- with(instructions, glue::glue("{outcome} by {gsub(' .*', '', predictors)}"))

  out <- lapply(
    seq_len(length(instructions[,1])),
    FUN = function(i) {
    model <-  model_builder(
        data_imp,
        outcome = instructions[i, "outcome"],
        predictors = instructions[i, "predictors"],
        control_vars = control_vars,
        table_only = FALSE,
        ranef = ranef
      )

    attr(model, "terms") <- c(paste0(gsub(" .*", "" ,instructions[i, "predictors"]), "[-4:4 by = 0.1]"), glue::glue("{moderator} [{moderator_term}]"))
    attr(model, "RQ") <- instructions[i , "RQ"]
    attr(model, "moderator") <- moderator
    model

    }
  )

  names(out) <- instructions[,"model_name"]
  out
}
