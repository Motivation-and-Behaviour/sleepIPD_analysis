#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_imp
#' @return
#' @author conig
#' @export
make_model_list <- function(data_imp) {

  sleep_vars <- c("sleep_duration",
                  "sleep_efficiency",
                  "sleep_onset",
                  "sleep_regularity")

  PA_vars <- c("pa_volume", "pa_intensity")

  control_vars <- c("ses", "sex", "bmi")


  instructions.rq1 <-
    expand.grid(
      sleep_measure = c(
        "sleep_duration",
        "sleep_efficiency",
        "sleep_onset",
        "sleep_regularity"
      ),
      exercise = c(
        "pa_volume * age + I(pa_volume^2) * age",
        "pa_intensity * age + I(pa_intensity^2) * age"
      ),
      stringsAsFactors = FALSE
    )

  instructions.rq1$model_name <- with(instructions.rq1, glue::glue("{sleep_measure} by {gsub(' .*', '', exercise)}"))

  out <- lapply(
    seq_len(length(instructions.rq1[,1])),
    FUN = function(i) {
      model_builder_RQ1(
        data_imp,
        outcome = instructions.rq1[i, 1],
        predictors = instructions.rq1[i,2],
        control_vars = control_vars,
        table_only = FALSE
      )

    }
  )

  names(out) <- instructions.rq1[,"model_name"]
  out
}
