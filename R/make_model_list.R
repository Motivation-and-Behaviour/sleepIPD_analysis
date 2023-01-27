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


  PA_vars <- c("pa_volume", "pa_intensity")

  control_vars <- c("ses", "sex", "bmi")

  instructions.rq1 <-
    expand.grid(
      sleep_measure = c(
        "scale_sleep_duration",
        "scale_sleep_efficiency",
        "scale_sleep_onset",
        "scale_sleep_regularity"
      ),
      exercise = c(
        "scale_pa_volume * age + I(scale_pa_volume^2) * age",
        "scale_pa_intensity * age + I(scale_pa_intensity^2) * age"
      ),
      stringsAsFactors = FALSE
    )

  instructions.rq1$model_name <- with(instructions.rq1, glue::glue("{sleep_measure} by {gsub(' .*', '', exercise)}"))

  out <- lapply(
    seq_len(length(instructions.rq1[,1])),
    FUN = function(i) {
    model <-  model_builder_RQ1(
        data_imp,
        outcome = instructions.rq1[i, 1],
        predictors = instructions.rq1[i, 2],
        control_vars = control_vars,
        table_only = FALSE
      )

    attr(model, "terms") <- c(paste0(gsub(" .*", "" ,instructions.rq1[i, 2]), "[-4:4 by = 0.1]"), "age [11, 18, 35, 65]")
    model

    }
  )

  names(out) <- instructions.rq1[,"model_name"]
  out
}
