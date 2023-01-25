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

  control_vars <- c()

  instructions <- list(
    "sleep_duration" = " pa_volume*age + I(pa_volume^2) * age",
    "sleep_efficiency" = PA_vars,
    sleep_onset = PA_vars,
    sleep_regularity = PA_vars,
    "pa_volume" = sleep_vars,
    "pa_intensity" = sleep_vars
  )

  require(future.apply)

  plan(multisession, workers = availableCores() - 4)

  out <- future_lapply(
    seq_len(length(instructions)),
    FUN = function(i) {
      model_builder_RQ1(
        data_imp,
        outcome = names(instructions)[i],
        predictors = instructions[[i]],
        table_only = FALSE
      )

    }
  )

  names(out) <- names(instructions)
  out
}
