#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data data holdout or clean data
#' @param n_imps number of imputations
#' @return
#' @test data <- tar_read(data_holdout)
#' @author conig
#' @export

make_data_imp <- function(data, n_imps = 3) {
  imp_data <- data %>%
    select(-n_valid_hours, -n_hours, -weekday_x, -day_zero) |>
    dplyr::mutate(participant_id = as.integer(factor(participant_id)))

  # Empty imputation to change defaults:
  m0 <- mice(imp_data, maxit = 0)

  # Don't do imputation based on these vars:

  dont_imp <- c("filename", "calendar_date")
  dont_use <- c("age_cat", "studyid", "participant_id", "country" ,
                "accelerometer_wear_location", "accelerometer_model")
  # Don't imp some vars, and disable some as predictors
  meth <- m0$method
  pred <- m0$predictorMatrix
  meth[names(meth) %in% dont_imp] <- ""
  pred[, colnames(pred) %in% c(dont_use, dont_imp)] <- 0

  participant_invar <- c("age","weight","height","bmi")

  participant_cont <-
    c("pa_volume",
      "pa_intensity",
      "sleep_duration",
      "sleep_efficiency",
      "sleep_onset",
      "sleep_wakeup",
      "sleep_onset_time",
      "sleep_wakeup_time",
      "sleep_regularity",
      "sleep_efficiency_lag",
      "sleep_onset_lag",
      "sleep_wakeup_lag",
      "sleep_onset_time_lag",
      "sleep_wakeup_time_lag",
      "sleep_regularity_lag",
      "sleep_duration_lag"
    )

  # Multi-level imputation, consider correlations within participant
  pred["sex",] <- 0
  pred["sex", c("age","bmi","pa_intensity","screen_time","sleep_regularity")] <- 1
  pred[c(participant_cont, participant_invar, "sex"), "participant_id"] <- -2L
  meth[c(participant_cont)] <- "2l.norm"
  meth[c(participant_invar)] <- "2lonly.pmm"
  meth["sex"] <- "2lonly.pmm"

  # Run imps with better settings
  future_cores <- min(parallel::detectCores() - 1, n_imps)

  imps <-
    futuremice(
      imp_data,
      m = n_imps,
      predictorMatrix = pred,
      method = meth,
      n.core = future_cores
    )

  # include_scale_variables
  sleep_vars <- c(
    "sleep_duration",
    "sleep_efficiency",
    "sleep_onset",
    "sleep_regularity"
  )

  variables_to_scale <-
    c(
      sleep_vars,
      "pa_volume",
      "pa_intensity",
      paste0(sleep_vars, "_lag")
    )

  scale_names <- paste0("scale_", variables_to_scale)

  imp_list <- data.table(complete(imps, action = "long", include = TRUE))

  for (v in seq_along(variables_to_scale)) {
    imp_list[, (eval(scale_names[v])) := as.numeric(scale(eval(parse(text = variables_to_scale[v])))), by = ".imp"]
  }

  as.mids(imp_list)
}
