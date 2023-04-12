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
  require(mice)

  imp_data <- data %>%
    dplyr::select(-n_valid_hours, -n_hours, -day_zero) |>
    dplyr::mutate(participant_id = as.integer(factor(participant_id)))

  # Empty imputation to change defaults:
  m0 <- mice(imp_data, maxit = 0)

  # Don't do imputation based on these vars:
  dont_imp <- c("filename", "calendar_date")
  dont_use <- c(
    "age_cat", "studyid", "participant_id", "country", "region",
    "accelerometer_wear_location", "accelerometer_model", "pa_intensity_m16",
    "pa_mostactivehr", "weekday_x"
  )
  # Don't imp some vars, and disable some as predictors
  meth <- m0$method
  pred <- m0$predictorMatrix
  meth[names(meth) %in% dont_imp] <- ""
  pred[, colnames(pred) %in% c(dont_use, dont_imp)] <- 0

  participant_invar <- c("age", "weight", "height", "bmi")

  participant_cont <-
    c(
      "pa_volume",
      "pa_intensity",
      "pa_intensity_m16",
      "pa_mostactivehr",
      "sleep_duration",
      "sleep_efficiency",
      "sleep_onset",
      "sleep_wakeup",
      "sleep_regularity",
      "sleep_efficiency_lag",
      "sleep_onset_lag",
      "sleep_wakeup_lag",
      "sleep_regularity_lag",
      "sleep_duration_lag"
    )

  # Multi-level imputation, consider correlations within participant
  pred["sex", ] <- 0
  pred["sex", c("age", "bmi", "pa_intensity", "screen_time", "sleep_regularity")] <- 1
  pred[c(participant_cont, participant_invar, "sex"), "participant_id"] <- -2L
  meth[c(participant_cont)] <- "2l.pmm"
  meth[c(participant_invar)] <- "2lonly.pmm"
  meth["sex"] <- "2lonly.pmm"

  # Run imps with better settings
  future_cores <- min(parallel::detectCores() - 1, n_imps)

  dist.core <- cut(1:n_imps, future_cores, labels = paste0("core", 1:future_cores))
  n.imp.core <- as.vector(table(dist.core))

  future::plan("multisession",
    workers = future_cores
  )

  imps <- furrr::future_map(n.imp.core, function(x) {
    mice(
      data = imp_data,
      m = x,
      predictorMatrix = pred,
      method = meth,
      printFlag = FALSE,
      seed = NA
    )
  },
  .options = furrr::furrr_options(seed = TRUE, packages = c("mice", "miceadds"))
  )

  future::plan(future::sequential)

  # postprocess clustered imputation into a mids object
  imp <- imps[[1]]
  if (length(imps) > 1) {
    for (i in 2:length(imps)) {
      imp <- ibind(imp, imps[[i]])
    }
  }
  # let imputation matrix correspond to grand m
  for (i in 1:length(imp$imp)) {
    colnames(imp$imp[[i]]) <- 1:imp$m
  }

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
      "pa_intensity_m16",
      paste0(sleep_vars, "_lag")
    )

  scale_names <- paste0("scale_", variables_to_scale)

  imp_list <- data.table(complete(imp, action = "long", include = TRUE))

  for (v in seq_along(variables_to_scale)) {
    imp_list[, (eval(scale_names[v])) := as.numeric(scale(eval(parse(text = variables_to_scale[v])))), by = ".imp"]
  }

  imp_list$log_pa_volume <- log(imp_list$pa_volume)

  as.mids(imp_list)
}
