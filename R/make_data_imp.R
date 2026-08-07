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

make_data_imp <- function(data, n_imps = 3, adult_ref = NULL) {
  require(mice)

  imp_data <- data %>%
    dplyr::mutate(participant_id = as.integer(factor(participant_id))) |>
    dplyr::filter(eligible) |>
    dplyr::select(
      -n_valid_hours,
      -n_hours,
      -day_zero,
      # Constant after the filter, and redundant with participant_id.
      -eligible,
      -filename,
      # Data-quality bookkeeping, not a variable any model uses.
      -accel_file_flagged,
      # 237-level factor not used
      -measurement_day,
      # Rederived from the completed data at the end of this function.
      -dplyr::ends_with("_lag")
    )

  # Empty imputation to change defaults:
  m0 <- mice(imp_data, maxit = 0)

  # Not imputed: derived from something else, or unused by any model. weight and
  # height are missing whenever bmi is (96% of the time), so imputing them adds
  # noise without information, and no model reads them.
  dont_imp <- c(
    "calendar_date",
    "age_cat",
    "pa_intensity_m16",
    "weight",
    "height",
    "bmi_z"
  )
  # Not used as predictors. city/country are near-collinear with studyid; both
  # are character, which mice silently drops
  dont_use <- c(
    "age_cat",
    "participant_id",
    "city",
    "country",
    "region",
    "acc_wear_loc",
    "accelerometer_model",
    "pa_intensity_m16",
    "pa_mostactivehr",
    "weekday",
    "ethnicity",
    # Incomplete and not imputed, so they cannot be predictors either.
    "weight",
    "height",
    "bmi_z"
  )
  # Don't imp some vars, and disable some as predictors
  meth <- m0$method
  pred <- m0$predictorMatrix
  meth[names(meth) %in% dont_imp] <- ""
  pred[, colnames(pred) %in% c(dont_use, dont_imp)] <- 0

  participant_invar <- c("age", "bmi")

  participant_cont <-
    c(
      "pa_volume",
      "pa_intensity",
      "pa_mostactivehr",
      "sleep_duration",
      "sleep_efficiency",
      "sleep_onset",
      "sleep_wakeup",
      "sleep_regularity"
    )

  # Multi-level imputation, consider correlations within participant
  pred["sex", ] <- 0
  pred[
    "sex",
    c(
      "studyid",
      "age",
      "bmi",
      "pa_intensity",
      "screen_time",
      "sleep_regularity"
    )
  ] <- 1
  pred[c(participant_cont, participant_invar, "sex"), "participant_id"] <- -2L
  meth[c(participant_cont)] <- "2l.pmm"
  meth[c(participant_invar)] <- "2lonly.pmm"
  meth["sex"] <- "2lonly.pmm"

  # Run imps with better settings.
  future_cores <- min(parallel::detectCores() - 1, n_imps, 8)

  dist_core <- cut(
    1:n_imps,
    future_cores,
    labels = paste0("core", 1:future_cores)
  )
  n_imp_core <- as.vector(table(dist_core))

  future::plan("multisession", workers = future_cores)
  on.exit(future::plan(future::sequential), add = TRUE)

  imps <- furrr::future_map(
    n_imp_core,
    function(x) {
      mice(
        data = imp_data,
        m = x,
        predictorMatrix = pred,
        method = meth,
        printFlag = FALSE,
        seed = NA
      )
    },
    .options = furrr::furrr_options(
      seed = TRUE,
      packages = c("mice", "miceadds")
    )
  )

  # ibind() keeps only its first argument's loggedEvents, so collect them here
  # and renumber `im` to the final imputation index.
  logged <- do.call(
    rbind,
    lapply(seq_along(imps), function(k) {
      events <- imps[[k]]$loggedEvents
      if (is.null(events) || nrow(events) == 0) {
        return(NULL)
      }
      events$im <- events$im + sum(n_imp_core[seq_len(k - 1)])
      events
    })
  )

  # postprocess clustered imputation into a mids object
  imp <- imps[[1]]
  if (length(imps) > 1) {
    for (i in 2:length(imps)) {
      imp <- ibind(imp, imps[[i]])
    }
  }
  # let imputation matrix correspond to grand m
  for (i in seq_along(imp$imp)) {
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

  imp_long <- complete(imp, action = "long", include = TRUE)

  # Derive the lags from the completed data, one imputation at a time, so that
  # sleep_x_lag on day t is sleep_x on day t-1 in the same imputation.
  imp_long$.row <- seq_len(nrow(imp_long))
  imp_long <- add_sleep_lags(imp_long, by = c(".imp", "participant_id"))
  imp_long <- imp_long[order(imp_long$.row), , drop = FALSE]
  imp_long$.row <- NULL

  # Follow imputed age rather than being imputed separately.
  imp_long$age_cat <- age_categories(imp_long$age)

  # Same for bmi_z, which is a deterministic function of bmi, age and sex.
  # One adult reference across all imputations keeps them on a common scale;
  # passing data_clean's reference in also keeps them on the same scale as
  # Table 1 and the by_bmi moderator points.
  imputed_rows <- imp_long$.imp > 0
  if (is.null(adult_ref)) {
    adult_ref <- bmi_z_adult_ref(
      bmi = imp_long$bmi[imputed_rows],
      age = imp_long$age[imputed_rows],
      participant_id = paste(
        imp_long$.imp[imputed_rows],
        imp_long$participant_id[imputed_rows]
      )
    )
  }
  imp_long$bmi_z <- make_bmi_z(
    imp_long$bmi,
    imp_long$age,
    imp_long$sex,
    imp_long$participant_id,
    adult_ref = adult_ref
  )

  imp_list <- data.table(imp_long)

  # One set of scaling constants for every imputation, taken from the imputed
  # rows
  for (v in seq_along(variables_to_scale)) {
    x <- imp_list[[variables_to_scale[v]]]
    data.table::set(
      imp_list,
      j = scale_names[v],
      value = (x - mean(x[imputed_rows], na.rm = TRUE)) /
        stats::sd(x[imputed_rows], na.rm = TRUE)
    )
  }

  imp_list$log_pa_volume <- log(imp_list$pa_volume)

  out <- as.mids(imp_list)
  out$loggedEvents <- logged
  out
}
