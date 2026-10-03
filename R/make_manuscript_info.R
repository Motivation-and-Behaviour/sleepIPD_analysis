#' Make manuscript information
#'
#' Makes a list containing the information needed for the manuscript.
#' This avoids needing to load large objects when rendering the manuscript, and
#' makes it easier to include data in the abstract.
#'
#' @title make_manuscript_info
#' @param data_clean
#' @param participant_summary
#' @return List containing information for the manuscript
#' @author Taren Sanders
make_manuscript_info <- function(
  data_clean,
  participant_summary,
  data_imp = NULL,
  min_wear_days = min_wear_days_primary
) {
  data_clean_eligible <- data_clean |>
    dplyr::filter(eligible)

  data_clean_analytic <- data_clean_eligible |>
    dplyr::filter(n_valid_wear_days >= min_wear_days)

  ms_info <- list()

  ## Results
  n_pts <- length(unique(data_clean$participant_id))
  n_pts_eligible <- length(unique(data_clean_eligible$participant_id))

  ms_info$n_obs <- nrow(data_clean) |> fmt_n()
  ms_info$n_pts <- n_pts |> fmt_n()
  ms_info$n_pts_eligible <- n_pts_eligible |> fmt_n()
  ms_info$n_obs_excluded <- (nrow(data_clean) - nrow(data_clean_eligible)) |>
    fmt_n()
  ms_info$n_pts_excluded <- (n_pts - n_pts_eligible) |> fmt_n()

  # The wear-time criterion, applied on top of eligibility.
  n_pts_analytic <- length(unique(data_clean_analytic$participant_id))
  ms_info$min_wear_days <- min_wear_days
  ms_info$n_pts_analytic <- n_pts_analytic |> fmt_n()
  ms_info$n_pts_excluded_wear <- (n_pts_eligible - n_pts_analytic) |> fmt_n()
  ms_info$n_obs_analytic <- nrow(data_clean_analytic) |> fmt_n()
  ms_info$n_obs_excluded_wear <-
    (nrow(data_clean_eligible) - nrow(data_clean_analytic)) |> fmt_n()

  ms_info$n_missing_age <- dplyr::filter(data_clean_analytic, is.na(age)) |>
    dplyr::distinct(participant_id) |>
    nrow()

  ms_info$n_studies <- length(unique(data_clean$studyid))

  # Read the imputation count off the run rather than hardcoding it in the
  # Methods, where it had drifted to 50 while _targets.R was set to 5.
  ms_info$n_imps <- if (is.null(data_imp)) NA_integer_ else data_imp$m

  # Accelerometer files judged miscalibrated by null_bad_accel_files().
  ms_info$n_accel_flagged <-
    length(unique(data_clean$participant_id[data_clean$accel_file_flagged]))
  ms_info$pa_volume_limit <- plausible_ranges$pa_volume[2]

  # RQ3 predicts activity from the previous day's sleep. make_data_imp() builds
  # those lags after the eligibility filter, so days whose predecessor is
  # missing or ineligible carry no lag and drop from those models.
  ms_info$n_obs_eligible <- nrow(data_clean_eligible) |> fmt_n()
  ms_info$n_obs_lagged <- data_clean_analytic |>
    dplyr::arrange(participant_id, calendar_date) |>
    dplyr::group_by(participant_id) |>
    dplyr::summarise(
      n = sum(dplyr::lag(calendar_date) == calendar_date - 1, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::pull(n) |>
    sum() |>
    fmt_n()

  ms_info$p_female <-
    scales::label_percent(0.1)(
      mean(participant_summary$sex == "Female", na.rm = TRUE)
    )
  # Take the band names from the factor rather than hard-coding them
  age_bands <- levels(participant_summary$age_cat)
  ms_info$p_young <-
    scales::label_percent(0.1)(mean(
      participant_summary$age_cat == age_bands[1],
      na.rm = TRUE
    ))
  ms_info$p_old <-
    scales::label_percent(0.1)(mean(
      participant_summary$age_cat == age_bands[length(age_bands)],
      na.rm = TRUE
    ))

  weekday_table <- (data_clean_analytic$weekday) |> table()
  weekday <- chisq.test(weekday_table)
  ms_info$weekday_res <- glue::glue(
    "$\\chi^2_{(..weekday$parameter..)}$",
    " = ..papaja::print_num(weekday$statistic).., ",
    "p = ..papaja::print_p(weekday$p.value)..",
    .open = "..",
    .close = ".."
  )

  ms_info$weekday <- weekday

  ms_info$weekday$stdres <- format(round(weekday$stdres, 2), nsmall = 2)

  ms_info
}

fmt_n <- function(x) format(x, big.mark = ",")
