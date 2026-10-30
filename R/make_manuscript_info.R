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
  model_list_by_age = NULL,
  model_list_by_age_7days = NULL,
  model_list_by_age_blended = NULL,
  model_list_by_age_clpm = NULL,
  model_list_by_age_mvpa = NULL,
  negative_control = NULL,
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
  weekday_p <- papaja::print_p(weekday$p.value)
  if (!startsWith(weekday_p, "<")) weekday_p <- paste("=", weekday_p)
  # Plain text, not LaTeX: Quarto prints inline `{r}` output literally
  ms_info$weekday_res <- glue::glue(
    "χ²({weekday$parameter}) = {papaja::print_num(weekday$statistic)}, ",
    "p {weekday_p}"
  )

  ms_info$weekday <- weekday

  ms_info$weekday$stdres <- format(round(weekday$stdres, 2), nsmall = 2)

  ms_info$age_range <- paste(
    floor(min(data_clean_analytic$age, na.rm = TRUE)),
    "to",
    floor(max(data_clean_analytic$age, na.rm = TRUE))
  )
  studies_no_regularity <- data_clean_analytic |>
    dplyr::group_by(studyid) |>
    dplyr::summarise(none = all(is.na(sleep_regularity))) |>
    dplyr::filter(none) |>
    dplyr::pull(studyid) |>
    as.character()
  ms_info$studies_no_regularity <- paste(studies_no_regularity, collapse = ", ")
  ms_info$n_studies_no_regularity <- length(studies_no_regularity)

  ms_info$n_pts_7days <- data_clean_analytic |>
    dplyr::filter(n_valid_wear_days >= 7) |>
    dplyr::distinct(participant_id) |>
    nrow() |>
    fmt_n()

  if (!is.null(model_list_by_age)) {
    ms_info$age_centre <- attr(model_list_by_age, "age_centre")
    ms_info$slopes <- format_age_slopes(model_list_by_age)
    ms_info$slopes_between <- format_age_slopes(model_list_by_age, between = TRUE)
    ms_info$sd <- format_predictor_sds(model_list_by_age)
    # Day-to-day SD of volume within participants
    sd_volume <- attr(model_list_by_age, "scale_descriptives")[var == "pa_volume", sd]
    ms_info$sd_within_pa_volume <- paste(papaja::print_num(
      sd(data_imp$data$scale_pa_volume_wp, na.rm = TRUE) * sd_volume,
      digits = 1
    ), "mg")
    ms_info$n_obs_model <- format_model_n(model_list_by_age)
  }
  families <- list(
    slopes_7days = model_list_by_age_7days,
    slopes_blended = model_list_by_age_blended,
    slopes_clpm = model_list_by_age_clpm,
    slopes_mvpa = model_list_by_age_mvpa
  )
  for (nm in names(families)) {
    if (!is.null(families[[nm]])) {
      ms_info[[nm]] <- format_age_slopes(families[[nm]])
    }
  }
  if (!is.null(model_list_by_age_mvpa)) {
    ms_info$sd$pa_mvpa <- format_predictor_sds(model_list_by_age_mvpa)$pa_mvpa
  }
  # b [95% CI] by term, for each outcome
  if (!is.null(negative_control)) {
    ms_info$negative_control <- lapply(negative_control, function(tab) {
      as.list(stats::setNames(as.character(tab[[2]]), tab$term))
    })
  }

  ms_info
}

fmt_n <- function(x) format(x, big.mark = ",")

# Multiplier from each variable's stored scale to its reported unit
raw_units <- data.table::data.table(
  var = c(
    "sleep_duration",
    "sleep_efficiency",
    "sleep_onset",
    "sleep_regularity",
    "pa_volume",
    "pa_intensity",
    "pa_mvpa"
  ),
  mult = c(1, 100, 60, 1, 1, 1, 1),
  unit = c("min", "percentage points", "min", "points", "mg", "", "min"),
  digits = c(1, 1, 0, 1, 1, 2, 1)
)

format_predictor_sds <- function(model_list) {
  sds <- attr(model_list, "scale_descriptives")
  sds <- sds[!grepl("_lag$", var)]
  out <- lapply(seq_len(nrow(sds)), function(i) {
    u <- raw_units[var == sds$var[i]]
    trimws(paste(
      papaja::print_num(sds$sd[i] * u$mult, digits = u$digits),
      u$unit
    ))
  })
  names(out) <- sds$var
  out
}

#' format_age_slopes
#'
#' @param between person-mean slopes instead of within-person (or blended) ones
#' @return list keyed "<outcome>_by_<predictor>", then "a<age>", holding `b`,
#'   `ci`, `p`, and `raw` (outcome units per 1 SD of the predictor). MVPA
#'   predictors are per 30 min instead of per SD.
format_age_slopes <- function(model_list, between = FALSE) {
  sds <- attr(model_list, "scale_descriptives")
  out <- lapply(names(model_list), function(nm) {
    slopes <- model_list[[nm]]$model_assets$age_slopes
    slopes <- slopes[grepl("_pm$", term) == between]
    predictor <- gsub("^(scale|log)_|_lag$", "", sub(".* by ", "", nm))
    per <- if (predictor == "pa_mvpa") 30 / sds[var == "pa_mvpa", sd] else 1
    slopes[,
      c("estimate", "conf.low", "conf.high") := lapply(.SD, `*`, per),
      .SDcols = c("estimate", "conf.low", "conf.high")
    ]
    outcome <- gsub("^(scale|log)_", "", sub(" by .*", "", nm))
    u <- raw_units[var == outcome]
    sd_outcome <- sds[var == outcome, sd]
    per_age <- lapply(seq_len(nrow(slopes)), function(j) {
      s <- slopes[j]
      list(
        b = papaja::print_num(s$estimate),
        ci = paste0(
          papaja::print_num(s$estimate),
          " [",
          papaja::print_num(s$conf.low),
          ", ",
          papaja::print_num(s$conf.high),
          "]"
        ),
        p = papaja::print_p(s$p.value),
        raw = trimws(paste(
          papaja::print_num(
            abs(s$estimate) * sd_outcome * u$mult,
            digits = u$digits
          ),
          u$unit
        ))
      )
    })
    names(per_age) <- paste0("a", slopes$age)
    per_age
  })
  names(out) <- model_keys(model_list)
  out
}

#' format_model_n
#'
#' @return list keyed like `format_age_slopes()`, holding each model's
#'   person-days, formatted
format_model_n <- function(model_list) {
  out <- lapply(model_list, function(m) {
    n <- m$model_assets$n_obs
    if (is.null(n)) NA_character_ else fmt_n(n)
  })
  names(out) <- model_keys(model_list)
  out
}

# "scale_pa_volume by scale_sleep_regularity_lag" -> "pa_volume_by_sleep_regularity"
model_keys <- function(model_list) {
  vapply(
    names(model_list),
    function(nm) {
      parts <- strsplit(nm, " by ")[[1]]
      paste(
        gsub("^(scale|log)_|_lag$", "", parts[1]),
        "by",
        gsub("^(scale|log)_|_lag$", "", parts[2]),
        sep = "_"
      )
    },
    character(1),
    USE.NAMES = FALSE
  )
}
