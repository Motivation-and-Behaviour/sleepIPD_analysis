create_distinct <- function(df) {
  variables <- c(
    "SES",
    "Ethnicity",
    "Sleep conditions",
    "Maturational status",
    "Sleep medications"
  )

  dir.create("temp", showWarnings = FALSE)

  purrr::map(
    variables,
    ~ df %>%
      dplyr::mutate(studyid = gsub(".*(\\d{3}).*", "\\1", studyid)) %>%
      dplyr::distinct(dplyr::across(dplyr::all_of(c("studyid", .x)))) %>%
      dplyr::filter(!is.na(dplyr::across(dplyr::all_of(.x)))) %>%
      readr::write_csv(file.path("temp", paste0(.x, ".csv", collapse = "")))
  )
}

#' figure_theme
#'
#' Attractive ggplot2 theme defaults

figure_theme <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
      text = ggplot2::element_text(family = "serif"),
      strip.text.y = ggplot2::element_text(angle = 0),
      strip.background.y = ggplot2::element_blank()
    )
}

#' APA_style for gt

apa_style <- function(x) {
  x |>
    opt_table_lines(extent = "none") |>
    tab_options(
      heading.border.bottom.width = 2,
      heading.border.bottom.color = "black",
      heading.border.bottom.style = "solid",
      heading.title.font.size = 12,
      table.font.size = 12,
      heading.subtitle.font.size = 12,
      table_body.border.bottom.color = "black",
      table_body.border.bottom.width = 1,
      table_body.border.bottom.style = "solid",
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = 1
    ) |>
    opt_table_font(font = "times")
}
#' convert dates to seasons
#' @param date a date
#' @param lat latitude
#' @return a season
#'

get_season <- function(date, lat) {
  if (is.na(lat) || is.na(date)) {
    return(NA_character_)
  }
  if (lat > 0) {
    if (lubridate::month(date) %in% c(3:5)) {
      "spring"
    } else if (lubridate::month(date) %in% c(6:8)) {
      "summer"
    } else if (lubridate::month(date) %in% c(9:11)) {
      "autumn"
    } else {
      "winter"
    }
  } else {
    if (lubridate::month(date) %in% c(3:5)) {
      "autumn"
    } else if (lubridate::month(date) %in% c(6:8)) {
      "winter"
    } else if (lubridate::month(date) %in% c(9:11)) {
      "spring"
    } else {
      "summer"
    }
  }
}

find_max <- function(x) {
  counts <- table(x)
  if (length(counts) == 0L || sum(counts) == 0L) {
    return(NA_character_)
  }
  names(which.max(counts))
}

#' age_categories
#'
#' Bin continuous age into the five reporting bands.
age_categories <- function(age) {
  min_age <- floor(min(age, na.rm = TRUE))
  cut(
    age,
    breaks = c(0, 12, 19, 36, 66, Inf),
    labels = c(
      glue::glue("{min_age}-11 years"),
      "12-18 years",
      "19-35 years",
      "36-65 years",
      "66+ years"
    ),
    right = FALSE,
    include.lowest = TRUE
  )
}

#' Generates display names for variables
#' @param x a list containing variables e.g. pa_vars

display_names <- function(x) {
  is_scale <- grepl("^scale", x)
  is_log <- grepl("^log", x)

  out <- gsub("(scale_|log_)", "", x) |>
    gsub("_", " ", x = _) |>
    gsub("pa", "physical activity", x = _) |>
    stringr::str_to_sentence()

  out[is_scale] <- paste(out[is_scale], "(z)")
  out[is_log] <- paste(out[is_log], "(ln)")
  out
}

#' get_scale_descriptives
#'
#' Get descriptives for multiply imputed data for scale variables

get_scale_descriptives <- function(data, ...) {
  vars <- unlist(list(...))
  # We want descriptives to convert log back to scale
  # So we need the scale descriptives, not the log descriptives
  vars <- gsub("(log_|scale_)", "", vars)
  dat <- mice::complete(data, action = "long", include = FALSE) |>
    data.table()

  # Get all imp data, make long form
  dt <- dat[, c(vars, ".imp"), with = FALSE] |>
    tidyr::pivot_longer(-.imp, names_to = "var") |>
    data.table()

  # Mean and SD of the stacked imputations
  dt[,
    .(
      mean = mean(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE)
    ),
    by = "var"
  ]
}

#' predictor_grid
#'
#' Build a `ggeffects` `terms` string spanning a variable's observed range.
#'
#' @param data_imp a `mids` object
#' @param var variable name
#' @param n approximate number of grid points, when `step` is not given
#' @param step explicit grid spacing
#'
#' @details Replaces a hardcoded `[-5:5]`. For `log_pa_volume`, observed over
#' 0.33-5.30, that put 53% of the requested grid below any observed value, and
#' the heat-map's `age[10:80 by = 1]` omitted both ends of a sample spanning
#' 2.75-94.1. Every point returned here lies inside the data, so no prediction
#' is an extrapolation.
#'
#' The range comes from `data_imp$data`, the unimputed rows. Every method in
#' `make_data_imp()` is predictive mean matching (`2l.pmm` / `2lonly.pmm`),
#' which can only donate values that were observed somewhere, so the completed
#' range equals the observed range — verified against `mice::complete()` for
#' all five predictors. Reading it from a single unimputed copy also guarantees
#' the grid is identical across imputations, which
#' `ggeffects::pool_predictions()` requires. If a method is ever changed to one
#' that can generate values outside the observed range, this needs revisiting.
predictor_grid <- function(data_imp, var, n = 200, step = NULL) {
  x <- data_imp$data[[var]]
  if (is.null(x)) {
    stop(
      "predictor_grid(): '",
      var,
      "' is not a column of the imputed data.",
      call. = FALSE
    )
  }
  rng <- range(x, na.rm = TRUE)
  if (is.null(step)) {
    step <- signif(diff(rng) / n, 3)
  }
  # Round the endpoints inward, so rounding cannot put a grid point outside the
  # observed range and reintroduce the extrapolation this function exists to
  # remove.
  scale <- 10^4
  lower <- ceiling(rng[1] * scale) / scale
  upper <- floor(rng[2] * scale) / scale
  as.character(glue::glue("{var}[{lower}:{upper} by = {step}]"))
}

#' cap_blas_threads
#'
#' Limit this process's BLAS/OpenMP thread pool.
#'
#' @param n threads to allow
#'
#' @details R here links `openblas-pthread`, which spawns one BLAS thread per
#' core (48) in *every* process. Measured on a live run: 8 imputation workers
#' carrying 49 threads each, ~346 CPU-hours of BLAS-thread time in 7.6 h wall,
#' load average 182; and each crew worker carrying 79 threads, which at
#' `workers = 16` is ~1,264 threads on 48 cores. At this problem size 48-thread
#' BLAS is not faster than single-threaded (0.16 s vs 0.14 s on a 20,000 x 200
#' crossprod), so that CPU time is pure loss.
#'
#' `OPENBLAS_NUM_THREADS` cannot fix it from inside R: OpenBLAS reads the
#' variable when the shared library loads, which is before R evaluates
#' `.Renviron`, so neither a project `.Renviron`, `Sys.setenv()` nor crew's
#' `rscript_envs` has any effect. The thread pool is per-process, so this has to
#' be called *inside* each process that does the work — hence the calls in the
#' `furrr` callback and at the top of `model_builder()` rather than once in
#' `_targets.R`.
#'
#' Guarded rather than hard-required, so a missing package cannot kill a
#' multi-hour run. Note that changing the thread count can reorder
#' floating-point summation, so results may differ in the last few bits.
cap_blas_threads <- function(n = 1L) {
  if (requireNamespace("RhpcBLASctl", quietly = TRUE)) {
    RhpcBLASctl::blas_set_num_threads(n)
    RhpcBLASctl::omp_set_num_threads(n)
  }
  invisible(NULL)
}

plot_percentile <- function(var) {
  require(ggplot2)

  p <- lapply(seq(0, 1, by = 0.005), function(x) {
    data.frame(p = x * 100, value = quantile(var, x, na.rm = TRUE))
  }) |>
    data.table::rbindlist()

  ggplot(p, aes(x = p, y = value)) +
    geom_point() +
    labs(x = "Percentile")
}

sheet_read <- function(sheet_name) {
  sheetid <- "1A75Qk8mNXygxcsCxLQ4maspZsQxZXJ5K-12X338CQ2s"
  googlesheets4::gs4_deauth()
  googlesheets4::read_sheet(
    sheetid,
    sheet = sheet_name,
    col_types = "c",
    range = "A:C"
  )
}

sheet_last_modified <- function() {
  sheetid <- "1A75Qk8mNXygxcsCxLQ4maspZsQxZXJ5K-12X338CQ2s"
  googledrive::drive_deauth()
  info <- googledrive::drive_get(googledrive::as_id(sheetid))
  info$drive_resource[[1]]$modifiedTime
}
