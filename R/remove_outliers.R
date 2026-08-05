# The variables the +/-4SD screen applies to.
outlier_screen_vars <- c(
  "pa_volume",
  "pa_intensity",
  "pa_intensity_m16",
  "sleep_duration",
  "sleep_efficiency",
  "sleep_onset",
  "sleep_wakeup",
  "sleep_regularity",
  "weight",
  "height",
  "waist_circumference",
  "bmi"
)

#' remove_outliers
#'
#' Set values further than `sd_threshold` SDs from the mean to NA.
#'
#' @param data data frame
#' @param cols character vector of columns to screen
#' @param by optional character vector of grouping columns; the mean and SD are
#'   computed within each group
#' @param sd_threshold number of SDs beyond which a value is set to NA
#'
#' @details The reference distribution is **within study** (`by = "studyid"` as
#' called from `clean_data()`). Pooling across all 20 studies and ages 2-94
#' meant the threshold was driven by between-study differences in device,
#' protocol and population rather than by implausible measurements, so it
#' trimmed the tails of the age and study distributions instead. Groups with
#' fewer than two observed values, or zero variance, are left untouched.
#'
#' `doc/manuscript.Rmd` (~line 196) says only "outside of +/-4SD of the mean"
#' and must be updated to name the reference distribution (Step 6).
remove_outliers <- function(data, cols, by = NULL, sd_threshold = 4) {
  missing_cols <- setdiff(c(cols, by), names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Columns not found in data: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  if (is.null(by)) {
    groups <- rep(1L, nrow(data))
  } else {
    groups <- interaction(as.list(data[by]), drop = TRUE)
  }

  for (var in cols) {
    x <- data[[var]]
    for (g in unique(groups)) {
      i <- which(groups == g)
      x[i] <- remove_outlier(x[i], sd_threshold = sd_threshold)
    }
    data[[var]] <- x
  }

  data
}

#' remove_outlier
#' @param x a numeric vector to remove outliers from
#' @param sd_threshold number of SDs beyond which a value is set to NA
remove_outlier <- function(x, sd_threshold = 4) {
  mu <- mean(x, na.rm = TRUE)
  sigma <- stats::sd(x, na.rm = TRUE)

  # Fewer than two observed values, or no variation: nothing to screen against.
  if (is.na(sigma) || sigma == 0) {
    return(x)
  }

  x[!is.na(x) & abs(x - mu) > sd_threshold * sigma] <- NA
  x
}
