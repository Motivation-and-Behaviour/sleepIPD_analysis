# Hard physiological limits.
# pa_volume is daily average ENMO in mg; 200 sits above any achievable 24 h
# average and above the highest legitimate participant in these data (172).
plausible_ranges <- list(
  pa_volume = c(0, 200)
)

# CDC biologically implausible value cut-offs for children
implausible_growth_z <- list(
  height = list(item = "height2_20", limits = c(-5, 4)),
  weight = list(item = "weight2_20", limits = c(-5, 8))
)

# Accelerometer-derived variables nulled when a file is judged miscalibrated.
bad_accel_vars <- c(
  "pa_volume",
  "pa_intensity",
  "pa_intensity_m16",
  "pa_mostactivehr",
  "pa_mvpa"
)

# The variables the +/-4SD screen applies to.
outlier_screen_vars <- c(
  "pa_volume",
  "pa_intensity",
  "pa_intensity_m16",
  "pa_mvpa",
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

#' apply_plausibility_bounds
#'
#' Set values outside a fixed physiological range to NA.
#'
#' @param data data frame
#' @param ranges named list of `c(lower, upper)`, one per column to bound
#'
#' @details Must run **before** `remove_outliers()`.
apply_plausibility_bounds <- function(data, ranges = plausible_ranges) {
  missing_cols <- setdiff(names(ranges), names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Columns not found in data: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  for (var in names(ranges)) {
    limits <- ranges[[var]]
    x <- data[[var]]
    x[!is.na(x) & (x < limits[1] | x > limits[2])] <- NA
    data[[var]] <- x
  }

  data
}

#' remove_implausible_growth
#'
#' Set children's height and weight to NA when they are implausible for their
#' age and sex, scored against the CDC 2000 growth reference.
#'
#' @param data data frame with `age`, `sex`, `height` and `weight`
#' @param cutoffs see `implausible_growth_z`
#' @param adult_age age from which the child reference no longer applies
#'
#' @details Must run **before** BMI is calculated.
remove_implausible_growth <- function(
  data,
  cutoffs = implausible_growth_z,
  adult_age = 18
) {
  child <- !is.na(data$age) & data$age < adult_age

  for (var in names(cutoffs)) {
    z <- childsds::sds(
      value = data[[var]][child],
      age = data$age[child],
      sex = as.character(data$sex[child]),
      item = cutoffs[[var]]$item,
      ref = childsds::cdc.ref,
      type = "SDS",
      male = "Male",
      female = "Female"
    )
    limits <- cutoffs[[var]]$limits
    x <- data[[var]][child]
    x[!is.na(z) & (z < limits[1] | z > limits[2])] <- NA
    data[[var]][child] <- x
  }

  data
}

#' null_bad_accel_files
#'
#' Null every acceleration-derived variable for files whose mean `var` breaches
#' `limit`.
#'
#' @param data data frame
#' @param limit mean above which the file is treated as miscalibrated
#' @param by column identifying the accelerometer file
#' @param var the variable the limit applies to
#' @param vars columns to set to NA for a flagged file

null_bad_accel_files <- function(
  data,
  limit = 200,
  by = "participant_id",
  var = "pa_volume",
  vars = bad_accel_vars
) {
  missing_cols <- setdiff(c(by, var, vars), names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Columns not found in data: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  file_mean <- tapply(data[[var]], data[[by]], mean, na.rm = TRUE)
  bad <- names(file_mean)[!is.na(file_mean) & file_mean > limit]
  flagged <- data[[by]] %in% bad

  for (v in vars) {
    data[[v]][flagged] <- NA
  }

  data$accel_file_flagged <- flagged

  data
}

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
