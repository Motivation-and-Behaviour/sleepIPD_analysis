# The day-level sleep variables that get a `_lag` counterpart.
sleep_lag_source_vars <- c(
  "sleep_efficiency",
  "sleep_onset",
  "sleep_wakeup",
  "sleep_regularity",
  "sleep_duration"
)

#' add_sleep_lags
#'
#' Add `*_lag` columns holding each sleep variable's value on the previous day.
#'
#' @param data day-level data containing `calendar_date`
#' @param by character vector identifying a participant (add `.imp` for
#'   completed imputations, so each imputation is lagged separately)
#'
#' @details The lag is NA unless the immediately preceding calendar day is also
#' a row in `data`. Called twice: once in `clean_data()` on observed data, and
#' once in `make_data_imp()` on the completed imputations. Deriving the lags
#' after imputation is what keeps `sleep_x_lag` on day *t* equal to `sleep_x` on
#' day *t-1*; imputing them as separate variables did not.
#'
#' Returns `data` sorted by `by` then `calendar_date`.
add_sleep_lags <- function(data, by) {
  needed <- c(by, "calendar_date", sleep_lag_source_vars)
  missing_cols <- setdiff(needed, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "Columns not found in data: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  data |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(by, "calendar_date")))) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(by))) |>
    dplyr::mutate(dplyr::across(
      dplyr::all_of(sleep_lag_source_vars),
      ~ ifelse(
        dplyr::lag(calendar_date) == calendar_date - 1,
        dplyr::lag(.x),
        NA
      ),
      .names = "{.col}_lag"
    )) |>
    dplyr::ungroup()
}
