#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_imp
#' @return
#' @author Taren Sanders
#' @export
check_imps <- function(data_imp) {
  imps <- data.table(mice::complete(data_imp, action = "long", include = TRUE))

  imps_long <- imps %>%
    dplyr::select(
      ".imp", ".id", "pa_volume", "pa_intensity", "pa_intensity_m16",
      "sleep_duration", "sleep_efficiency", "sleep_onset", "sleep_wakeup",
      "sleep_regularity", "sleep_efficiency_lag", "sleep_onset_lag",
      "sleep_wakeup_lag", "sleep_regularity_lag", "sleep_duration_lag",
      "age", "weight", "height", "bmi", "daylight_hours", "pa_mostactivehr"
    ) %>%
    reshape2::melt(c(".imp", ".id")) %>%
    dplyr::mutate(
      imputed = dplyr::if_else(.imp == 0, "Observed", "Imputed"),
      value = as.numeric(value)
    )
}