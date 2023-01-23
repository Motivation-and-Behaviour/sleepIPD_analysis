#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_joined
#' @return
#' @author
#' @export
clean_data <- function(data_joined) {
  data_joined %>%
    clean_names() %>%
    # Remove problem studies
    # TODO: Remove when data are fixed
    filter(!studyid %in%
      c(
        "data/100_ISCOLE.csv", "data/102_Camilla.csv", "data/115_Angelica.csv"
      )) %>%
    # Rename data
    rename(
      pa_volume = acc_day_mg,
      pa_intensity = ig_gradient,
      sleep_duration = dur_spt_sleep_min, # TODO: confirm
      sleep_efficiency = sleep_efficiency,
      sleep_onset = sleeponset_p5,
      sleep_wakeup = wakeup_p5,
      sleep_onset_time = sleeponset_ts_p5,
      sleep_wakeup_time = wakeup_ts_p5,
      sleep_regularity = sleep_regularity_index
    ) %>%
    mutate(across(c(
      pa_volume, pa_intensity, sleep_duration, sleep_efficiency, sleep_onset,
      sleep_wakeup, sleep_regularity, age, n_valid_hours, weight
    ), as.numeric)) %>%
    # Filter for OK data
    # TODO: Decision rules for this
    filter((n_valid_hours > 10) &
      (is.na(sleep_duration) | sleep_duration > 300))
}
