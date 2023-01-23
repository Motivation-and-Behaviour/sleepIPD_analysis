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
    # Remove problem studies
    # TODO: Remove when data are fixed
    filter(!studyid %in%
      c(
        "data/100_ISCOLE.csv", "data/102_Camilla.csv", "data/115_Angelica.csv"
      )) %>%
    # Rename data
    rename(
      pa_volume = ACC_day_mg,
      pa_intensity = ig_gradient,
      sleep_duration = dur_spt_sleep_min, # TODO: confirm
      sleep_efficiency = sleep_efficiency,
      sleep_onset = sleeponset_p5,
      sleep_wakeup = wakeup_p5,
      sleep_onset_time = sleeponset_ts_p5,
      sleep_wakeup_time = wakeup_ts_p5,
      sleep_regularity = SleepRegularityIndex
    ) %>% janitor::clean_names() %>%
    # Filter for OK data
    # TODO: Decision rules for this
    filter((`N valid hours` > 10) &
      (is.na(sleep_duration) | sleep_duration > 300))
}
