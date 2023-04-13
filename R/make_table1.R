#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_holdout
#' @return gt object with table1
#' @author noetel & conigrave
#' @export
make_participant_summary <- function(data_clean) {
  require(dplyr)
  require(labelled)

  d <- data_clean %>%
    select(
      n_valid_hours, acc_wear_loc,
      pa_volume, pa_intensity,
      sleep_duration, sleep_efficiency,
      sleep_onset, sleep_wakeup,
      sleep_regularity, # sleep_onset_time, sleep_wakeup_time,
      sleep_duration, sleep_efficiency,
      sex, age, height, weight, bmi, waist_circumference,
      ses, screen_time, daylight_hours, city, eligible,
      sleep_conditions, country, season, region, studyid, participant_id
    ) %>%
    mutate(
      studyid = as.factor(studyid),
      city = as.factor(city),
      season = as.factor(season)
    )

  participants <- d %>%
    group_by(participant_id) %>%
    summarise(
      across(where(is.numeric), mean, na.rm = TRUE),
      across(where(is.factor), find_max),
      across(where(is.logical), any)
    ) %>%
    mutate(sleep_conditions = as.factor(sleep_conditions))

  var_label(participants) <- c(
    "Participant ID",
    "Valid Weartime Hours",
    "PA Volume",
    "PA Intensity",
    "Sleep Duration",
    "Sleep Efficiency",
    "Sleep Onset",
    "Sleep Wakeup",
    "Sleep Regularity",
    "Age",
    "Height",
    "Weight",
    "BMI",
    "Waist Circumference",
    "Screen Time",
    "Daylight Hours",
    "Accelerometer Wear Location",
    "Sex",
    "Socioeconomic Status",
    "City",
    "Sleep Conditions Reported",
    "Season",
    "Region",
    "Study ID",
    "Any Observations Met Weartime Criteria"
  )

  participants
}
