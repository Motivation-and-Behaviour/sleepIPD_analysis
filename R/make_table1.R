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
      acc_wear_loc, age, bmi, city, country, daylight_hours, eligible,
      ethnicity, height, n_valid_hours, pa_intensity, pa_volume, participant_id,
      region, screen_time, season, ses, sex, sleep_conditions, sleep_duration,
      sleep_efficiency, sleep_onset, sleep_regularity, sleep_wakeup, studyid,
      waist_circumference, weight
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

  var_label(participants) <- list(
    participant_id = "Participant ID",
    age = "Age",
    bmi = "BMI",
    daylight_hours = "Daylight Hours",
    height = "Height",
    n_valid_hours = "Valid Weartime Hours",
    pa_intensity = "PA Intensity",
    pa_volume = "PA Volume",
    screen_time = "Screen Time",
    sleep_duration = "Sleep Duration",
    sleep_efficiency = "Sleep Efficiency",
    sleep_onset = "Sleep Onset",
    sleep_regularity = "Sleep Regularity",
    sleep_wakeup = "Sleep Wakeup",
    waist_circumference = "Waist Circumference",
    weight = "Weight",
    acc_wear_loc = "Accelerometer Wear Location",
    city = "City",
    ethnicity = "Ethnicity",
    region = "Region",
    season = "Season",
    ses = "Socioeconomic Status",
    sex = "Sex",
    sleep_conditions = "Sleep Conditions Reported",
    studyid = "Study ID",
    eligible = "Any Observations Met Weartime Criteria"
  )

  participants
}
