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
    filter(eligible) %>%
    select(
      participant_id, studyid, acc_wear_loc, age, bmi, city, country,
      daylight_hours, eligible, ethnicity, height, n_valid_hours, pa_intensity,
      pa_volume, region, screen_time, season, ses, sex, sleep_conditions,
      sleep_duration, sleep_efficiency, sleep_onset, sleep_regularity,
      sleep_wakeup, waist_circumference, weight
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
      across(where(is.logical), any),
      n_valid_days = n()
    ) %>%
    mutate(sleep_conditions = as.factor(sleep_conditions))

  # Create age bins with specified age points
  age_breaks <- c(0, 11, 18, 35, 65, Inf)
  age_labels <-
    c(glue::glue(
      "{floor(min(participants$age, na.rm = TRUE))}-11 years"
    ), "12-18 years", "19-35 years", "36-65 years", "66+ years")

  # Assign age categories to the 'age_cat' column
  participants$age_cat <-
    cut(
      participants$age,
      breaks = age_breaks,
      labels = age_labels,
      include.lowest = TRUE,
      right = FALSE,
      ordered_result = TRUE
    )

  var_label(participants) <- list(
    participant_id = "Participant ID",
    age = "Age",
    age_cat = "Age Category",
    bmi = "BMI",
    daylight_hours = "Daylight Hours",
    height = "Height",
    n_valid_hours = "Valid Weartime Hours",
    n_valid_days = "Valid Weatime Days",
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
    studyid = "Study ID"
  )

  participants
}
