#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_holdout
#' @return gt object with table1
#' @author noetel
#' @export
make_table1 <- function(data_clean) {
  # targets::tar_make()
  # d <- targets::tar_load(data_clean)
  d <- data_clean %>%
    select(n_valid_hours, accelerometer_wear_location,
    pa_volume, pa_intensity,
    sleep_duration, sleep_efficiency,
    sleep_onset, sleep_wakeup,
    sleep_regularity,# sleep_onset_time, sleep_wakeup_time, 
    sleep_duration, sleep_efficiency,
    sex, age, height, weight, bmi, waist_circumference,
    ses, screen_time, daylight_hours, city,
    sleep_conditions, country, season, studyid, participant_id) %>%
    mutate(studyid = as.factor(studyid),
            city = as.factor(city),
            season = as.factor(season))
  # Label everything nicely
  var_label(d) <- c("Valid Weartime Hours",
    "Accelerometer Wear Location",
    "PA Volume",
    "PA Intensity",
    "Sleep Duration",
    "Sleep Efficiency",
    "Sleep Onset",
    "Sleep Wakeup", "Sleep Regularity",
    "Sex", "Age", "Height", "Weight", "BMI", "Waist Circumference",
    "Socioeconomic Status", "Screen Time", "Daylight Hours", "City",
    "Sleep Conditions Reported", "Country", "Season", "Study ID", "Participant ID")
  
  # Tabulate the number of observations
  d %>% select(-studyid, -participant_id) %>% 
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                                all_categorical() ~ "{n} ({p}%)"),
    digits = all_continuous() ~ 2,
    missing = "no") %>%
    as_gt() %>%
    gtsave("tables/observations_table_one.html")
  
  # Now across each participant_id, summarise all variables.
  # For factor variables, pick the most common.
  # For numeric variables, take the mean.
  find_max  <- function(x) {names(which.max(table(x)))}
  participants <- d %>%
    group_by(participant_id) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE),
              across(where(is.factor), find_max)) %>%
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
    "Country",
    "Season",
    "Study ID")

  participants %>% select(-studyid, -participant_id) %>% 
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                                all_categorical() ~ "{n} ({p}%)"),
    digits = all_continuous() ~ 2,
    missing = "no") %>%
    as_gt() %>%
    gtsave("tables/participants_table_one.html")
 
# Supplement: 
# Table with characteristics of included vs excluded participants


# Tables with characteristics of participants with high and low sleep duration / PA volume
participants$pa_sleep_cat <- case_when(
  participants$pa_volume > median(participants$pa_volume, na.rm = TRUE) &
    participants$sleep_duration > median(participants$sleep_duration, na.rm = TRUE) ~ "High PA, High Sleep",
  participants$pa_volume <= median(participants$pa_volume, na.rm = TRUE) &
    participants$sleep_duration > median(participants$sleep_duration, na.rm = TRUE) ~ "Low PA, High Sleep",
  participants$pa_volume > median(participants$pa_volume, na.rm = TRUE) &
    participants$sleep_duration <= median(participants$sleep_duration, na.rm = TRUE) ~ "High PA, Low Sleep",
  participants$pa_volume <= median(participants$pa_volume, na.rm = TRUE) &
    participants$sleep_duration <= median(participants$sleep_duration, na.rm = TRUE) ~ "Low PA, Low Sleep",
  TRUE ~ "NA"
)
participants$pa_sleep_cat[participants$pa_sleep_cat == "NA"] <- NA
 participants %>% select(-studyid, -participant_id) %>%
  tbl_summary(by = pa_sleep_cat,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                                all_categorical() ~ "{n} ({p}%)"),
    digits = all_continuous() ~ 2,
    missing = "no") %>%
    as_gt() %>%
    gtsave("tables/participants_by_pa_sleep.html")
  return(NULL)
}
