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
  # targets::tar_load(data_clean)
  # d <- data_clean
  d <- data_clean %>%
    select(n_valid_hours, accelerometer_wear_location,
    pa_volume, pa_intensity,
    sleep_duration, sleep_efficiency,
    sleep_onset, sleep_wakeup,
    sleep_regularity,# sleep_onset_time, sleep_wakeup_time,
    sleep_duration, sleep_efficiency,
    sex, age, height, weight, bmi, waist_circumference,
    ses, screen_time, daylight_hours, city, eligible,
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
    "Socioeconomic Status", "Screen Time", "Daylight Hours", "City", "Met Weartime Criteria",
    "Sleep Conditions Reported", "Country", "Season", "Study ID", "Participant ID")

  # Tabulate the number of observations
  d %>% select(-studyid, -participant_id) %>%
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                                all_categorical() ~ "{n} ({p}%)"),
    digits = all_continuous() ~ 2,
    missing = "no") %>%
    as_gt() %>%
<<<<<<< HEAD
    gtsave("tables/observations_table_one.html")

    # Now across each participant_id, summarise all variables.
=======
    gtsave("tables/observations_table_one.png")
  
  # Now across each participant_id, summarise all variables.
>>>>>>> 1388336572f5b4f8d0c88c83d811c68093ce3d64
  # For factor variables, pick the most common.
  # For numeric variables, take the mean.
  find_max  <- function(x) {names(which.max(table(x)))}
  participants <- d %>%
    group_by(participant_id) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE),
              across(where(is.factor), find_max),
              across(where(is.logical), any)) %>%
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
    "Study ID",
    "Any Observations Met Weartime Criteria")

  participants %>% select(-studyid, -participant_id) %>%
  tbl_summary(statistic = list(all_continuous() ~ "{mean} ({sd})",
                                all_categorical() ~ "{n} ({p}%)"),
    digits = all_continuous() ~ 2,
    missing = "no") %>%
    as_gt() %>%
<<<<<<< HEAD
    gtsave("tables/participants_table_one.html")

# Supplement:
=======
    gtsave("tables/participants_table_one.png")
 
# Supplement: 
>>>>>>> 1388336572f5b4f8d0c88c83d811c68093ce3d64
# Table with characteristics of included vs excluded participants

participants %>% select(-participant_id) %>%
  mutate(eligible = as.factor(ifelse(eligible, "Has eligible data", "No eligible observations"))) %>%
  tbl_summary(by = eligible,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                                all_categorical() ~ "{n} ({p}%)"),
    digits = all_continuous() ~ 2,
    missing = "no") %>%
    as_gt() %>%
    gtsave("tables/participants_by_any_eligible_data.png")

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
    gtsave("tables/participants_by_pa_sleep.png")

studies_by_age <- participants %>% group_by(studyid) %>%
  summarise(mean_age = mean(age, na.rm = TRUE)) %>%
  arrange(mean_age) %>% select(studyid)
# sort study by mean age and plot by study
participants %>% mutate(studyid = factor(studyid,
                                        levels = studies_by_age$studyid)) %>%
                                        select(-participant_id) %>%
  tbl_summary(by = studyid,
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                                all_categorical() ~ "{n} ({p}%)"),
    digits = all_continuous() ~ 2,
    missing = "no") %>%
    as_gt() %>%
    gtsave("tables/participants_by_study.png")
  return(NULL)
}
