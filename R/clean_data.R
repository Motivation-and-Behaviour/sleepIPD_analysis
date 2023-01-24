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
  #tar_load(data_joined)
  d <- data_joined %>%
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
     # remove all the ggir data execpt the columns that we're using
    select(-ig_gradient_enmo_0_24hr:-thresh_wear_loc,
    -contains("guider_"), -id,
    pa_volume, pa_intensity, sleep_duration, sleep_efficiency, sleep_onset,
    sleep_wakeup, sleep_onset_time, sleep_wakeup_time, sleep_regularity
    ) %>%
    # convert numeric variables to numeric
    mutate(across(c(
      pa_volume, pa_intensity, sleep_duration, sleep_efficiency, sleep_onset,
      sleep_wakeup, sleep_regularity, age, n_valid_hours, n_hours, measurementday,
      weight, height, screen_time, waist_circumference, maturational_status
    ), as.numeric)) %>%

    # convert charcter variables to factors
    mutate(across(c(studyid, sex, ethnicity, sleep_medications, sleep_conditions,
    country, season, accelerometer_wear_location,
    weekday_x, ethnicity,
    ), as.factor)) %>%

    # convert calendar_date to date
    mutate(calendar_date = as.Date(calendar_date)) %>%
    #   calendar_date, measurementday, sleep_onset_time, sleep_wakeup_time
    # ), as.Date)) %>%

    # Add participant ID using the study and filename
    mutate(participant_id = paste(as.numeric(studyid), filename, sep = "_")) %>%

    # Filter for OK data
    # TODO: Decision rules for this
    filter((n_valid_hours > 10) &
      (is.na(sleep_duration) | sleep_duration > 300)) %>%
    remove_outliers(ignore_cols = c("age")) %>%

    # recalculate measurement day by getting the minimum
    # date for each person
    group_by(participant_id) %>%
    mutate(day_zero = min(calendar_date),
           measurement_day = as.numeric(calendar_date - day_zero)) %>%
    ungroup() %>%
           # if the new measurement day is absurdly high, use old one,s
           # and fix calendar day using the old one too)
    mutate(calendar_date = as.Date(ifelse(measurement_day > 10 * measurementday,
                                    day_zero + lubridate::days(measurementday),
                                    calendar_date), "1970-01-01"),
            measurement_day = ifelse(measurement_day > 10 * measurementday,
                                    measurementday,
                                    measurement_day),
            # also calculate bmi
            bmi = weight / ((height / 100)^2),
            sleep_onset_time = chron(times = sleep_onset_time),
            sleep_wakeup_time = chron(times = sleep_wakeup_time)) %>%
            select(-measurementday)
  # read in sleep conditions harmonisation data
  sleep_refactors <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1A75Qk8mNXygxcsCxLQ4maspZsQxZXJ5K-12X338CQ2s/edit#gid=1960479274",
  sheet = "Sleep conditions",
  col_types = "c",
  range = "A1:C50") %>%
  remove_empty() %>%
  clean_names() %>%
  mutate(studyid = as.numeric(studyid))

  # change study_id to a number to match against harmonisation sheet
  d <- d %>% mutate(studyid = parse_number(as.character(studyid)))

  d$sleep_conditions[d$studyid == 110 & is.na(d$sleep_conditions)] <- "No sleep apnea"
  # when sleep_conditions matches column 2,
  # and studyid matches column 1, replace with column 3
  d <- d %>%
    left_join(sleep_refactors,
              by = c("studyid" = "studyid",
              "sleep_conditions" = "sleep_conditions")) %>%
    mutate(sleep_conditions = harmonized) %>%
    select(-harmonized)

  # do the same thing for ses
  ses_refactors <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1A75Qk8mNXygxcsCxLQ4maspZsQxZXJ5K-12X338CQ2s/edit#gid=1960479274",
  sheet = "SES",
  col_types = "c",
  range = "A1:C100") %>%
  remove_empty() %>%
  clean_names() %>%
  mutate(studyid = as.numeric(studyid),
  ses = str_to_title(ses))
  d <- d %>%
    left_join(ses_refactors,
              by = c("studyid" = "studyid",
              "ses" = "ses")) %>%
    mutate(ses = factor(harmonized, levels = c("Low", "Medium", "High"))) %>%
    select(-harmonized)

  d$age_cat <- cut(d$age, breaks = c(0, 18, 35, 65, 100), labels = c("0-18 years", "19-35 years", "36-65 years", "65+ years"))

  d
}
