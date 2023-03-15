#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_joined
#' @return
#' @author
#' @export
clean_data <- function(data_joined, region_lookup) {
  data_gsheet <- "https://docs.google.com/spreadsheets/d/1A75Qk8mNXygxcsCxLQ4maspZsQxZXJ5K-12X338CQ2s/edit#gid=1960479274" # nolint
  d <- data_joined %>%
    clean_names() %>%
    # Rename data
    rename(
      pa_volume = acc_day_mg,
      pa_intensity = ig_gradient,
      sleep_duration = dur_spt_sleep_min,
      sleep_efficiency = sleep_efficiency,
      sleep_onset = sleeponset_p5,
      sleep_wakeup = wakeup_p5,
      sleep_onset_time = sleeponset_ts_p5,
      sleep_wakeup_time = wakeup_ts_p5,
      sleep_regularity = sleep_regularity_index
    ) %>%
    # remove all the ggir data execpt the columns that we're using
    select(
      -ig_gradient_enmo_0_24hr:-thresh_wear_loc, -contains("guider_"), -id,
      measurementday, pa_volume, pa_intensity, sleep_duration, sleep_efficiency,
      sleep_onset, sleep_wakeup, sleep_onset_time, sleep_wakeup_time,
      sleep_regularity
    ) %>%
    # convert numeric variables to numeric
    mutate(across(c(
      pa_volume, pa_intensity, sleep_duration, sleep_efficiency, sleep_onset,
      sleep_wakeup, sleep_regularity, age, n_valid_hours, n_hours,
      measurementday, weight, height, screen_time, waist_circumference,
      maturational_status
    ), as.numeric)) %>%
    # convert charcter variables to factors
    mutate(across(c(
      studyid, sex, ethnicity,
      ses, sleep_medications, sleep_conditions,
      country, season, accelerometer_wear_location,
      weekday_x, accelerometer_model
    ), as.factor)) %>%
    # convert calendar_date to date
    mutate(calendar_date = as.Date(calendar_date)) %>%
    # Add participant ID using the study and filename
    mutate(participant_id = paste(as.numeric(studyid), filename, sep = "_")) %>%
    # Remove rows with no valid hours, these are not really 'observations'
    filter(n_valid_hours > 0) %>%
    # Set data with indicated problems to missing
    mutate(
      across(
        c(
          sleep_duration, sleep_efficiency, sleep_onset, sleep_wakeup,
          sleep_regularity
        ),
        ~ if_else(sleep_wakeup_time == times("23:59:55"), NA_real_, .x)
      ),
      across(
        c(sleep_onset_time, sleep_wakeup_time),
        ~ if_else(sleep_wakeup_time == times("23:59:55"), NA_character_, .x)
      )
    ) %>%
    # Filter for OK data
    # Decision rules for this
    mutate(eligible = (n_valid_hours > 10) &
      (is.na(sleep_duration) | sleep_duration > 200)) %>%
    remove_outliers(ignore_cols = c("age")) %>%
    # recalculate measurement day by getting the minimum
    # date for each person
    group_by(participant_id) %>%
    mutate(
      day_zero = min(calendar_date),
      measurement_day = as.numeric(calendar_date - day_zero)
    ) %>%
    ungroup() %>%
    # if the new measurement day is absurdly high, use old one,s
    # and fix calendar day using the old one too)
    mutate(
      calendar_date = as.Date(ifelse(measurement_day > 10 * measurementday,
        day_zero + lubridate::days(measurementday),
        calendar_date
      ), "1970-01-01"),
      measurement_day = ifelse(measurement_day > 10 * measurementday,
        measurementday,
        measurement_day
      ),
      # remove implausible heights
      height = ifelse(height > 30, height, NA),
      # also calculate bmi
      bmi = weight / ((height / 100)^2),
      sleep_onset_time = chron(times = sleep_onset_time),
      sleep_wakeup_time = chron(times = sleep_wakeup_time)
    ) %>%
    select(-measurementday)
  # read in sleep conditions harmonisation data

  sleep_refactors <-
    googlesheets4::read_sheet(data_gsheet,
      sheet = "Sleep conditions",
      col_types = "c",
      range = "A1:C50"
    ) %>%
    remove_empty(which = "rows") %>%
    clean_names() %>%
    mutate(studyid = as.numeric(studyid))

  # change study_id to a number to match against harmonisation sheet
  d <- d %>% mutate(studyid = parse_number(as.character(studyid)))

  d$sleep_conditions[d$studyid == 110 & is.na(d$sleep_conditions)] <-
    "No sleep apnea"
  # when sleep_conditions matches column 2,
  # and studyid matches column 1, replace with column 3
  d <- d %>%
    left_join(sleep_refactors,
      by = c(
        "studyid" = "studyid",
        "sleep_conditions" = "sleep_conditions"
      )
    ) %>%
    mutate(sleep_conditions = as.factor(harmonized)) %>%
    select(-harmonized)

  # do the same thing for ses
  ses_refactors <-
    googlesheets4::read_sheet(data_gsheet,
      sheet = "SES",
      col_types = "c",
      range = "A1:C100"
    ) %>%
    remove_empty(which = "rows") %>%
    clean_names() %>%
    mutate(
      studyid = as.numeric(studyid),
      ses = tolower(ses)
    )
  d <- d %>%
    mutate(ses = tolower(ses)) %>%
    left_join(ses_refactors,
      by = c(
        "studyid" = "studyid",
        "ses" = "ses"
      )
    ) %>%
    mutate(ses = factor(harmonized, levels = c("Low", "Medium", "High"))) %>%
    select(-harmonized)

  d$age_cat <- cut(d$age,
    breaks = c(0, 11, 18, 35, 65, 100),
    labels = c(
      "0-11 years", "12-18 years", "19-35 years", "36-65 years", "65+ years"
    )
  )

  # removing some variables we can't harmonise
  d <- d %>% select(
    -sleep_medications,
    -ethnicity,
    -maturational_status
  )

  # Clean the country names
  d <- d %>%
    mutate(country = str_to_title(country)) %>%
    mutate(
      country = case_when(
        country == "España" ~ "Spain",
        country == "Españ" ~ "Spain",
        country == "Marruecos" ~ "Morocco",
        country == "Rumania" ~ "Romania",
        country == "Ucrania" ~ "Ukraine",
        country == "Czechia" ~ "Czech Republic",
        country == "Us" ~ "United States",
        country == "Uk" ~ "United Kingdom",
        studyid == 101 ~ "Switzerland",
        studyid == 103 ~ "New Zealand",
        studyid == 104 ~ "Brazil",
        studyid == 108 ~ "Australia",
        studyid == 110 ~ "Finland",
        studyid == 112 ~ "Spain",
        studyid == 114 ~ "United States",
        studyid == 117 ~ "Spain",
        studyid == 118 ~ "Australia",
        TRUE ~ country
      ),
      city = case_when(
        # 100 ISCOLE
        studyid == 100 & country == "United States" ~ "Baton Rouge",
        studyid == 100 & country == "United Kingdom" ~ "Bath",
        studyid == 100 & country == "Australia" ~ "Adelaide",
        studyid == 100 & country == "Portugal" ~ "Porto",
        studyid == 100 & country == "South Africa" ~ "Cape Town",
        studyid == 100 & country == "Kenya" ~ "Nairobi",
        studyid == 100 & country == "Colombia" ~ "Bogotá",
        studyid == 100 & country == "Brazil" ~ "São Paulo",
        studyid == 100 & country == "Canada" ~ "Ottawa",
        studyid == 100 & country == "China" ~ "Tianjin",
        studyid == 100 & country == "India" ~ "Bangalore",
        studyid == 100 & country == "Finland" ~ "Helsinki",
        # Other studies
        studyid == 101 ~ "Lausanne",
        studyid == 103 ~ "Dunedin",
        studyid == 104 ~ "Florianópolis",
        studyid == 105 ~ "Prague",
        studyid == 108 ~ "Sydney",
        studyid == 110 ~ "Southwest Finland",
        studyid == 112 ~ "Seville",
        studyid == 117 ~ "Madrid",
        studyid == 118 ~ "Sydney",
        TRUE ~ city
      )
    )

  d <- d %>% mutate(
    country = as.factor(country),
    location = paste(city, country, sep = ", ")
  )

  d <- dplyr::left_join(d, region_lookup, by = "country")

  locations <- unique(d$location)
  locations <- locations[!is.na(locations)]
  update_latlong(locations)
  latlong <- read.csv("latlong.csv") %>% select(-X)
  d <- d %>% left_join(latlong, by = "location")


  # use suncalc to find sunrise and sunset times
  sunlight <- d %>%
    rename(date = calendar_date) %>%
    getSunlightTimes(
      data = .,
      keep = c("sunrise", "sunset")
    )

  sunlight$daylight_hours <- sunlight$sunset - sunlight$sunrise

  d <- d %>% left_join(distinct(sunlight),
    by = c(
      "calendar_date" = "date",
      "lat", "lon"
    )
  )

  d$season <- mapply(get_season, date = d$calendar_date, lat = d$lat)

  d <- d %>%
    mutate(daylight_hours = as.numeric(daylight_hours)) %>%
    select(-lat, -lon, -sunrise, -sunset, -location)

  # Remove some duplicated rows where data are mostly missing
  d <- d %>%
    # Sort desc by valid hours because `distinct` takes first row
    arrange(studyid, filename, calendar_date, desc(n_valid_hours)) %>%
    distinct(studyid, filename, calendar_date, .keep_all = TRUE)

  # Add lagged sleep variables
  d <- d %>%
    arrange(studyid, filename, calendar_date) %>%
    group_by(studyid, filename) %>%
    mutate(across(
      c(
        sleep_efficiency, sleep_onset, sleep_wakeup, sleep_onset_time,
        sleep_wakeup_time, sleep_regularity, sleep_duration
      ),
      ~ ifelse(lag(calendar_date) == calendar_date - 1, lag(.x), NA),
      .names = "{.col}_lag"
    )) %>%
    ungroup()

  d
}
