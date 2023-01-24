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
    sex, age, height, weight, bmi,
    ses, screen_time, 
    sleep_conditions, country, season, studyid, participant_id) %>%
    mutate(studyid = as.factor(studyid))
  # Tabulate the number of observations
  observations_tab_one <- tableone::CreateTableOne(data = d,
  includeNA = TRUE)
  write_obs <- print(observations_tab_one,
  quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
## Save to a CSV file
write.csv(write_obs, file = "tables/observations_table_one.csv")
  
  # Now across each participant_id, summarise all variables.
  # For factor variables, pick the most common.
  # For numeric variables, take the mean.
  find_max  <- function(x) {names(which.max(table(x)))}
  participants <- d %>%
    group_by(participant_id) %>%
    summarise(across(where(is.numeric), mean, na.rm = TRUE),
    across(where(is.factor), find_max))
  
  participants_table_one <- tableone::CreateTableOne(data = select(participants,
  -studyid, -participant_id),
  includeNA = TRUE)
  write_participants <- print(participants_table_one,
  quote = FALSE, noSpaces = TRUE, printToggle = FALSE)
  ## Save to a CSV file
  write.csv(write_participants, file = "tables/participants_table_one.csv")

  return(list(observations_tab_one, participants_table_one))
}
