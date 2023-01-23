#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_clean
#' @return gt object with table1
#' @author noetel
#' @export
make_table1 <- function(data_clean) {
  # targets::tar_make()
  # targets::tar_load(data_clean)
  d <- data_clean
  # names(d)
  # grabbing a range of variables from https://www.bmj.com/content/bmj/suppl/2019/08/19/bmj.l4570.DC1/ekeu048737.ww1.pdf
  # Specifically: studyid,
  # wear_time, pa_volume,
  # pa_intensity, sleep_duration, sleep_efficiency
  # sleep_onset, sleep_regularity,
  # gender, age, height, weight, bmi,
  # waist_circumference, maturational_status,
  # ses, ethnicity, screentime, sleep_medications, conditions,
  # country, city, time_zone, daylight_length, season

  d <- data_clean %>% select(studyid, filename,
  			n_valid_hours, accelerometer_wear_location,
			pa_volume, pa_intensity,
			sleep_duration, sleep_efficiency,
			sleep_onset, sleep_wakeup,
			sleep_onset_time, sleep_wakeup_time, sleep_regularity,
			sleep_duration, sleep_efficiency,
  			sex, age, height, weight,
			ses, ethnicity, screen_time, sleep_medications,
			sleep_conditions, country, city, season) %>%
			group_by(studyid, filename) %>%
			distinct()
			# convert charcter variables to factors
			#mutate(across(studyid, sex, ethinicity,
			#	ses, sleep_medications, sleep_conditions,
			#	country, city, season), as.factor)
			# lots of variables are categorical so take the most common response
			# for each filename 
			# mutate(accelerometer_wear_location = )
			
			


	# bmi, time_zone, daylight_length don't yet exist

	# consider also weight distributions for each range (normal_weight, etc.)
  return(NULL)
}
