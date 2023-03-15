#' demographic_table
#'
#' Create a demographic table based on Mike's data
#' @description I've added in this next segment in order to have a data.frame version of the participant table
#' Which we can use with papaja. The gtsummary version does not allow for rowspans and was probably
#' too long to include in the paper.

make_demog_table <- function(table_1, region_lookup){

participants <- table_1$participants
# First I select all eligible participants
participants <- dplyr::filter(participants, elible = TRUE) |>
  # I simplify country to region
  dplyr::left_join(region_lookup, by = "country") |>
  # I remove variables we don't want in the table
  dplyr::select(
    -c(waist_circumference, screen_time, daylight_hours, city, studyid, country, eligible, pa_sleep_cat,
       height, weight, sleep_wakeup))

# I store the variable labels for use in the table and add region
participant_labels <- var_label(participants)
participant_labels$region <- "Region"

# I select all character variables and create a long from dataset
participants_character <- dplyr::select(participants, where(is.character) | where(is.factor)) |>
  tidyr::pivot_longer(-participant_id) |>
  data.table()
# I select all numeric variables and create a long form dataset
participants_numeric <- dplyr::select(participants, where(is.numeric), participant_id) |>
  tidyr::pivot_longer(-participant_id) |>
  data.table()

# For the numeric variables I calculate means and SDs
participants_numeric <- participants_numeric[, {
  m = papaja::print_num(mean(value, na.rm = TRUE))
  sd = papaja::print_num(sd(value, na.rm = TRUE))
  .(out = glue::glue("{m} ({sd})"), variable = "Numeric variables")}, by = "name"]

# For the character variables I get counts and percents
participants_character <- participants_character[,
                                                 {
                                                   num <- table(value) |>
                                                     data.frame()
                                                   percent <- table(value) |>
                                                     prop.table() |>
                                                     data.frame()

                                                   list(
                                                     level = as.character(num$value),
                                                     out = glue::glue(
                                                       "{format(num$Freq, big.mark = ',')} ({papaja::print_num(percent$Freq*100)}%)"
                                                     )
                                                     , variable = "Categorical variables")

                                                 },
                                                 by = "name"]

# I join these datasets together
tab1 <- rbindlist(list(participants_numeric, participants_character), fill = TRUE)

# I recode the names using variable labels
tab1$name <- dplyr::recode(tab1$name, !!!participant_labels)
tab1$level[is.na(tab1$level)] <-tab1$name[is.na(tab1$level)]
tab1$level <- stringr::str_to_sentence(tab1$level)
tab1$level[tab1$level == "Bmi"] <- "BMI"
tab1$level <- gsub("Pa", "PA", tab1$level)
# I want to have all numeric variables under a single row span so replace their name.
# The categorical variables will each get their own rowspan
tab1$name[tab1$variable == "Numeric variables"] <- "Numeric variables"
tab1

}
