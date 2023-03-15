#' demographic_table
#'
#' Create a demographic table based on Mike's data
#' @description I've added in this next segment in order to have a data.frame version of the participant table
#' Which we can use with papaja. The gtsummary version does not allow for rowspans and was probably
#' too long to include in the paper.

make_demog_table <- function(participant_summary){

participants <- participant_summary
# First I select all eligible participants
participants <- dplyr::filter(participants, elible = TRUE) |>
  # I remove variables we don't want in the table
  dplyr::select(
    -c(waist_circumference, screen_time, daylight_hours, city, studyid, eligible,
       height, weight, sleep_wakeup))

# Create age bins with specified age points
age_breaks <- c(0, 11, 18, 35, 65, Inf)
age_labels <- c(glue::glue("{floor(min(participants$age, na.rm = TRUE))}-11 years"), "12-18 years", "19-35 years", "36-65 years", "66+ years")

# Assign age categories to the 'age_cat' column in the 'participants' data frame
participants$age_cat <-
  cut(
    participants$age,
    breaks = age_breaks,
    labels = age_labels,
    include.lowest = TRUE,
    right = FALSE,
    ordered_result = TRUE
  )

# I store the variable labels for use in the table and add region
participant_labels <- var_label(participants)

# I select all character variables and create a long from dataset
participants_character <- dplyr::select(participants, where(is.character) | where(is.factor)) |>
  tidyr::pivot_longer(-c(participant_id, age_cat)) |>
  data.table()
# I select all numeric variables and create a long form dataset
participants_numeric <- dplyr::select(participants, where(is.numeric), participant_id, age_cat) |>
  tidyr::pivot_longer(-c(participant_id, age_cat)) |>
  data.table()

# For the numeric variables I calculate means and SDs
participants_numeric <- participants_numeric[, {
  m = papaja::print_num(mean(value, na.rm = TRUE))
  sd = papaja::print_num(sd(value, na.rm = TRUE))
  .(out = glue::glue("{m} ({sd})"), variable = "Numeric variables")}, by = c("name", "age_cat")]


Ns <-
  data.table::data.table(participants)[, .(
    out = glue::as_glue(format(length(unique(participant_id)), big.mark = ",")),
      level = "N",
      variable = "Numeric variables"
    ),
    by = "age_cat"]

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
                                                 by = c("name","age_cat")]

# I join these datasets together
tab1 <- rbindlist(list(Ns, participants_numeric, participants_character), fill = TRUE)
# There were a few people for which we could not determine their age
tab1 <- tab1[!is.na(tab1$age_cat)]
tab1 <- tab1 |> tidyr::pivot_wider(values_from = out, names_from = age_cat)

# Pivot wider is annoying and ignores factor levels
non_age_names <- names(tab1)[!names(tab1) %in% age_labels]
tab1 <- tab1[, c(non_age_names, age_labels)]

# I recode the names using variable labels
tab1$name <- dplyr::recode(tab1$name, !!!participant_labels)
tab1$level[is.na(tab1$level)] <- tab1$name[is.na(tab1$level)]
tab1$level <- stringr::str_to_sentence(tab1$level)
tab1$level[tab1$level == "Bmi"] <- "BMI"
tab1$level <- gsub("Pa", "PA", tab1$level)
# I want to have all numeric variables under a single row span so replace their name.
# The categorical variables will each get their own rowspan
tab1$name[tab1$variable == "Numeric variables"] <- "Numeric variables"
tab1[is.na(tab1)] <- " - "
out_tab <- tab1 |> dplyr::select(-c(variable, name)) |>
  dplyr::rename(Characteristic = level) |>
  split(tab1$name)

out_tab <-
  append(out_tab["Numeric variables"], out_tab[!names(out_tab) %in% "Numeric variables"])

# Reorder socioeconomic status
out_tab$`Socioeconomic Status`$Characteristic <-
  factor(out_tab$`Socioeconomic Status`$Characteristic,
         levels = c("Low", "Medium", "High"))
out_tab$`Sleep Conditions Reported` <-
  out_tab$`Sleep Conditions Reported`[out_tab$`Sleep Conditions Reported`$Characteristic == "Yes",]

out_tab$`Socioeconomic Status` <-
  out_tab$`Socioeconomic Status`[order(out_tab$`Socioeconomic Status`$Characteristic), ]

out_tab
}
