#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param model_list
#' @return list of tables
#' @author Taren Sanders
#' @test model_list <- model_list_by_age_log
#' @export
make_model_tables <- function(model_list) {
  recode_var <- c("ses"= "SES" ,"bmi" = "BMI", "studyid" = "the fixed effects of study IDs")
  control_vars <- model_list[[1]]$control_vars |>
    dplyr::recode(!!!recode_var)

  control_vars <- paste(control_vars, collapse = ", ") |>
    # replace last comma with and
    gsub(",([^,]+)$", ", and\\1", x = _, perl = TRUE)

  note <- paste("Adjusted for", control_vars)

  sleep_vars <- list(
    `Sleep duration` = "scale_sleep_duration",
    `Sleep efficency` = "scale_sleep_efficiency",
    `Sleep onset` = "scale_sleep_onset",
    `Sleep regularity` = "scale_sleep_regularity"
  )

  pa_vars <- list(
    `Physical activity volume` = "scale_pa_volume",
    `Physical activity intensity` = "scale_pa_intensity"
  )

  if(attr(model_list, "use_log")) {
    pa_vars$`Physical activity volume` <- "log_pa_volume"
  }

  sleep_table <- list()

  sleep_table$data <- lapply(sleep_vars, function(sleep) {
    model1_name <- paste(
      sleep, "by", pa_vars["Physical activity volume"],
      collapse = " "
    )
    model2_name <- paste(
      sleep, "by", pa_vars[["Physical activity intensity"]],
      collapse = " "
    )

    tab <- cbind(
      format_table(model_list[[model1_name]]$table),
      format_table(model_list[[model2_name]]$table)
    )
    tab[, c(1:5, 7:10)]
  })

  sleep_table$caption <-
    glue::glue("Physical activity predicting sleep controlling for {control_vars}.")
  sleep_table$note <- paste0(note, ". Outcomes variables are listed in the column headers.")

  sleep_conv_issue <- any(sapply(sleep_table$data, function(x) any(grepl("\\\\dagger", x$"$\\beta$ [95\\% CI]"))))

  if(sleep_conv_issue){
    sleep_table$note <- paste0(sleep_table$note, ". $^\\dagger$ value came from a pooled model where fewer than 75\\% of models converged.")
  }

  sleep_table$col_spanners <- list(
    "Physical Activity Volume" = c(2, 5),
    "Physical Activity Intensity" = c(6, 9)
  )

  pa_table <- list()
  pa_table$data <- lapply(sleep_vars, function(sleep) {
    model1_name <- paste(
      pa_vars["Physical activity volume"], "by", paste0(sleep, "_lag"),
      collapse = " "
    )
    model2_name <- paste(
      pa_vars[["Physical activity intensity"]], "by", paste0(sleep, "_lag"),
      collapse = " "
    )

    tab <- cbind(
      format_table(model_list[[model1_name]]$table),
      format_table(model_list[[model2_name]]$table)
    )
    tab[, c(1:5, 7:10)]
  })

  pa_table$caption <-
    glue::glue("Sleep predicting physical activity controlling for {control_vars}")
  pa_table$note <- paste0(note, ". Outcomes variables are listed in the row headers.")

  pa_conv_issue <- any(sapply(pa_table$data, function(x) any(grepl("\\\\dagger", x$"$\\beta$ [95\\% CI]"))))

  if(pa_conv_issue){
    pa_table$note <- paste0(pa_table$note, ". $^\\dagger$ value came from a pooled model where fewer than 75\\% of models converged.")
  }

  if(attr(model_list, "use_log")) {
  pa_table$note <- paste0(pa_table$note, "PA volume was log-transformed.")
  }

  pa_table$col_spanners <- list(
    "Physical Activity Volume" = c(2, 5),
    "Physical Activity Intensity" = c(6, 9)
  )

  return(list(sleep = sleep_table, physical_activity = pa_table))
}

#' format_table
#'
#' A function for taking raw table output and preparing it for publication
#' @param tab data.frame object
#' @param conv_daggers a bool. If true, daggers will be converted to double daggers.

format_table <- function(tab, conv_daggers = FALSE) {
  tab$term <- gsub("I\\(", "", tab$term) |>
    gsub("_", " ", x = _) |>
    gsub("\\^2\\)", "$^2$", x = _) |>
    gsub("accelerometer wear location", "", x = _ )
  tab <- tab[!grepl("^ses", tab$term), ]
  tab <- tab[!grepl("^sex", tab$term), ]
  tab <- tab[!grepl("^bmi", tab$term), ]
  tab$term <- stringr::str_to_sentence(tab$term)
  tab$term <- gsub(
    "(S|s)cale pa (intensity|volume)", "Physical activity", tab$term
  )
  tab$term <- gsub(
    "(S|s)cale sleep", "Sleep", tab$term
  )
  tab$term <- gsub("\\slag", "", tab$term)
  tab$term <- gsub(":", " $\\\\times$ ", tab$term)
  names(tab) <- c("Term", "$\\beta$ [95\\% CI]", "SE", "t", "p")
  tab
}

