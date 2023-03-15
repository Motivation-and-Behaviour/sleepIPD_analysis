#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param model_list
#' @return list of tables
#' @author Taren Sanders
#' @export
make_model_tables <- function(model_list) {
  note <- "Adjusted for SES, BMI, and sex."

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
    "Physical activity on sleep controlling for SES, gender and BMI"
  sleep_table$note <- note
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
    "Sleep on physical activity controlling for SES, gender and BMI"
  pa_table$note <- note
  pa_table$col_spanners <- list(
    "Physical Activity Volume" = c(2, 5),
    "Physical Activity Intensity" = c(6, 9)
  )

  return(list(sleep = sleep_table, physical_activity = pa_table))
}

format_table <- function(tab) {
  tab$term <- gsub("I\\(", "", tab$term) |>
    gsub("_", " ", x = _) |>
    gsub("\\^2\\)", "$^2$", x = _)
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
