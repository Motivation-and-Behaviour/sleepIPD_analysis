create_distinct <- function(df) {
  variables <- c(
    "SES", "Ethnicity", "Sleep conditions", "Maturational status",
    "Sleep medications"
  )

  dir.create("temp", showWarnings = FALSE)

  purrr::map(
    variables,
    ~ df %>%
      dplyr::mutate(studyid = gsub(".*(\\d{3}).*", "\\1", studyid)) %>%
      dplyr::distinct(dplyr::across(dplyr::all_of(c("studyid", .x)))) %>%
      dplyr::filter(!is.na(dplyr::across(dplyr::all_of(.x)))) %>%
      readr::write_csv(file.path("temp", paste0(.x, ".csv", collapse = "")))
  )
}

load_packages <- function() {
  invisible(source("./packages.R"))
}



#' figure_theme
#'
#' Attractive ggplot2 theme defaults

figure_theme <- function(){

    ggplot2::theme_bw() +
      ggplot2::theme(
        text = ggplot2::element_text(family = "serif"),
        strip.text.y = ggplot2::element_text(angle = 0),
        strip.background.y = ggplot2::element_blank()
      )

}

#' convert dates to seasons
#' @param date a date
#' @param lat latitude
#' @return a season
#' 

get_season <- function(date, lat){
  if (lat > 0) {
    if (month(date) %in% c(3:5)) {
      "spring"
    } else if (month(date) %in% c(6:8)) {
      "summer"
    } else if (month(date) %in% c(9:11)) {
      "autumn"
    } else {
      "winter"
    }
  } else {
    if (month(date) %in% c(3:5)) {
      "autumn"
    } else if (month(date) %in% c(6:8)) {
      "winter"
    } else if (month(date) %in% c(9:11)) {
      "spring"
    } else {
      "summer"
    }
  }
}

