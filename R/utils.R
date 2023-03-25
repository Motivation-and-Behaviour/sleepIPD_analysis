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

#' APA_style for gt

apa_style <- function(x) {
  x |>
    opt_table_lines(extent = "none")|>
    tab_options(
      heading.border.bottom.width = 2,
      heading.border.bottom.color = "black",
      heading.border.bottom.style = "solid",
      heading.title.font.size = 12,
      table.font.size = 12,
      heading.subtitle.font.size = 12,
      table_body.border.bottom.color = "black",
      table_body.border.bottom.width = 1,
      table_body.border.bottom.style = "solid",
      column_labels.border.bottom.color = "black",
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = 1
    ) |>
    opt_table_font(font = "times")
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

find_max  <- function(x) {names(which.max(table(x)))}

