# Temp data:
# clean_data <- data.table::fread("data/100_ISCOLE.csv")

#' make_lineplot
#'
#' @param clean_data cleaned data (not imputed)
#' @param x x-variable
#' @param y y-varaible

make_lineplot <- function(clean_data, x, y, title = NULL){

  require(ggplot2)

  clean_data |>
    ggplot(ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    geom_point() +
    geom_smooth() +
    labs(title = title) +
    figure_theme()

}

#' create_img_list

make_explore_img_list <- function(data_clean){

  input <- expand.grid(exercise_vars = c("pa_volume", "pa_intensity"),
              sleep_vars = c("sleep_duration", "sleep_efficiency","sleep_onset","sleep_regularity"),
              stringsAsFactors = FALSE)

  input$lab <- glue::glue("{input$exercise_vars} by {input$sleep_vars}")

  img_list <- lapply(seq_len(nrow(input)), function(i){

    make_lineplot(
      data_clean,
      x = input$exercise_vars[i],
      y = input$sleep_vars[i]
    )
  })

names(img_list) <- input$lab
  img_list
}
