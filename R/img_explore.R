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
#'
#' @param data_clean non-imputated data

make_explore_img_list <- function(data_clean){

  input <- expand.grid(x_var = c("pa_volume", "pa_intensity"),
              y_var = c("sleep_duration", "sleep_efficiency","sleep_onset","sleep_regularity"),
              stringsAsFactors = FALSE)

  target_vars <- c(input$x_var, input$y_var)

  additional_input <- expand.grid(
    x_var = c("age", "weight"),
    y_var = target_vars)

  input <- rbind(input, additional_input)

  input$lab <- glue::glue("{input$x_var} by {input$y_var}")

  img_list <- lapply(seq_len(nrow(input)), function(i){

    make_lineplot(
      data_clean,
      x = input$x_var[i],
      y = input$y_var[i]
    )
  })

names(img_list) <- input$lab
  img_list
}
