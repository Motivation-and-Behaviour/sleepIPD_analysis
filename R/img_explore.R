# Temp data:
# clean_data <- data.table::fread("data/100_ISCOLE.csv")

#' make_lineplot
#'
#' @param clean_data cleaned data (not imputed)
#' @param x x-variable
#' @param y y-varaible

make_lineplot <- function(clean_data, x, y, title = NULL, path = NULL){

  if(is.null(path)) stop("argument path must be supplied")

  require(ggplot2)

  p <- clean_data |>
    ggplot(ggplot2::aes(x = .data[[x]], y = .data[[y]])) +
    geom_point(alpha = .2) +
    geom_smooth() +
    geom_density2d(alpha = .5, colour = "grey") +
    labs(title = title) +
    figure_theme()

  filename <-  glue::glue("Figures/{path}")

  ggsave(plot = p, filename = filename,
         height = 10, width = 12, units = "cm", dpi = 300)

  return(filename)

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

  input$lab <- glue::glue("{input$y_var} by {input$x_var}")

  img_list <- lapply(seq_len(nrow(input)), function(i){

    make_lineplot(
      data_clean,
      x = input$x_var[i],
      y = input$y_var[i],
      title = input$lab[i],
      path = paste0("explore/",input$lab[i], ".jpg")
    )
  })

names(img_list) <- input$lab
  img_list
}
