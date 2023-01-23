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

img_explore <- function(){

  data.frame(x = "sleepefficiency", "")



}
