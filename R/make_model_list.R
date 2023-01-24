#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_imp
#' @return
#' @author conig
#' @export
make_model_list <- function(data_imp) {

  input <- expand.grid(x_var = c("pa_volume", "pa_intensity"),
                       y_var = c("sleep_duration", "sleep_efficiency","sleep_onset","sleep_regularity"),
                       stringsAsFactors = FALSE)

  target_vars <- c(input$x_var, input$y_var)

  additional_input <- expand.grid(
    x_var = c("age", "weight"),
    y_var = target_vars)

}
