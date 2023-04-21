#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param ... model_lists separated by commas
#' @author conig
#' @export
make_model_diagnostics <- function(...) {
  lst <- list(...)
  names_lst <- as.character(substitute(list(...)))[-1]

  lst <- lapply(lst, check_list)

  names(lst) <- names_lst |>
    gsub("model_list_by", "models moderated by", x = _ ) |>
    stringr::str_to_sentence() |>
    gsub("scale_", "", x = _ ) |>
    gsub("_lag", "(lagged)", x = _ ) |>
    gsub("pa|Pa", "PA", x = _ ) |>
    gsub("_", " ", x = _ )

  lst
}

check_list <- function(model_list){

  info <- lapply(model_list, function(x){
    x$model_assets$diagnostics
  }) |> data.table::rbindlist()

  info$model_name <- names(model_list)|>
    stringr::str_to_sentence() |>
    gsub("scale_", "", x = _ ) |>
    gsub("_lag", "(lagged)", x = _ ) |>
    gsub("pa|Pa", "PA", x = _ ) |>
    gsub("_", " ", x = _ )

  info[, .("Model name" = model_name, Skewness, Kurtosis, `Converged (\\%)`)]


}

#' check_model
#'
#' Check the assumptions of a single model
#' @param model list of models
#' @param conv convergence proportion
#' @test model <- model_list_by_age

check_model <- function(model, conv){
    p_conv <- conv *100
    pc_conv <- papaja::print_num(p_conv) |>
      paste0("%")

    cbind(check_resids(model),
          "Converged (\\%)" = pc_conv) |> data.table::data.table()
}

#' check_resids
#'
#' Get skewness and kurtosis for models
#' @param model list of models

check_resids <- function(model){
  require(dplyr)
  dt <- model |> lapply(function(x){
    resid <- residuals(x)
    tibble(Skewness = moments::skewness(resid),
           Kurtosis = moments::kurtosis(resid) - 3)

  }) |> data.table::rbindlist()
  dt[, lapply(.SD, function(x) mean(x, na.rm = TRUE))]
}
