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
  names(lst) <- names_lst |>
    gsub("model_list_by", "models moderated by", x = _ ) |>
    gsub("_", " ", x = _ ) |>
    stringr::str_to_sentence()

  lapply(lst, check_model)

}

#' check_model
#'
#' Check the assumptions of a single model
#' @test model <- model_list_by_age

check_model <- function(model){

  out <- seq_len(length(model)) |> lapply(function(x){
    model_name <- names(model)[x]
    x <- model[[x]]
    moderator <- moderator <- attr(x, "moderator")
    p_conv <- attr(x$pooled_model, "conv")*100
    pc_conv <- papaja::print_num(p_conv) |>
      paste0("%")

    cbind("Model name" = model_name, check_resids(x),
          "Converged (\\%)" = pc_conv)

  }) |> data.table::rbindlist()

  out$"Model name" = gsub("scale_", "", out$"Model name") |>
    gsub("pa|Pa", "PA", x = _ ) |>
    gsub("_lag", "(lagged)", x = _ ) |>
    gsub("_", " ", x = _ )


  out

}

#' check_resids
#'
#' Get skewness and kurtosis for models

check_resids <- function(model){
  require(dplyr)
  dt <- model$model |> lapply(function(x){
    resid <- residuals(x)
    tibble(Skewness = moments::skewness(resid),
           Kurtosis = 3 - moments::kurtosis(resid))

  }) |> data.table::rbindlist()
  dt[, lapply(.SD, function(x) mean(x, na.rm = TRUE))]
}
