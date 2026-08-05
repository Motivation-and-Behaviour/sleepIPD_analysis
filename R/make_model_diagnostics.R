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
    gsub("model_list_by", "models moderated by", x = _) |>
    stringr::str_to_sentence() |>
    gsub("scale_", "", x = _) |>
    gsub("_lag", "(lagged)", x = _) |>
    gsub("pa|Pa", "PA", x = _) |>
    gsub("_", " ", x = _)

  lst
}

check_list <- function(model_list) {
  info <- lapply(model_list, function(x) {
    x$model_assets$diagnostics
  }) |>
    data.table::rbindlist()

  info$model_name <- names(model_list) |>
    stringr::str_to_sentence() |>
    gsub("scale_", "", x = _) |>
    gsub("_lag", "(lagged)", x = _) |>
    gsub("pa|Pa", "PA", x = _) |>
    gsub("_", " ", x = _)

  info[, .(
    "Model name" = model_name,
    Skewness,
    Kurtosis,
    `Converged (\\%)`,
    `Singular (\\%)`
  )]
}

#' check_model
#'
#' Summarise the assumption checks for one model across imputations
#' @param resids list of per-imputation residual moments, from `resid_moments()`
#' @param conv proportion of imputations where the optimizer converged
#' @param singular proportion of imputations that produced a singular fit
#' @details Singularity is reported alongside, not folded into, convergence. A
#' singular fit is a variance component estimated at the boundary, not an
#' optimizer failure — see `is_converged()` in `R/model_builder.R`.

check_model <- function(resids, conv, singular) {
  as_pc <- function(x) paste0(papaja::print_num(x * 100), "%")

  dt <- data.table::rbindlist(resids)

  cbind(
    dt[, lapply(.SD, function(x) mean(x, na.rm = TRUE))],
    "Converged (\\%)" = as_pc(conv),
    "Singular (\\%)" = as_pc(singular)
  ) |>
    data.table::data.table()
}

#' resid_moments
#'
#' Skewness and excess kurtosis of one model's residuals
#' @param model a single fitted model

resid_moments <- function(model) {
  resid <- residuals(model)
  data.table::data.table(
    Skewness = moments::skewness(resid),
    Kurtosis = moments::kurtosis(resid) - 3
  )
}
