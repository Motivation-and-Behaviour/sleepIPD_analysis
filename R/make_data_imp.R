#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data data holdout or clean data
#' @param n_imps number of imputations
#' @return
#' @author conig
#' @export

make_data_imp <- function(data, n_imps = 3) {
  imps <- futuremice(data, m = n_imps, n.core = parallel::detectCores() - 1)

  imps
}
