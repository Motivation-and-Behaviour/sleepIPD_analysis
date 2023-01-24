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

  # Empty imputation to change defaults:
  m0 <- mice(data, maxit = 0)

  # Don't do imputation based on these vars:
  dont_imp <- c("age_cat", "filename")
  meth <- m0$method
  pred <- m0$predictorMatrix
  meth[names(meth) %in% dont_imp] <- ""
  pred[, colnames(pred) %in% dont_imp] <- 0

  # Run imps with better settings
  imps <- futuremice(data, m = n_imps, predictorMatrix = pred, method = meth, n.core = parallel::detectCores() - 1)

  imps
}
