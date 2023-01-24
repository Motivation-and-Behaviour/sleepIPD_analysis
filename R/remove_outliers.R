#' remove_outliers
#'
#' @param data data
#' @param ignore_cols a character vector to not remove


remove_outliers <- function(data, ignore_cols = c()){

  if(any(!ignore_cols %in% names(data))) stop("At least one ignore_cols not found in data")

  classes <- sapply(data, class)
  numeric_cols <- names(data)[classes %in% c("numeric" , "integer")]
  numeric_cols <- numeric_cols[!numeric_cols %in% ignore_cols]

  for(var in numeric_cols){

    data[[var]] <- remove_outlier(data[[var]])

  }

  data

}

#' remove_outlier
#' @param x a vector to remove outliers from
#'

remove_outlier <- function(x){

  scaled_x <- scale(x)
  outlier_i <- which(scaled_x > 4 | scaled_x < - 4)

  x[outlier_i]  <- NA

  x
}

