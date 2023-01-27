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
    data <- data %>%
      filter(n_valid_hours > 0) %>%
      select(-n_valid_hours, -n_hours, -weekday_x, -day_zero)

  # Empty imputation to change defaults:
  m0 <- mice(data, maxit = 0)

  # Don't do imputation based on these vars:
  dont_imp <- c("age_cat", "filename", "calendar_date")
  meth <- m0$method
  pred <- m0$predictorMatrix
  meth[names(meth) %in% dont_imp] <- ""
  pred[, colnames(pred) %in% dont_imp] <- 0

  # Run imps with better settings
  imps <-
    futuremice(
      data,
      m = n_imps,
      predictorMatrix = pred,
      method = meth,
      n.core = parallel::detectCores() - 1
    )

  # include_scale_variables
  sleep_vars <- c("sleep_duration",
                  "sleep_efficiency",
                  "sleep_onset",
                  "sleep_regularity")


  variables_to_scale <-
    c(sleep_vars,
      "pa_volume",
      "pa_intensity",
      paste0(sleep_vars, "_lag")
    )

  scale_names <- paste0("scale_", variables_to_scale)

  imp_list <- data.table(complete(imps, action = "long", include = TRUE))

  for(v in seq_along(variables_to_scale)) {
    imp_list[, (eval(scale_names[v])) := as.numeric(scale(eval(parse(text = variables_to_scale[v])))), by = ".imp"]

  }

  as.mids(imp_list)

}
