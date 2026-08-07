#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_imp
#' @return
#' @author Taren Sanders
#' @export
check_imps <- function(data_imp) {
  require(dplyr)
  require(ggplot2)

  imps <- as_tibble(mice::complete(data_imp, action = "long", include = TRUE))

  # mice drops variables it cannot handle (character columns, constants,
  # collinear predictors) without erroring, so record what it dropped.
  logged <- data_imp$loggedEvents
  logged_file <- "Figures/explore/imps_logged_events.csv"
  if (is.null(logged)) {
    logged <- data.frame(
      it = integer(),
      im = integer(),
      dep = character(),
      meth = character(),
      out = character()
    )
  }
  write.csv(logged, logged_file, row.names = FALSE)
  if (nrow(logged) > 0) {
    # `out` is NA for events that dropped nothing (e.g. constant predictors).
    out_lists <- strsplit(stats::na.omit(logged$out), ", ")
    dropped <- sort(unique(unlist(out_lists)))
    warning(
      "mice logged ",
      nrow(logged),
      " event(s) dropping ",
      length(dropped),
      " predictor(s); see ",
      logged_file,
      ": ",
      paste(utils::head(dropped, 20), collapse = ", "),
      if (length(dropped) > 20) ", ..." else "",
      call. = FALSE
    )
  }

  # Invariant variables should be the same within participants in each imp.
  participant_invar <- c("age", "weight", "height", "bmi", "bmi_z")
  stopifnot(
    "An invariant variable varies within a particpant" = all(sapply(
      participant_invar,
      function(x) {
        n_distinct(select(imps, .imp, participant_id)) ==
          n_distinct(select(imps, .imp, participant_id, {{ x }}))
      }
    ))
  )

  # Some variables should always be positive
  positive_vars <- c(
    "pa_volume",
    "sleep_duration",
    "sleep_efficiency",
    "sleep_onset",
    "sleep_wakeup",
    "sleep_efficiency_lag",
    "sleep_onset_lag",
    "sleep_wakeup_lag",
    "sleep_duration_lag",
    "age",
    "weight",
    "height",
    "bmi",
    "daylight_hours",
    "pa_mostactivehr"
  )

  stopifnot(
    "A postive variable contains negative numbers" = all(sapply(
      positive_vars,
      function(x) {
        nrow(filter(imps, .imp > 0 & !!sym(x) < 0)) == 0
      }
    ))
  )

  # Make some density plots.
  density_vars <- c(
    "pa_volume",
    "pa_intensity",
    "pa_intensity_m16",
    "sleep_duration",
    "sleep_efficiency",
    "sleep_onset",
    "sleep_wakeup",
    "sleep_regularity",
    "sleep_efficiency_lag",
    "sleep_onset_lag",
    "sleep_wakeup_lag",
    "sleep_regularity_lag",
    "sleep_duration_lag",
    "age",
    "weight",
    "height",
    "bmi",
    "bmi_z",
    "daylight_hours",
    "pa_mostactivehr"
  )

  na_observed <- colSums(is.na(imps[imps$.imp == 0, density_vars]))
  na_completed <-
    colSums(is.na(imps[imps$.imp > 0, density_vars])) / data_imp$m
  not_imputed <- density_vars[
    na_observed > 0 & abs(na_completed - na_observed) < 0.5
  ]

  imps_long <- imps %>%
    select(".imp", ".id", all_of(density_vars)) %>%
    tidyr::pivot_longer(-c(".imp", ".id"), names_to = "variable") %>%
    mutate(
      imputed = if_else(.imp == 0, "Observed", "Imputed"),
      value = as.numeric(value),
      variable = factor(variable, levels = density_vars)
    )

  plot <- ggplot(imps_long, aes(x = value, group = .imp, colour = imputed)) +
    stat_density(
      geom = "path",
      position = "identity",
      alpha = 0.4,
      linewidth = 0.5
    ) +
    facet_wrap(~variable, ncol = 4, scales = "free") +
    labs(
      caption = if (length(not_imputed) > 0) {
        paste0(
          "Carried through un-imputed (both curves are the same data): ",
          paste(not_imputed, collapse = ", ")
        )
      } else {
        NULL
      }
    )

  filename <- "Figures/explore/imps_density.png"
  ggsave(filename, plot, width = 12, height = 12, dpi = 300)

  c(filename, logged_file)
}
