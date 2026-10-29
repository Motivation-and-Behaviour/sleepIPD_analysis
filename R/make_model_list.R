#' make_model_list
#'
#' Fit one model family in series. 
#'
#' @title
#' @param data_imp
#' @return
#' @author conig
#' @export
make_model_list <- function(
  data_imp,
  moderator = "age",
  moderator_term = "11, 18, 35, 65",
  control_vars = c("ses", "age", "sex", "bmi_z"),
  ranef = "(1|studyid) + (1|participant_id)",
  sleep_vars = c(
    "scale_sleep_duration",
    "scale_sleep_efficiency",
    "scale_sleep_onset",
    "scale_sleep_regularity"
  ),
  pa_vars = c(
    "log_pa_volume",
    "scale_pa_intensity"
  ),
  min_wear_days = min_wear_days_primary,
  age_centre,
  spec = "wb"
) {
  specs <- make_model_specs(moderator, sleep_vars, pa_vars, age_centre, spec)

  models <- lapply(seq_len(nrow(specs)), function(i) {
    fit_model_spec(
      data_imp,
      specs[i, ],
      moderator = moderator,
      moderator_term = moderator_term,
      control_vars = control_vars,
      ranef = ranef,
      min_wear_days = min_wear_days,
      age_centre = age_centre
    )
  })

  assemble_model_list(
    models,
    specs,
    data_imp,
    moderator = moderator,
    control_vars = control_vars,
    sleep_vars = sleep_vars,
    pa_vars = pa_vars,
    min_wear_days = min_wear_days,
    age_centre = age_centre,
    spec = spec
  )
}

#' make_model_specs
#'
#' The 16 models in a family: each sleep outcome on each PA predictor (RQ1),
#' and each PA outcome on each lagged sleep predictor (RQ3).
#' @return data frame with `outcome`, `predictors`, `RQ` and `model_name`
make_model_specs <- function(
  moderator,
  sleep_vars,
  pa_vars,
  age_centre,
  spec = "wb"
) {
  spec <- match.arg(spec, c("wb", "blended", "clpm"))
  sleep_lag_vars <- paste0(sleep_vars, "_lag")

  mod_formula <- formula_term(moderator, age_centre)
  quadratic_pattern <- "{x} * {mod_formula} + I({x}^2) * {mod_formula}"
  make_quadratic <- function(x) as.character(glue::glue(quadratic_pattern))

  rq1_x <- make_quadratic(pa_vars)
  rq3_x <- make_quadratic(sleep_lag_vars)
  # Within-between: the day's deviation from the person mean, then the mean
  if (spec != "blended") {
    rq1_x <- paste(
      make_quadratic(paste0(pa_vars, "_wp")),
      "+",
      make_quadratic(paste0(pa_vars, "_pm"))
    )
    rq3_x <- paste(
      make_quadratic(paste0(sleep_vars, "_wp_lag")),
      "+",
      make_quadratic(paste0(sleep_vars, "_pm"))
    )
  }

  instructions_rq1 <-
    expand.grid(
      outcome = sleep_vars,
      predictors = rq1_x,
      RQ = 1,
      stringsAsFactors = FALSE
    )

  instructions_rq3 <-
    expand.grid(
      outcome = pa_vars,
      predictors = rq3_x,
      RQ = 3,
      stringsAsFactors = FALSE
    )

  instructions <- rbind(instructions_rq1, instructions_rq3)

  # Cross-lagged: also adjust for the outcome's own previous value
  if (spec == "clpm") {
    instructions$predictors <- with(
      instructions,
      paste(predictors, "+", paste0(outcome, "_wp_lag"))
    )
  }

  instructions$model_name <- with(
    instructions,
    as.character(glue::glue(
      "{outcome} by {sub('_wp', '', gsub(' .*', '', predictors))}"
    ))
  )

  instructions
}

#' fit_model_spec
#'
#' Fit one row of `make_model_specs()` across all imputations.
#' @param spec a one-row data frame from `make_model_specs()`
fit_model_spec <- function(
  data_imp,
  spec,
  moderator,
  moderator_term,
  control_vars,
  ranef,
  min_wear_days,
  age_centre
) {
  stopifnot(nrow(spec) == 1)

  model <- model_builder(
    data_imp,
    outcome = spec$outcome,
    predictors = spec$predictors,
    moderator = moderator,
    control_vars = family_controls(control_vars, moderator),
    table_only = FALSE,
    ranef = ranef,
    terms = c(
      predictor_grid(
        data_imp,
        gsub(" .*", "", spec$predictors),
        rows = analysed_rows(
          data_imp,
          outcome = spec$outcome,
          predictors = spec$predictors,
          moderator = moderator,
          min_wear_days = min_wear_days
        )
      ),
      glue::glue("{moderator} [{moderator_term}]")
    ),
    RQ = spec$RQ,
    min_wear_days = min_wear_days,
    age_centre = age_centre
  )

  attr(model, "RQ") <- spec$RQ # nolint: object_name_linter.
  attr(model, "moderator") <- moderator

  model
}

#' assemble_model_list
#'
#' Name the fitted models and attach the attributes other targets read.
#' @param models list of `fit_model_spec()` results, in the order of `specs`
assemble_model_list <- function(
  models,
  specs,
  data_imp,
  moderator,
  control_vars,
  sleep_vars,
  pa_vars,
  min_wear_days,
  age_centre,
  spec = "wb"
) {

  stopifnot(length(models) == nrow(specs))
  for (i in seq_along(models)) {
    effects <- models[[i]]$model_assets$effects
    if (
      !identical(unique(effects$outcome), specs$outcome[i]) ||
        !identical(
          unique(gsub("\\[.*", "", effects$x_name)),
          gsub(" .*", "", specs$predictors[i])
        )
    ) {
      stop("Model ", i, " is not '", specs$model_name[i], "'.", call. = FALSE)
    }
  }

  control_vars <- family_controls(control_vars, moderator)
  sleep_lag_vars <- paste0(sleep_vars, "_lag")

  # Apply display names
  names(sleep_vars) <- display_names(sleep_vars)
  names(sleep_lag_vars) <- display_names(sleep_lag_vars)
  names(pa_vars) <- display_names(pa_vars)

  # Add some attributes used in other targets
  out <- unname(models)
  names(out) <- specs$model_name
  attr(out, "vars") <- list(sleep_vars = sleep_vars, pa_vars = pa_vars)
  attr(out, "scale_descriptives") <- get_scale_descriptives(
    data_imp,
    sleep_vars,
    sleep_lag_vars,
    pa_vars
  )
  attr(out, "age_centre") <- age_centre
  attr(out, "spec") <- spec
  attr(out, "filename_suffix") <-
    dplyr::case_when(
      spec == "blended" && "scale_pa_volume" %in% pa_vars ~ "_blended",
      spec == "clpm" ~ "_clpm",
      "scale_pa_mvpa" %in% pa_vars ~ "_mvpa",
      "studyid" %in% control_vars ~ "_fixedef",
      min_wear_days == 0 ~ "_allwear",
      min_wear_days != min_wear_days_primary ~ paste0("_", min_wear_days, "days"),
      "scale_pa_volume" %in% pa_vars ~ "_nolog",
      TRUE ~ ""
    )

  out
}

# The moderator is never also a control
family_controls <- function(control_vars, moderator) {
  control_vars[!control_vars == moderator]
}
