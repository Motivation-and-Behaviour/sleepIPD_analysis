#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param model_list
#' @return list of tables
#' @author Taren Sanders
#' @test model_list <- model_list_by_wear_location
#' @export
# Display names for variables that would otherwise print as their raw column
# name. Shared by the "Adjusted for ..." note and by format_table(), so a
# variable cannot be named one way in the note and another in the table.
term_display_names <- c(
  "ses" = "SES",
  "bmi" = "BMI",
  "bmi_z" = "BMI z-score",
  "daylight_hours" = "daylight hours",
  "pa_mostactivehr" = "most active hour",
  "acc_wear_loc" = "wear location",
  "studyid" = "the fixed effects of study IDs"
)

make_model_tables <- function(model_list) {
  adjusted_for <- model_list[[1]]$control_vars
  moderator <- attr(model_list[[1]], "moderator")

  control_vars <- adjusted_for |>
    dplyr::recode(!!!term_display_names)

  control_vars <- paste(control_vars, collapse = ", ") |>
    # replace last comma with and
    gsub(",([^,]+)$", ", and\\1", x = _, perl = TRUE)

  note <- paste("Adjusted for", control_vars)

  vars <- attr(model_list, "vars")

  sleep_vars <- vars$sleep_vars
  pa_vars <- vars$pa_vars

  # Columns name their variable rather than sitting under spanners
  pa_short <- gsub("^Physical activity", "PA", names(pa_vars))
  table_columns <- c(
    "Term",
    as.vector(rbind(paste(pa_short, "β [95% CI]"), paste(pa_short, "p")))
  )
  pair_tables <- function(model_names) {
    tabs <- lapply(model_names, function(nm) {
      format_table(model_list[[nm]]$table, adjusted_for, moderator)
    })
    tab <- do.call(cbind, c(list(tabs[[1]][, 1]), lapply(tabs, function(t) t[, c(2, 5)])))
    names(tab) <- table_columns
    tab
  }

  note_terms <- paste0(
    "Variables marked (z) are standardised",
    if (any(grepl("^log_", pa_vars))) " and (ln) natural-log transformed",
    "; ² denotes a quadratic term and × an interaction."
  )
  if (!identical(attr(model_list, "spec"), "blended")) {
    note_terms <- paste0(
      note_terms,
      " (within) is the day's deviation from the participant's mean, ",
      "(between) is that mean, and (within, prior) is the previous day's ",
      "deviation (for sleep regularity, the index for the two preceding days)."
    )
  }
  if (moderator == "age") {
    age_centre <- attr(model_list, "age_centre")
    note_terms <- paste0(
      note_terms,
      " Age was centred at ",
      age_centre,
      " years (the mean across person-days) and scaled per 10 years, so ",
      "main effects are associations at age ",
      age_centre,
      " and age interactions are the change per 10 years."
    )
  }
  conv_note <- paste0(
    " † value came from a pooled model where fewer than ",
    papaja::print_num(convergence_threshold * 100),
    "% of models converged."
  )
  has_dagger <- function(tabs) {
    any(vapply(
      tabs,
      function(x) any(grepl("†", unlist(x), fixed = TRUE)),
      TRUE
    ))
  }

  sleep_table <- list()

  sleep_table$data <- lapply(sleep_vars, function(sleep) {
    pair_tables(paste(sleep, "by", pa_vars))
  })

  sleep_table$caption <-
    glue::glue(
      "Physical activity predicting sleep controlling for {control_vars}."
    )
  sleep_table$note <- paste0(
    note,
    ". Rows are grouped by sleep outcome; columns give the physical activity ",
    "predictor. ",
    note_terms
  )
  if (has_dagger(sleep_table$data)) {
    sleep_table$note <- paste0(sleep_table$note, conv_note)
  }

  pa_table <- list()
  pa_table$data <- lapply(sleep_vars, function(sleep) {
    pair_tables(paste(pa_vars, "by", paste0(sleep, "_lag")))
  })

  pa_table$caption <-
    glue::glue(
      "Sleep predicting physical activity controlling for {control_vars}"
    )
  pa_table$note <- paste0(
    note,
    ". Rows are grouped by the previous night's sleep predictor; columns give ",
    "the physical activity outcome. ",
    note_terms
  )
  if (has_dagger(pa_table$data)) {
    pa_table$note <- paste0(pa_table$note, conv_note)
  }

  return(list(sleep = sleep_table, physical_activity = pa_table))
}

#' format_table
#'
#' A function for taking raw table output and preparing it for publication
#' @param tab data.frame object
#' @param control_vars the covariates the model adjusted for. Their main-effect
#'   rows are hidden, because the table note names them instead.
#' @param moderator the moderator's variable name, used to strip that name off
#'   its own factor levels.
#' @param conv_daggers a bool. If true, daggers will be converted to double daggers.
#'
#' @details Rows are selected on the **raw** term names, before any cosmetic
#' substitution, and only ever on a main effect. The previous version matched the
#' display strings with `^ses`/`^sex`/`^bmi`, which in the three families
#' moderated by those variables also caught the moderator's own rows *and* its
#' quadratic interaction — R names that term `moderator:I(x^2)`, so it starts
#' with the moderator's name. `by_bmi`, `by_ses` and `by_sex` therefore shipped
#' without the quadratic moderation term.

format_table <- function(
  tab,
  control_vars,
  moderator = NULL,
  conv_daggers = FALSE
) {
  # Centred age (see age_term()) back to plain "age"
  term <- gsub(
    "I\\(\\(age - [0-9.]+\\)/10\\)",
    "age",
    as.character(tab$term)
  )

  # Main effects of the adjustment variables only: never an interaction, and
  # never the moderator, which make_model_list() removes from control_vars.
  # Prefix-matched so factor expansions (sesMedium, sexMale) go too.
  if (length(control_vars) > 0) {
    is_covariate <- !grepl(":", term, fixed = TRUE) &
      Reduce(`|`, lapply(control_vars, function(v) startsWith(term, v)))
    tab <- tab[!is_covariate, , drop = FALSE]
    term <- term[!is_covariate]
  }

  if (!is.null(moderator)) {
    # R pastes a factor level onto its variable name ("regionNorth America",
    # "seasonspring"). Drop the name so the level reads as itself. The lookahead
    # requires something to follow, so a numeric moderator's bare main-effect
    # term ("age", "bmi_z") is left alone.
    term <- gsub(paste0(moderator, "(?=[A-Za-z0-9])"), "", term, perl = TRUE)
    # Numeric moderators have no level suffix, so name them properly instead.
    if (moderator %in% names(term_display_names)) {
      term <- gsub(
        paste0("\\b", moderator, "\\b"),
        term_display_names[[moderator]],
        term
      )
    }
  }

  tab$term <- gsub("I\\(", "", term) |>
    gsub("_", " ", x = _) |>
    gsub("\\^2\\)", "²", x = _) |>
    gsub("accelerometer wear location", "", x = _)
  # Capitalise the first letter of each side of an interaction, and nothing
  # else. str_to_sentence() lower-cased everything after the first character,
  # which mangled factor levels into "Regionnorth america".
  tab$term <- gsub("(^|:)([a-z])", "\\1\\U\\2", tab$term, perl = TRUE)
  tab$term <- gsub(
    "(S|s)cale pa (intensity|volume|mvpa)",
    "Physical activity",
    tab$term
  )
  tab$term <- gsub(
    "(S|s)cale sleep",
    "Sleep",
    tab$term
  )
  tab$term <- gsub(" wp lag\\b", " (within, prior)", tab$term)
  tab$term <- gsub("\\slag", "", tab$term)
  tab$term <- gsub(" wp\\b", " (within)", tab$term) |>
    gsub(" pm\\b", " (between)", x = _)
  tab$term <- gsub(":", " × ", tab$term)
  # model_builder() marks low-convergence estimates with a LaTeX dagger
  tab[[2]] <- gsub("$^\\dagger$", "†", tab[[2]], fixed = TRUE)
  names(tab) <- c("Term", "β [95% CI]", "SE", "t", "p")
  tab
}
