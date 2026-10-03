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

  sleep_table <- list()

  sleep_table$data <- lapply(sleep_vars, function(sleep) {
    model1_name <- paste(
      sleep,
      "by",
      pa_vars[[1]],
      collapse = " "
    )
    model2_name <- paste(
      sleep,
      "by",
      pa_vars[[2]],
      collapse = " "
    )

    tab <- cbind(
      format_table(model_list[[model1_name]]$table, adjusted_for, moderator),
      format_table(model_list[[model2_name]]$table, adjusted_for, moderator)
    )
    tab[, c(1:5, 7:10)]
  })

  sleep_table$caption <-
    glue::glue(
      "Physical activity predicting sleep controlling for {control_vars}."
    )
  sleep_table$note <- paste0(
    note,
    ". Outcomes variables are listed in the column headers."
  )

  sleep_conv_issue <- any(sapply(sleep_table$data, function(x) {
    any(grepl("\\\\dagger", x$"$\\beta$ [95\\% CI]"))
  }))

  if (sleep_conv_issue) {
    sleep_table$note <- paste0(
      sleep_table$note,
      ". $^\\dagger$ value came from a pooled model where fewer than ",
      papaja::print_num(convergence_threshold * 100),
      "\\% of models converged."
    )
  }

  sleep_table$col_spanners <- list(
    c(2, 5),
    c(6, 9)
  )
  names(sleep_table$col_spanners) <- names(pa_vars)

  pa_table <- list()
  pa_table$data <- lapply(sleep_vars, function(sleep) {
    model1_name <- paste(
      pa_vars[[1]],
      "by",
      paste0(sleep, "_lag"),
      collapse = " "
    )
    model2_name <- paste(
      pa_vars[[2]],
      "by",
      paste0(sleep, "_lag"),
      collapse = " "
    )

    tab <- cbind(
      format_table(model_list[[model1_name]]$table, adjusted_for, moderator),
      format_table(model_list[[model2_name]]$table, adjusted_for, moderator)
    )
    tab[, c(1:5, 7:10)]
  })

  pa_table$caption <-
    glue::glue(
      "Sleep predicting physical activity controlling for {control_vars}"
    )
  pa_table$note <- paste0(
    note,
    ". Outcomes variables are listed in the row headers."
  )

  pa_conv_issue <- any(sapply(pa_table$data, function(x) {
    any(grepl("\\\\dagger", x$"$\\beta$ [95\\% CI]"))
  }))

  if (pa_conv_issue) {
    pa_table$note <- paste0(
      pa_table$note,
      ". $^\\dagger$ value came from a pooled model where fewer than ",
      papaja::print_num(convergence_threshold * 100),
      "\\% of models converged."
    )
  }

  pa_table$col_spanners <- list(
    c(2, 5),
    c(6, 9)
  )
  names(pa_table$col_spanners) <- names(pa_vars)

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
#' without the quadratic moderation term. See D8 in CODE_REVIEW.md.

format_table <- function(
  tab,
  control_vars,
  moderator = NULL,
  conv_daggers = FALSE
) {
  term <- as.character(tab$term)

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
    gsub("\\^2\\)", "$^2$", x = _) |>
    gsub("accelerometer wear location", "", x = _)
  # Capitalise the first letter of each side of an interaction, and nothing
  # else. str_to_sentence() lower-cased everything after the first character,
  # which mangled factor levels into "Regionnorth america".
  tab$term <- gsub("(^|:)([a-z])", "\\1\\U\\2", tab$term, perl = TRUE)
  tab$term <- gsub(
    "(S|s)cale pa (intensity|volume)",
    "Physical activity",
    tab$term
  )
  tab$term <- gsub(
    "(S|s)cale sleep",
    "Sleep",
    tab$term
  )
  tab$term <- gsub("\\slag", "", tab$term)
  tab$term <- gsub(":", " $\\\\times$ ", tab$term)
  names(tab) <- c("Term", "$\\beta$ [95\\% CI]", "SE", "t", "p")
  tab
}
