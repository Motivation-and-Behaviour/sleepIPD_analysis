#' Make manuscript information
#'
#' Makes a list containing the information needed for the manuscript.
#' This avoids needing to load large objects when rendering the manuscript, and
#' makes it easier to include data in the abstract.
#'
#' @title make_manuscript_info
#' @param data_clean
#' @param participant_summary
#' @return List containing information for the manuscript
#' @author Taren Sanders
make_manuscript_info <- function(data_clean, participant_summary) {
  data_clean_eligible <- data_clean |>
    dplyr::filter(eligible)

  ms_info <- list()

  ## Results
  n_pts <- length(unique(data_clean$participant_id))
  n_pts_eligible <- length(unique(data_clean_eligible$participant_id))

  ms_info$n_obs <- nrow(data_clean) |> pretty()
  ms_info$n_pts <- n_pts |> pretty()
  ms_info$n_pts_eligible <- n_pts_eligible |> pretty()
  ms_info$n_obs_excluded <- (nrow(data_clean) - nrow(data_clean_eligible)) |>
    pretty()
  ms_info$n_pts_excluded <- (n_pts - n_pts_eligible) |> pretty()
  ms_info$n_missing_age <- dplyr::filter(data_clean_eligible, is.na(age)) |>
    dplyr::distinct(participant_id) |>
    nrow()

  ms_info$n_studies <- length(unique(data_clean$studyid))

  ms_info$p_female <-
    scales::label_percent(0.1)(
      mean(participant_summary$sex == "Female", na.rm = TRUE)
    )
  ms_info$p_young <-
    scales::label_percent(0.1)(mean(
      participant_summary$age_cat == "2-11 years",
      na.rm = TRUE
    ))
  ms_info$p_old <-
    scales::label_percent(0.1)(mean(
      participant_summary$age_cat == "66+ years",
      na.rm = TRUE
    ))

  weekday_table <- (data_clean_eligible$weekday) |> table()
  weekday <- chisq.test(weekday_table)
  ms_info$weekday_res <- glue::glue(
    "$\\chi^2_{(..weekday$parameter..)}$",
    " = ..papaja::print_num(weekday$statistic).., ",
    "p = ..papaja::print_p(weekday$p.value)..",
    .open = "..",
    .close = ".."
  )

  ms_info$weekday <- weekday

  ms_info$weekday$stdres <- format(round(weekday$stdres, 2), nsmall = 2)

  ms_info
}

pretty <- function(x) format(x, big.mark = ",")
