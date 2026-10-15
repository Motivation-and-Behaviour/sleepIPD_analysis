#' make_study_table
#'
#' Table S1: one row per study in the pooled data, summarising the analytic
#' sample. Study names and citations come from `study_references_file`.
make_study_table <- function(
  data_clean,
  study_references_file,
  min_wear_days = min_wear_days_primary
) {
  require(data.table)

  refs <- data.table::fread(
    study_references_file,
    colClasses = "character",
    na.strings = ""
  )

  analytic <- data.table::as.data.table(data_clean)[
    eligible & n_valid_wear_days >= min_wear_days
  ]
  analytic[, studyid := as.character(studyid)]

  device_name <- function(x) {
    x <- trimws(x)
    x[tolower(x) == "geneactiv"] <- "GENEActiv"
    x
  }
  list_or_count <- function(x, max_listed = 3) {
    x <- sort(unique(stats::na.omit(x)))
    if (length(x) > max_listed) paste(length(x), "countries") else
      paste(x, collapse = ", ")
  }

  per_participant <- analytic[,
    .(
      age = age[1],
      sex = sex[1],
      n_days = n_valid_wear_days[1]
    ),
    by = .(studyid, participant_id)
  ]

  study_rows <- analytic[,
    .(
      Country = list_or_count(country),
      Device = paste(sort(unique(device_name(accelerometer_model))), collapse = ", "),
      `Wear location` = paste(sort(unique(acc_wear_loc)), collapse = ", ")
    ),
    by = studyid
  ]

  participant_rows <- per_participant[,
    .(
      N = format(.N, big.mark = ","),
      `Age, mean (range)` = sprintf(
        "%s (%s-%s)",
        papaja::print_num(mean(age, na.rm = TRUE), digits = 1),
        floor(min(age, na.rm = TRUE)),
        floor(max(age, na.rm = TRUE))
      ),
      `Female, %` = papaja::print_num(
        100 * mean(sex == "Female", na.rm = TRUE),
        digits = 1
      ),
      `Valid days, median` = papaja::print_num(
        stats::median(n_days, na.rm = TRUE),
        digits = 0
      )
    ),
    by = studyid
  ]

  all_studies <- data.table::data.table(
    studyid = sort(unique(as.character(data_clean$studyid)))
  )
  out <- Reduce(
    function(x, y) merge(x, y, by = "studyid", all.x = TRUE),
    list(all_studies, refs, study_rows, participant_rows)
  )
  out[is.na(N), N := "0"]
  for (j in names(out)) {
    data.table::set(
      out,
      which(is.na(out[[j]])),
      j,
      if (j %in% c("study", "reference")) "" else "-"
    )
  }
  data.table::setnames(
    out,
    c("studyid", "study", "reference"),
    c("Study ID", "Study", "Reference")
  )
  out[]
}

#' make_ses_table
#'
#' Table S2: each study's SES categories and their harmonised level.
make_ses_table <- function(refactors) {
  ses <- data.table::as.data.table(refactors$SES)
  data.table::setnames(
    ses,
    c("studyid", "SES", "Harmonized"),
    c("Study ID", "Original category", "Harmonised category")
  )
  ses[order(`Study ID`)]
}
