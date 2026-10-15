# If the sheet changes, run update_refactors() and commit the CSVs.
refactor_files <- c(
  "Sleep conditions" = "data/refactors_sleep_conditions.csv",
  "Ethnicity" = "data/refactors_ethnicity.csv",
  "SES" = "data/refactors_ses.csv"
)

#' read_refactors
#'
#' @param files paths from the `refactors_files` target
#' @param sheets named paths, see `refactor_files`
#' @return named list of tibbles, one per sheet, every column character
#'
#' @details `na = ""` because the sheets hold the literal string "NA" as a
#'   harmonised value which must not be read as missing.
read_refactors <- function(files, sheets = refactor_files) {
  # format = "file" targets drop names, so map the paths back to their sheets
  stopifnot(setequal(files, sheets))
  lapply(sheets, function(path) {
    x <- readr::read_csv(
      path,
      col_types = readr::cols(.default = readr::col_character()),
      na = "",
      trim_ws = FALSE,
      name_repair = "minimal",
      lazy = FALSE,
      progress = FALSE
    )
    # Rebuild the tibble so its serialised bytes match googlesheets4.
    out <- unname(as.list(x))
    attributes(out) <- list(
      class = c("tbl_df", "tbl", "data.frame"),
      row.names = .set_row_names(nrow(x)),
      names = c(names(x))
    )
    out
  })
}

#' update_refactors
#'
#' Re-download the recode tables from the Google Sheet into the CSVs that the
#' pipeline reads. Not called by the pipeline.
update_refactors <- function(files = refactor_files) {
  for (sheet in names(files)) {
    readr::write_csv(sheet_read(sheet), files[[sheet]], na = "")
  }
  invisible(files)
}

sheet_read <- function(sheet_name) {
  sheetid <- "1A75Qk8mNXygxcsCxLQ4maspZsQxZXJ5K-12X338CQ2s"
  googlesheets4::gs4_deauth()
  googlesheets4::read_sheet(
    sheetid,
    sheet = sheet_name,
    col_types = "c",
    range = "A:C"
  )
}
