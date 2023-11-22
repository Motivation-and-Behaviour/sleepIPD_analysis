#' Fetch datasets from Sharepoint
#'
#' @title fetch_data
#' @param base_folder The base folder where data is stored. This should end with
#' "`Pooled Sleep Study`"
#' @param files The list of files to fetch. Useful if you only want to only
#' redownload a few files
#' @return NULL
#' @author Taren Sanders
#' @export
fetch_data <- function(base_folder = "Motivation and Behaviour Program/Pooled Sleep Study", # nolint
                       files = c(
                         "100 ISCOLE/100_ISCOLE.csv",
                         "101 Pedro/101_Pedro.csv",
                         "102 Camilla/102_Camilla.csv",
                         "103 Skidmore/103_Skidmore.csv",
                         "104 Bruno/104_Bruno.csv",
                         "105 Ales/105_Ales.csv",
                         "106 Carol/106_Carol.csv",
                         "107 Veronica/107_Veronica.csv",
                         "108 iPLAY/108_iPLAY.csv",
                         "110 Sari/110_Sari.csv",
                         "111 Rowlands/111_Rowlands.csv",
                         "112 Jesus/Study 2/112_Jesus.csv",
                         "112 Jesus/Study 3/212_Jesus.csv",
                         "113 Manuel/113_Manuel.csv",
                         "114 Zenong/114_Zenong.csv",
                         "115 Angelica/115_Angelica.csv",
                         "117 Ivan/117_Ivan.csv",
                         "118 Lubans/118_Lubans.csv",
                         "221 Whitehall/221_Whitehall.csv",
                         "222 Wendt/222_Wendt.csv"
                       )) {
  require(Microsoft365R)

  team <- get_team("p-acu-Motivation_and_Behaviour_Program")
  drv <- team$get_drive("Cloudstor")

  dir.create("data", showWarnings = FALSE)

  downloaded_files <- purrr::map(files, ~ drv$download_file(
    paste(base_folder, .x, sep = "/"),
    dest = file.path("data", basename(.x)), overwrite = TRUE
  ))
}
