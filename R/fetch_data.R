#' Fetch datasets from cloudstor
#'
#' @title fetch_data
#' @param cloudstor_user Your cloudstor username
#' @param cloudstor_pwd Your cloudstor password
#' @param base_folder The base folder where data is stored. This should end with
#' "`Pooled Sleep Study`"
#' @param files The list of files to fetch. Useful if you only want to only
#' redownload a few files
#' @return invisible list of downloaded files
#' @author Taren Sanders
#' @export
fetch_data <- function(cloudstor_user = Sys.getenv("CLOUD_USER"),
                       cloudstor_pwd = Sys.getenv("CLOUD_PASS"),
                       base_folder = Sys.getenv("CLOUDSTOR_MB", unset = "Motivation and Behaviour Program/Pooled Sleep Study"), # nolint
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
                         "112 Jesus/Study 2/112_Jesus.csv",
                         "113 Manuel/113_Manuel.csv",
                         "114 Zenong/114_Zenong.csv",
                         "115 Angelica/115_Angelica.csv",
                         "117 Ivan/117_Ivan.csv",
                         "118 Lubans/118_Lubans.csv",
                         "221 Whitehall/221_Whitehall.csv"
                       )) {
  if (any(cloudstor_user == "" | cloudstor_pwd == "")) {
    stop(paste(
      "Could not find cloudstor credentials.",
      "Please set CLOUD_USER and CLOUD_PASS in your environment."
    ))
  }

  dir.create("data", showWarnings = FALSE)

  downloaded_files <- purrr::map(files, ~ cloudstoR::cloud_get(
    paste(base_folder, .x, sep = "/"),
    dest = file.path("data", basename(.x)),
    user = cloudstor_user, password = cloudstor_pwd,
    open_file = FALSE
  ))

  invisible(downloaded_files)
}
