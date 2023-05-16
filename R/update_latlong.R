#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title

#' @return
#' @author noetel
#' @export
update_latlong <- function(locations) {
  if (!file.exists("data/latlong.csv") ||
    length(read.csv("data/latlong.csv")$location) < length(locations)) {
    require(ggmap)
    if(!ggmap::has_google_key()) stop("To run the pipeline an API key must be\ncreated and stored with ggmap::register_google()")

    rawlatlong <- sapply(locations, geocode)
    latlong <- t(rawlatlong)
    latlong <- as.data.frame(latlong)
    latlong$location <- rownames(latlong)
    latlong <- apply(latlong, 2, as.character)
    write.csv(latlong, "data/latlong.csv")
  }
}
