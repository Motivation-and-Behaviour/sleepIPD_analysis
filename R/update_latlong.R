#' update_latlong
#'
#' Ensure data/latlong.csv has coordinates for every study location, geocoding
#' any that are missing.
#'
#' @param locations character vector of "City, Country" strings
#' @author noetel
#' @export
update_latlong <- function(locations) {
  existing <- NULL
  if (file.exists("data/latlong.csv")) {
    existing <- read.csv("data/latlong.csv", stringsAsFactors = FALSE)
    existing <- existing[, c("lon", "lat", "location"), drop = FALSE]
  }

  missing_locations <- setdiff(locations, existing$location)
  if (length(missing_locations) == 0) {
    return(invisible(NULL))
  }

  require(ggmap)
  if (!ggmap::has_google_key()) {
    stop(
      "To run the pipeline an API key must be\n",
      "created and stored with ggmap::register_google().\n",
      "Missing coordinates for: ",
      paste(missing_locations, collapse = "; ")
    )
  }

  raw <- sapply(missing_locations, geocode)
  new <- as.data.frame(t(raw))
  new$location <- rownames(new)
  new <- as.data.frame(
    lapply(new, as.character),
    stringsAsFactors = FALSE
  )[, c("lon", "lat", "location")]

  latlong <- rbind(existing, new)
  rownames(latlong) <- NULL
  # row.names = TRUE on purpose: clean_data() reads this file and drops the
  # resulting `X` column.
  write.csv(latlong, "data/latlong.csv")

  invisible(NULL)
}
