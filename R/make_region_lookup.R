make_region_lookup <- function() {
  lookup_table <-
    data.frame(
      country = c(
        "Australia",
        "United Kingdom",
        "Portugal",
        "Colombia",
        "Kenya",
        "Brazil",
        "South Africa",
        "India",
        "United States",
        "China",
        "Canada",
        "Finland",
        "Spain",
        "Ecuador",
        "Morocco",
        "Romania",
        "Ukraine",
        "Switzerland",
        "Denmark",
        "New Zealand",
        "Czech Republic"
      ),
      region = factor(c(
        "Oceania",
        "Europe",
        "Europe",
        "South America",
        "Africa",
        "South America",
        "Africa",
        "Asia",
        "North America",
        "Asia",
        "North America",
        "Europe",
        "Europe",
        "South America",
        "Africa",
        "Europe",
        "Europe",
        "Europe",
        "Europe",
        "Oceania",
        "Europe"
      ), levels = c("Oceania", "Europe", "Africa", "Asia", "North America", "South America")
    ))
  lookup_table
}


