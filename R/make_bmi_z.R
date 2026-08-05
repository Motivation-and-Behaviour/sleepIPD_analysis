#' make_bmi_z
#'
#' BMI z-score, as specified in the protocol ("body mass index z-score
#' (z transformed)").
#'
#' @param bmi numeric, kg/m^2
#' @param age numeric, years
#' @param sex factor or character, levels "Female" / "Male"
#' @param participant_id vector identifying participants
#' @param adult_age age at which the adult branch takes over (default 18)
#' @return numeric vector of z-scores
#'
make_bmi_z <- function(bmi, age, sex, participant_id, adult_age = 18) {
  stopifnot(
    length(bmi) == length(age),
    length(age) == length(sex),
    length(sex) == length(participant_id)
  )

  z <- rep(NA_real_, length(bmi))

  is_child <- !is.na(age) & age < adult_age
  is_adult <- !is.na(age) & age >= adult_age

  # Children — external growth reference
  if (any(is_child)) {
    z[is_child] <- childsds::sds(
      value = bmi[is_child],
      age = age[is_child],
      sex = as.character(sex[is_child]),
      item = "bmi",
      ref = childsds::cdc.ref,
      type = "SDS",
      male = "Male",
      female = "Female"
    )
  }

  # Adults — standardised on the adult participants of this sample.
  if (any(is_adult)) {
    adult_rows <- is_adult & !duplicated(participant_id)
    mu <- mean(bmi[adult_rows], na.rm = TRUE)
    sigma <- stats::sd(bmi[adult_rows], na.rm = TRUE)
    z[is_adult] <- (bmi[is_adult] - mu) / sigma
  }

  z
}
