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
#' @param adult_ref optional `list(mean =, sd =)` from `bmi_z_adult_ref()`. Pass
#'   one shared set when standardising multiple imputations, so they end up on
#'   the same scale. Computed from the data given when NULL.
#' @return numeric vector of z-scores
#'
make_bmi_z <- function(
  bmi,
  age,
  sex,
  participant_id,
  adult_age = 18,
  adult_ref = NULL
) {
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
    if (is.null(adult_ref)) {
      adult_ref <- bmi_z_adult_ref(bmi, age, participant_id, adult_age)
    }
    z[is_adult] <- (bmi[is_adult] - adult_ref$mean) / adult_ref$sd
  }

  z
}

#' bmi_z_adult_ref
#'
#' Mean and SD of adult BMI, one row per participant.
#'
#' @param participant_id participant key; include the imputation number when
#'   the input stacks several imputations, so each is counted once
#' @return list with `mean` and `sd`
#'
bmi_z_adult_ref <- function(bmi, age, participant_id, adult_age = 18) {
  rows <- !is.na(age) & age >= adult_age & !duplicated(participant_id)
  list(
    mean = mean(bmi[rows], na.rm = TRUE),
    sd = stats::sd(bmi[rows], na.rm = TRUE)
  )
}
