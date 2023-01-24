
#' model_builder
#'
#' Create models for data_imp
#' @param data_imp mids object
#' @param outcome chatacter. outcome variable name
#' @param predictors a chatacter vector of predictors
#' @param table_only if TRUE, only the table will be retured
#' @protocol to examine the relationship between sleep and physical activity (Research Questions 1-2) we will use study fixed-effects to account for the nesting of participants in studies (Curran et al 2009). Fixed-effects (not the same as complete pooling analysis that ignores data nesting) control for all time-invariant between-study variance and will allow us to explore within study associations and moderators. We will nest individuals within days, and days within study. We will examine both main effects and subpopulation effects (using separate models), including the following pre-specified individual-level moderators; age (chronological), body mass index z-score (z transformed), SES, ethnicity, and sex as categorical. Day of the week, season (summer vs winter), geographic location, and daylight length will also be included as moderators because these influence sleep and physical activity. Accelerometer wear location will be included as a moderator. Sleep and physical activity may be temporally related where early morning and late evening physical activity can negatively influence optimum sleep duration and sleep quality. To account for this, we will include the time of the day corresponding to the most active periods of physical activity as a moderator. The most active 60, 30, 15, 10 and 5 minutes within 4 windows of time; midnight to 6am (early), 6am to 12pm (normal), 12pm -6pm (normal), 6pm -midnight (late) will be extracted from GGIR and used to test the effect of physical activity proximity to bedtime and wake time on sleep.

#' @test-arguments outcome = "sleep_duration" predictors = c("pa_volume", "pa_intensity")

model_builder_RQ1 <- function(data_imp, outcome, predictors, table_only = TRUE){

  require(data.table)
  require(papaja)

  formula <- glue::glue("{outcome} ~ {paste(predictors, collapse = ' + ')} + factor(studyid) + factor(measurementday) + factor(participant_id)")

  m <- eval(parse(text = glue::glue("with(data_imp, lm(formula = {formula}))")))

  m_pooled <- pool(m)
  pool_summary <- data.table(summary(m_pooled))

  pool_summary <- pool_summary[!grepl("studyid",pool_summary$term),]
  pool_summary <- pool_summary[!grepl("night_number",pool_summary$term),]
  pool_summary <- pool_summary[!grepl("participant_id",pool_summary$term),]

  crit.val <- qnorm(1 - 0.05 / 2)

  pool_summary$lower <- print_num(with(pool_summary, estimate - crit.val * std.error))
  pool_summary$upper <- print_num(with(pool_summary, estimate + crit.val * std.error))

  tabby <- data.table(pool_summary)[
    ,
    list(
      term = term,
    b = with(pool_summary, glue::glue("{print_num(estimate)} [95% CI {lower}, {upper}]")),
    se = print_num(std.error),
    t = print_num(statistic),
     p = print_p(p.value)

    )
  ]

  if(table_only) return(rabby)

  list(model = m,
       pooled_model = pool(m),
       table = tabby)

}
