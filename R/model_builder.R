is_conv <- function(x) performance::check_convergence(x) & !performance::check_singularity(x)

#' fit_model
#'
#' A function to fit a model with a range of optimizers
#' @param ... arguments passed to lmer
#' @param data data object

fit_model <- function(..., data, max_iter = 1e6) {
  require(optimx)
  require(lme4)
  require(dfoptim)

  conv <- FALSE
  exhausted <- FALSE
  i <- 1
  meth.tab <- lme4:::meth.tab.0
  meth.tab <- cbind(meth.tab, maxit_name = c("maxfun", "maxfun", "maxit", "maxfeval", "maxit", "maxeval", "maxeval"))
  meth.tab <- meth.tab[sample(seq_len(nrow(meth.tab)), nrow(meth.tab), replace = FALSE), ]

  while (!conv & i <= nrow(meth.tab)) {
    if (meth.tab[i, 2] != "") {
      optCtrl <- list(method = unname(meth.tab[i, 2]))
    } else {
      optCtrl <- list()
    }

    optCtrl[[meth.tab[i, 3]]] <- max_iter

    mod <- lme4::lmer(
      ...,
      data = data,
      control = lmerControl(
        optimizer = meth.tab[i, 1],
        optCtrl = optCtrl
      )
    )
    mod@call$control$optimizer <- unname(meth.tab[i, 1])
    mod@call$control$optCtrl <- unlist(optCtrl)

    if (is_conv(mod)) {
      conv <- TRUE
      attr(mod, "conv") <- TRUE
      return(mod)
    }
    i <- i + 1
  }
  warning("No convergence with any optimizer:", ...)
  attr(mod, "conv") <- FALSE
  mod@call$formula <- butcher::axe_env(mod@call$formula)
  mod
}



#' model_builder
#'
#' Create models for data_imp
#' @param data_imp mids object
#' @param outcome chatacter. outcome variable name
#' @param predictors a chatacter vector of predictors
#' @param moderator a character. Variable name of moderator
#' @param control_vars a vector of control variables
#' @param table_only if TRUE, only the table will be retured
#' @param ranef random effects to paste to formula
#' @param terms character string of terms to pass to ggeffects
#' @param RQ numeric to pass to get_effects
#' @protocol to examine the relationship between sleep and physical activity (Research Questions 1-2) we will use study fixed-effects to account for the nesting of participants in studies (Curran et al 2009). Fixed-effects (not the same as complete pooling analysis that ignores data nesting) control for all time-invariant between-study variance and will allow us to explore within study associations and moderators. We will nest individuals within days, and days within study. We will examine both main effects and subpopulation effects (using separate models), including the following pre-specified individual-level moderators; age (chronological), body mass index z-score (z transformed), SES, ethnicity, and sex as categorical. Day of the week, season (summer vs winter), geographic location, and daylight length will also be included as moderators because these influence sleep and physical activity. Accelerometer wear location will be included as a moderator. Sleep and physical activity may be temporally related where early morning and late evening physical activity can negatively influence optimum sleep duration and sleep quality. To account for this, we will include the time of the day corresponding to the most active periods of physical activity as a moderator. The most active 60, 30, 15, 10 and 5 minutes within 4 windows of time; midnight to 6am (early), 6am to 12pm (normal), 12pm -6pm (normal), 6pm -midnight (late) will be extracted from GGIR and used to test the effect of physical activity proximity to bedtime and wake time on sleep.

#' @test-arguments outcome = "sleep_duration", predictors = "scale_pa_volume * age + I(scale_pa_volume^2) * age", control_vars = c(), table_only = FALSE, ranef  = "(1|studyid) + (1|participant_id)", terms = c("scale_pa_volume[-4:4 by = 0.1]", "age [11, 18, 35, 65]"), moderator = "age"

model_builder <-
  function(data_imp,
           outcome,
           predictors,
           moderator,
           control_vars = c(),
           table_only = TRUE,
           ranef,
           terms,
           RQ) {
    require(broom.mixed)
    require(lme4)
    require(data.table)

    formula <-
      glue::glue(
        "{outcome} ~ {paste(predictors, collapse = ' + ')} + {paste(control_vars, collapse = ' + ')} + {ranef}"
      )

    formula <- gsub("\\+  \\+", "+", formula)

    imp_list <- mice::complete(data_imp, "all")

    m <- lapply(imp_list, function(x) {
      mod <- fit_model(formula = eval(parse(text = formula)), data = x)
      mod
    })

    conv <- sapply(m, function(x) is_conv(x))
    conv_p <- sum(conv) / length(conv)

    m_pooled <- mice::pool(m)
    pool_summary <- data.table(summary(m_pooled))

    crit.val <- qnorm(1 - 0.05 / 2)
    pool_summary$lower <-
      papaja::print_num(with(pool_summary, estimate - crit.val * std.error))
    pool_summary$upper <-
      papaja::print_num(with(pool_summary, estimate + crit.val * std.error))

    tabby <- data.table(pool_summary)[
      ,
      list(
        term = term,
        "b [95\\% CI]" = with(
          pool_summary,
          glue::glue("{papaja::print_num(estimate)} [{lower}, {upper}]")
        ),
        se = papaja::print_num(std.error),
        t = papaja::print_num(statistic),
        p = papaja::print_p(p.value)
      )
    ]
    note <- "All models converged." # Add blank note

    if (conv_p < .75) {
      conv_print <- papaja::print_num(conv_p * 100)
      tabby$`b [95\\% CI]` <- paste0(tabby$`b [95\\% CI]`, "$^\\dagger$")
      note <- as.character(glue::glue("$^\\dagger$ these values were derived from a pooled model where fewer than {conv_print}% of models had converged."))
    }

    tabby <- tabby[!grepl("studyid", term), ]

    if (table_only) {
      return(tabby)
    }

    # Model_assets
    model_assets <- list(effects = get_effects(
      m,
      moderator = moderator,
      terms = terms,
      outcome = outcome,
      conv = conv_p,
      RQ = RQ
    ),
    conv = conv_p,
    diagnostics = check_model(m, conv = conv_p))

    list(
      model_assets = model_assets,
      pooled_model = m_pooled,
      table = tabby,
      note = note,
      control_vars = control_vars
    )
  }

#' get_effects
#'
#' Plot an effects display for a RQ1 model
#' @param model a list delivered by model_builder
#' @param moderator character. Moderator name
#' @param terms character vector. terms for effect predictors
#' @param outcome character. outcome variable
#' @param conv convergence as a proportion
#' @param RQ Research question number
#' @param ... additional arguments passed to the ggeffect engine
#' @example model = rq1_example_model, terms = c("pa_intensity", "pa_volume")

get_effects <- function(model, moderator, terms, outcome, conv, RQ, ...) {

  effects <-
    lapply(seq_len(length(model)), function(i) {
      suppressMessages(ggeffects::ggpredict(model[[i]], terms = terms, ...))
    }) |> ggeffects::pool_predictions()

  conv_print <- paste0(papaja::print_num((1 -conv) * 100), "%")

  dt <- data.table(effects)
  dt$x_name <- terms[1]
  dt$group_name <- terms[2]
  dt$outcome <- outcome
  dt$RQ <- RQ
  dt$moderator <- moderator
  dt$conv_p <- conv
  if(conv < .75){
    dt$message <- as.character(glue::glue("DID NOT CONVERGE ({conv_print})"))
  }else{
    dt$message <- " "
  }
  attr(dt, "conv") <- conv
  dt

}



