#' is_converged
#'
#' Did the optimizer converge?
#' @param x a fitted model
is_converged <- function(x) performance::check_convergence(x)

#' is_singular
#'
#' Is at least one variance component estimated at the boundary?
#' @param x a fitted model
is_singular <- function(x) performance::check_singularity(x)

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
  i <- 1
  meth.tab <- lme4:::meth.tab.0

  maxit_names <- list(
    "maxfun", # bobyqa
    "maxfun", # Nelder_Mead
    c("iter.max", "eval.max"), # nlminbwrap
    "maxfeval", # nmkbw
    "maxit", # optimx (L-BFGS-B)
    "maxeval", # nloptwrap (NLOPT_LN_NELDERMEAD)
    "maxeval" # nloptwrap (NLOPT_LN_BOBYQA)
  )
  stopifnot(length(maxit_names) == nrow(meth.tab))

  while (!conv & i <= nrow(meth.tab)) {
    if (meth.tab[i, 2] != "") {
      optCtrl <- list(method = unname(meth.tab[i, 2]))
    } else {
      optCtrl <- list()
    }

    for (nm in maxit_names[[i]]) {
      optCtrl[[nm]] <- max_iter
    }

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

    if (is_converged(mod)) {
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
#' @param RQ numeric to pass to pool_effects
#' @protocol to examine the relationship between sleep and physical activity (Research Questions 1-2) we will use study fixed-effects to account for the nesting of participants in studies (Curran et al 2009). Fixed-effects (not the same as complete pooling analysis that ignores data nesting) control for all time-invariant between-study variance and will allow us to explore within study associations and moderators. We will nest individuals within days, and days within study. We will examine both main effects and subpopulation effects (using separate models), including the following pre-specified individual-level moderators; age (chronological), body mass index z-score (z transformed), SES, ethnicity, and sex as categorical. Day of the week, season (summer vs winter), geographic location, and daylight length will also be included as moderators because these influence sleep and physical activity. Accelerometer wear location will be included as a moderator. Sleep and physical activity may be temporally related where early morning and late evening physical activity can negatively influence optimum sleep duration and sleep quality. To account for this, we will include the time of the day corresponding to the most active periods of physical activity as a moderator. The most active 60, 30, 15, 10 and 5 minutes within 4 windows of time; midnight to 6am (early), 6am to 12pm (normal), 12pm -6pm (normal), 6pm -midnight (late) will be extracted from GGIR and used to test the effect of physical activity proximity to bedtime and wake time on sleep.

#' @test-arguments outcome = "sleep_duration", predictors = "scale_pa_volume * age + I(scale_pa_volume^2) * age", control_vars = c(), table_only = FALSE, ranef  = "(1|studyid) + (1|participant_id)", terms = c("scale_pa_volume[-4:4 by = 0.1]", "age [11, 18, 35, 65]"), moderator = "age"

model_builder <-
  function(
    data_imp,
    outcome,
    predictors,
    moderator,
    control_vars = c(),
    table_only = TRUE,
    ranef,
    terms,
    RQ
  ) {
    require(broom.mixed)
    require(lme4)
    require(data.table)

    formula <-
      glue::glue(
        "{outcome} ~ {paste(predictors, collapse = ' + ')} + {paste(control_vars, collapse = ' + ')} + {ranef}"
      )

    formula <- gsub("\\+  \\+", "+", formula)
    model_formula <- stats::as.formula(formula, env = globalenv())

    # Granular age grid for the heat-map panel of the main figure
    if (!table_only && moderator == "age") {
      predictor_term <- gsub("\\[.*", "", terms[1])
      age_terms <- c(
        paste0(predictor_term, "[-5:5 by=0.1]"),
        "age[10:80 by = 1]"
      )
    } else {
      age_terms <- NULL
    }

    n_imp <- data_imp$m

    tidy_list <- vector("list", n_imp)
    effect_list <- vector("list", n_imp)
    age_effect_list <- vector("list", n_imp)
    resid_list <- vector("list", n_imp)
    converged <- logical(n_imp)
    singular <- logical(n_imp)

    for (i in seq_len(n_imp)) {
      dat <- mice::complete(data_imp, i)
      mod <- fit_model(formula = model_formula, data = dat)

      converged[i] <- is_converged(mod)
      singular[i] <- is_singular(mod)

      tidy_i <- data.table(broom.mixed::tidy(mod, effects = "fixed"))
      tidy_i[, `:=`(.imp = i, df.residual = stats::df.residual(mod))]
      tidy_list[[i]] <- tidy_i

      resid_list[[i]] <- resid_moments(mod)

      if (!table_only) {
        effect_list[[i]] <-
          suppressMessages(ggeffects::ggpredict(mod, terms = terms))
        if (!is.null(age_terms)) {
          age_effect_list[[i]] <-
            suppressMessages(ggeffects::ggpredict(mod, terms = age_terms))
        }
      }

      rm(mod, dat)
    }

    conv_p <- mean(converged)
    sing_p <- mean(singular)

    # pool.table() applies Rubin's rules to the stacked tidy estimates.
    pool_summary <- data.table(
      mice::pool.table(rbindlist(tidy_list), type = "all")
    )
    crit.val <- qnorm(1 - 0.05 / 2)
    pool_summary$lower <-
      papaja::print_num(with(pool_summary, estimate - crit.val * std.error))
    pool_summary$upper <-
      papaja::print_num(with(pool_summary, estimate + crit.val * std.error))

    tabby <- data.table(pool_summary)[,
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
      note <- as.character(glue::glue(
        "$^\\dagger$ these values were derived from a pooled model where fewer than {conv_print}% of models had converged."
      ))
    }

    tabby <- tabby[!grepl("studyid", term), ]

    if (table_only) {
      return(tabby)
    }

    if (!is.null(age_terms)) {
      pred_mat <- pool_effects(
        age_effect_list,
        moderator = "age",
        terms = age_terms,
        outcome = outcome,
        conv = conv_p,
        RQ = RQ
      )
    } else {
      pred_mat <- NULL
    }

    # Model_assets
    model_assets <- list(
      effects = pool_effects(
        effect_list,
        moderator = moderator,
        terms = terms,
        outcome = outcome,
        conv = conv_p,
        RQ = RQ
      ),
      conv = conv_p,
      diagnostics = check_model(resid_list, conv = conv_p, singular = sing_p),
      pred_matrix = pred_mat
    )

    list(
      model_assets = model_assets,
      table = tabby,
      note = note,
      control_vars = control_vars
    )
  }

#' pool_effects
#'
#' Pool an effects display across imputations.
#' Takes the per-imputation `ggeffects` predictions rather than the fitted
#' models, so the fits can be discarded as soon as they are made.
#' @param predictions list of ggeffects objects, one per imputation
#' @param moderator character. Moderator name
#' @param terms character vector. terms the predictions were made over
#' @param outcome character. outcome variable
#' @param conv convergence as a proportion
#' @param RQ Research question number

pool_effects <- function(predictions, moderator, terms, outcome, conv, RQ) {
  effects <- ggeffects::pool_predictions(predictions)

  conv_print <- paste0(papaja::print_num((1 - conv) * 100), "%")

  dt <- data.table(effects)
  dt$x_name <- terms[1]
  dt$group_name <- terms[2]
  dt$outcome <- outcome
  dt$RQ <- RQ
  dt$moderator <- moderator
  dt$conv_p <- conv
  if (conv < .75) {
    dt$message <- as.character(glue::glue("DID NOT CONVERGE ({conv_print})"))
  } else {
    dt$message <- " "
  }
  attr(dt, "conv") <- conv
  dt
}
