# Proportion of imputations that must converge before a pooled estimate is
# reported without a caveat. Shared by model_builder(), pool_effects() and
# make_model_tables(), which previously each hardcoded 0.75 / "75\\%".
convergence_threshold <- 0.75

# The optimizer escalation ladder fit_model() walks. Transcribed from
# lme4:::meth.tab.0 (lme4 2.0.6) so the pipeline no longer reaches into an
# unexported object. Each entry carries its own iteration-cap argument name,
# which used to live in a parallel `maxit_names` list keyed by row position —
# so a reorder upstream would have silently paired the wrong cap with the wrong
# optimizer. check_model_environment() compares this against lme4's own table
# when it can still be reached.
optimizer_ladder <- list(
  list(optimizer = "bobyqa", method = "", max_iter_arg = "maxfun"),
  list(optimizer = "Nelder_Mead", method = "", max_iter_arg = "maxfun"),
  list(
    optimizer = "nlminbwrap",
    method = "",
    max_iter_arg = c("iter.max", "eval.max")
  ),
  list(optimizer = "nmkbw", method = "", max_iter_arg = "maxfeval"),
  list(optimizer = "optimx", method = "L-BFGS-B", max_iter_arg = "maxit"),
  list(
    optimizer = "nloptwrap",
    method = "NLOPT_LN_NELDERMEAD",
    max_iter_arg = "maxeval"
  ),
  list(
    optimizer = "nloptwrap",
    method = "NLOPT_LN_BOBYQA",
    max_iter_arg = "maxeval"
  )
)

check_model_environment <- function() {
  require(lme4)

  # Soft check: warn if lme4's own ladder has moved away from our transcription.
  # Not an error — our copy is authoritative for this analysis, and lme4 is free
  # to drop the internal entirely.
  upstream <- tryCatch(
    utils::getFromNamespace("meth.tab.0", "lme4"),
    error = function(e) NULL
  )
  if (!is.null(upstream)) {
    ours <- cbind(
      vapply(optimizer_ladder, `[[`, character(1), "optimizer"),
      vapply(optimizer_ladder, `[[`, character(1), "method")
    )
    if (!identical(unname(as.matrix(upstream)), unname(ours))) {
      warning(
        "lme4's optimizer table no longer matches optimizer_ladder in ",
        "R/model_builder.R. The analysis still uses optimizer_ladder; check ",
        "whether the upstream change should be adopted. See E2 in ",
        "CODE_REVIEW.md.",
        call. = FALSE
      )
    }
  }

  fit <- lme4::lmer(
    Reaction ~ Days + (Days | Subject),
    data = lme4::sleepstudy,
    control = lme4::lmerControl(optimizer = "bobyqa", calc.derivs = TRUE)
  )

  if (is.null(fit@optinfo$derivs) || !isTRUE(is_converged(fit))) {
    stop(
      "lme4 ",
      utils::packageVersion("lme4"),
      " leaves @optinfo$derivs empty even with calc.derivs = TRUE, so ",
      "is_converged() can never return TRUE. fit_model() would run all seven ",
      "optimizers on every fit",
      call. = FALSE
    )
  }

  use_fixed_effects_prediction_se()

  probe <- suppressWarnings(stats::predict(
    fit,
    newdata = data.frame(Days = 0:4, Subject = lme4::sleepstudy$Subject[1]),
    re.form = NA,
    allow.new.levels = TRUE,
    se.fit = TRUE
  ))

  if (is.list(probe)) {
    stop(
      "predict.merMod() is still returning standard errors, so ggeffects ",
      "(>= 1.3.3) will take them from lme4::vcov_full() — the joint ",
      "fixed+random covariance, 24,638 x 24,638 here, rebuilt on every ",
      "ggpredict() call at ~44 s and ~11 GB each. ",
      "use_fixed_effects_prediction_se() did not take effect. ",
      call. = FALSE
    )
  }

  invisible(TRUE)
}

use_fixed_effects_prediction_se <- function() {
  lme4_predict <- utils::getS3method(
    "predict",
    "merMod",
    envir = asNamespace("lme4")
  )

  if (!"se.fit" %in% names(formals(lme4_predict))) {
    return(invisible(FALSE))
  }

  shim <- function(object, ..., se.fit = FALSE) {
    lme4_predict(object, ..., se.fit = FALSE)
  }

  registerS3method("predict", "merMod", shim, envir = asNamespace("stats"))

  invisible(TRUE)
}

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

  while (!conv & i <= length(optimizer_ladder)) {
    step <- optimizer_ladder[[i]]

    optCtrl <- if (nzchar(step$method)) list(method = step$method) else list()

    for (nm in step$max_iter_arg) {
      optCtrl[[nm]] <- max_iter
    }

    mod <- lme4::lmer(
      ...,
      data = data,
      control = lmerControl(
        optimizer = step$optimizer,
        optCtrl = optCtrl,
        # Explicit because lme4 >= 2.0 changed the default
        calc.derivs = TRUE
      )
    )
    if (i == 1L && is.null(mod@optinfo$derivs)) {
      stop(
        "lmer() returned no @optinfo$derivs despite calc.derivs = TRUE, so ",
        "is_converged() can never return TRUE and every model would be ",
        "recorded as unconverged. See C24 in CODE_REVIEW.md.",
        call. = FALSE
      )
    }

    mod@call$control$optimizer <- step$optimizer
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
    RQ,
    min_wear_days = 0
  ) {
    require(broom.mixed)
    require(lme4)
    require(data.table)

    # Must run inside the worker, before any ggpredict() call below. See the
    # function's @details: without it each prediction costs ~44 s and ~11 GB.
    use_fixed_effects_prediction_se()

    # Same reason — this runs in the crew worker, and the BLAS thread pool is
    # per-process. See cap_blas_threads().
    cap_blas_threads()

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
        # Half the resolution of terms[1] — this grid feeds the heat-map panel,
        # where each cell is a tile rather than a point on a line.
        predictor_grid(data_imp, predictor_term, n = 100),
        predictor_grid(data_imp, "age", step = 1)
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

      if (min_wear_days > 0) {
        if (is.null(dat[["n_valid_wear_days"]])) {
          stop(
            "min_wear_days = ",
            min_wear_days,
            " but the imputed data has no n_valid_wear_days column. ",
            "clean_data() adds it and make_data_imp() must carry it through.",
            call. = FALSE
          )
        }
        dat <- dat[dat[["n_valid_wear_days"]] >= min_wear_days, ]
      }

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
    # pool.table(type = "all") already returns conf.low/conf.high on the pooled
    # Barnard-Rubin degrees of freedom, the same df behind `statistic` and
    # `p.value` below. Building the interval from qnorm() instead let the CI and
    # the p-value disagree at the significance boundary, and the gap is widest
    # exactly where it matters: df shrink as the fraction of missing information
    # rises.
    pool_summary$lower <- papaja::print_num(pool_summary$conf.low)
    pool_summary$upper <- papaja::print_num(pool_summary$conf.high)

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
    # conv_p is the proportion of imputations that DID converge, so the note is
    # built from it rather than defaulting to "All models converged." — which
    # previously reported anything from 74% to 99% as if it were 100%.
    conv_print <- papaja::print_num(conv_p * 100)
    if (conv_p == 1) {
      note <- "All models converged."
    } else if (conv_p >= convergence_threshold) {
      note <- as.character(glue::glue(
        "{conv_print}% of models converged."
      ))
    } else {
      tabby$`b [95\\% CI]` <- paste0(tabby$`b [95\\% CI]`, "$^\\dagger$")
      note <- as.character(glue::glue(
        "$^\\dagger$ these values were derived from a pooled model where only ",
        "{conv_print}% of models converged."
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

  # (1 - conv) is the proportion that did NOT converge, which is what the
  # overlay reports.
  conv_print <- paste0(papaja::print_num((1 - conv) * 100), "%")

  dt <- data.table(effects)
  dt$x_name <- terms[1]
  dt$group_name <- terms[2]
  dt$outcome <- outcome
  dt$RQ <- RQ
  dt$moderator <- moderator
  dt$conv_p <- conv
  if (conv < convergence_threshold) {
    dt$message <- as.character(glue::glue("DID NOT CONVERGE ({conv_print})"))
  } else {
    dt$message <- " "
  }
  attr(dt, "conv") <- conv
  dt
}
