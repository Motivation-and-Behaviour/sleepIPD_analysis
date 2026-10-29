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

fit_ladder <- c(
  list(list(optimizer = "nloptwrap", method = "", max_iter_arg = "maxeval")),
  optimizer_ladder
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
        "whether the upstream change should be adopted.",
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

  while (!conv & i <= length(fit_ladder)) {
    step <- fit_ladder[[i]]

    # nloptwrap passes its control to nloptr, which silently ignores `method`
    method_arg <- if (step$optimizer == "nloptwrap") "algorithm" else "method"
    optCtrl <- if (nzchar(step$method)) {
      stats::setNames(list(step$method), method_arg)
    } else {
      list()
    }

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
        "recorded as unconverged.",
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


#' analysed_rows
#'
#' Rows of `data_imp` a model analyses: outcome, exposure and moderator
#' observed rather than imputed (covariates may be imputed), and at least
#' `min_wear_days` valid wear days. Shared by `model_builder()` and the
#' prediction grids, so the grids span the data the model fits.
#' @inheritParams model_builder
#' @return logical vector over `data_imp$data`
analysed_rows <- function(
  data_imp,
  outcome,
  predictors,
  moderator,
  min_wear_days = 0
) {
  observed <- c(outcome, gsub(" .*", "", predictors[1]), moderator)
  not_in_mids <- setdiff(observed, colnames(data_imp$where))
  if (length(not_in_mids) > 0) {
    stop(
      "Cannot tell which values were imputed for: ",
      paste(not_in_mids, collapse = ", "),
      call. = FALSE
    )
  }
  rows <- rowSums(data_imp$where[, observed, drop = FALSE]) == 0

  if (min_wear_days > 0) {
    wear <- data_imp$data[["n_valid_wear_days"]]
    if (is.null(wear)) {
      stop(
        "min_wear_days = ",
        min_wear_days,
        " but the imputed data has no n_valid_wear_days column. ",
        "clean_data() adds it and make_data_imp() must carry it through.",
        call. = FALSE
      )
    }
    rows <- rows & !is.na(wear) & wear >= min_wear_days
  }

  rows
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
#' @param min_wear_days participants with fewer valid wear days are dropped
#' @param age_centre age (years) that age is centred on in the formula
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
    min_wear_days = 0,
    age_centre
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

    formula_controls <- formula_term(control_vars, age_centre)
    formula <-
      glue::glue(
        "{outcome} ~ {paste(predictors, collapse = ' + ')} + {paste(formula_controls, collapse = ' + ')} + {ranef}"
      )

    formula <- gsub("\\+  \\+", "+", formula)
    model_formula <- stats::as.formula(formula, env = globalenv())

    exposure <- gsub(" .*", "", predictors[1])
    rows <- analysed_rows(data_imp, outcome, predictors, moderator, min_wear_days)

    # Granular age grid for the heat-map panel of the main figure
    if (!table_only && moderator == "age") {
      predictor_term <- gsub("\\[.*", "", terms[1])
      age_terms <- c(
        # Half the resolution of terms[1] — this grid feeds the heat-map panel,
        # where each cell is a tile rather than a point on a line.
        predictor_grid(data_imp, predictor_term, n = 100, rows = rows),
        predictor_grid(data_imp, "age", step = 1, rows = rows)
      )
    } else {
      age_terms <- NULL
    }

    # Slopes at each age in terms[2], pooled with Rubin's rules below
    if (!table_only && moderator == "age") {
      slope_ages <- as.numeric(strsplit(
        gsub(".*\\[|\\].*", "", terms[2]),
        ","
      )[[1]])
      # Within-between models also get slopes for the person-mean term
      slope_x <- unique(c(exposure, sub("_wp(_lag)?$", "_pm", exposure)))
      slope_weights <- (slope_ages - age_centre) / 10
      slope_dim <- c(data_imp$m, length(slope_ages), length(slope_x))
      slope_q <- array(NA_real_, slope_dim)
      slope_u <- array(NA_real_, slope_dim)
    } else {
      slope_ages <- NULL
    }

    n_imp <- data_imp$m

    tidy_list <- vector("list", n_imp)
    effect_list <- vector("list", n_imp)
    age_effect_list <- vector("list", n_imp)
    resid_list <- vector("list", n_imp)
    converged <- logical(n_imp)
    singular <- logical(n_imp)

    for (i in seq_len(n_imp)) {
      dat <- mice::complete(data_imp, i)[rows, ]

      mod <- fit_model(formula = model_formula, data = dat)

      converged[i] <- is_converged(mod)
      singular[i] <- is_singular(mod)

      # Complete-data df (n - k) for Barnard-Rubin pooling of the slopes and
      # the effects displays
      pool_n <- stats::nobs(mod)
      pool_k <- length(lme4::fixef(mod))

      tidy_i <- data.table(broom.mixed::tidy(mod, effects = "fixed"))
      tidy_i[, `:=`(.imp = i, df.residual = stats::df.residual(mod))]
      tidy_list[[i]] <- tidy_i

      resid_list[[i]] <- resid_moments(mod)

      if (!is.null(slope_ages)) {
        b <- lme4::fixef(mod)
        v <- as.matrix(stats::vcov(mod))
        for (s in seq_along(slope_x)) {
          sx <- slope_x[s]
          # lme4 orders an interaction's parts by first appearance in the formula
          sxa <- intersect(
            paste0(c(sx, age_term(age_centre)), ":", c(age_term(age_centre), sx)),
            names(b)
          )
          if (!sx %in% names(b) || length(sxa) != 1) {
            stop(
              "Age-specific slopes need coefficient '",
              sx,
              "' and its interaction with age, but the model has: ",
              paste(names(b), collapse = ", "),
              call. = FALSE
            )
          }
          slope_q[i, , s] <- b[[sx]] + slope_weights * b[[sxa]]
          slope_u[i, , s] <- v[sx, sx] +
            slope_weights^2 * v[sxa, sxa] +
            2 * slope_weights * v[sx, sxa]
        }
      }

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
        RQ = RQ,
        n = pool_n,
        k = pool_k
      )
    } else {
      pred_mat <- NULL
    }

    if (!is.null(slope_ages)) {
      cells <- expand.grid(j = seq_along(slope_ages), s = seq_along(slope_x))
      age_slopes <- rbindlist(lapply(seq_len(nrow(cells)), function(r) {
        j <- cells$j[r]
        s <- cells$s[r]
        pooled <- mice::pool.scalar(
          slope_q[, j, s],
          slope_u[, j, s],
          n = pool_n,
          k = pool_k
        )
        se <- sqrt(pooled$t)
        crit <- stats::qt(0.975, pooled$df)
        data.table(
          term = slope_x[s],
          age = slope_ages[j],
          estimate = pooled$qbar,
          std.error = se,
          conf.low = pooled$qbar - crit * se,
          conf.high = pooled$qbar + crit * se,
          p.value = 2 * stats::pt(-abs(pooled$qbar / se), pooled$df)
        )
      }))
    } else {
      age_slopes <- NULL
    }

    # Model_assets
    model_assets <- list(
      age_slopes = age_slopes,
      # Person-days in each fit
      n_obs = pool_n,
      effects = pool_effects(
        effect_list,
        moderator = moderator,
        terms = terms,
        outcome = outcome,
        conv = conv_p,
        RQ = RQ,
        n = pool_n,
        k = pool_k
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
#' @param n,k observations and fixed effects in each fit; see
#'   `pool_predictions_rubin()`

pool_effects <- function(
  predictions,
  moderator,
  terms,
  outcome,
  conv,
  RQ,
  n,
  k
) {
  effects <- pool_predictions_rubin(predictions, n = n, k = k)

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

#' pool_predictions_rubin
#'
#' Pool per-imputation `ggeffects` predictions point by point with Rubin's
#' rules.
#'
#' @param predictions list of ggeffects objects, one per imputation
#' @param n,k observations and fixed effects in each fit, giving the
#'   complete-data df (n - k) for the Barnard-Rubin adjustment
#' @param ci_level confidence level of the intervals
#'
#' @details Replaces `ggeffects::pool_predictions()`. Its pooled standard
#' errors are right, but (as of 2.3.2) it computes a single Barnard-Rubin df
#' for the whole display with `m` set to the number of grid points and `b` to
#' the variance of the pooled curve across the grid, not between imputations.
#' The steeper or more widely separated the curves, the smaller that df, down
#' to a floor of 1, so the intervals on the strongest associations were up to
#' 6.5 times too wide. Here each grid point is pooled with
#' `mice::pool.scalar()`, the same pooling behind the age-specific slopes, and
#' gets its own df.
pool_predictions_rubin <- function(predictions, n, k, ci_level = 0.95) {
  frames <- lapply(predictions, as.data.frame)
  value_cols <- c("predicted", "std.error", "conf.low", "conf.high")
  key_cols <- setdiff(names(frames[[1]]), value_cols)

  same_grid <- vapply(
    frames,
    function(f) {
      identical(names(f), names(frames[[1]])) &&
        all(vapply(
          key_cols,
          function(col) identical(f[[col]], frames[[1]][[col]]),
          logical(1)
        ))
    },
    logical(1)
  )
  if (!all(same_grid)) {
    stop(
      "Cannot pool predictions: the prediction grid (",
      paste(key_cols, collapse = ", "),
      ") differs between imputations.",
      call. = FALSE
    )
  }

  q <- do.call(cbind, lapply(frames, `[[`, "predicted"))
  u <- do.call(cbind, lapply(frames, `[[`, "std.error"))^2

  pooled <- lapply(seq_len(nrow(q)), function(r) {
    mice::pool.scalar(q[r, ], u[r, ], n = n, k = k)
  })
  qbar <- vapply(pooled, `[[`, numeric(1), "qbar")
  se <- sqrt(vapply(pooled, `[[`, numeric(1), "t"))
  df <- vapply(pooled, `[[`, numeric(1), "df")
  crit <- stats::qt((1 + ci_level) / 2, df)

  out <- frames[[1]]
  out$predicted <- qbar
  out$std.error <- se
  out$conf.low <- qbar - crit * se
  out$conf.high <- qbar + crit * se
  out$df <- df
  out
}
