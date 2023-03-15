
#' model_builder
#'
#' Create models for data_imp
#' @param data_imp mids object
#' @param outcome chatacter. outcome variable name
#' @param predictors a chatacter vector of predictors
#' @param table_only if TRUE, only the table will be retured
#' @protocol to examine the relationship between sleep and physical activity (Research Questions 1-2) we will use study fixed-effects to account for the nesting of participants in studies (Curran et al 2009). Fixed-effects (not the same as complete pooling analysis that ignores data nesting) control for all time-invariant between-study variance and will allow us to explore within study associations and moderators. We will nest individuals within days, and days within study. We will examine both main effects and subpopulation effects (using separate models), including the following pre-specified individual-level moderators; age (chronological), body mass index z-score (z transformed), SES, ethnicity, and sex as categorical. Day of the week, season (summer vs winter), geographic location, and daylight length will also be included as moderators because these influence sleep and physical activity. Accelerometer wear location will be included as a moderator. Sleep and physical activity may be temporally related where early morning and late evening physical activity can negatively influence optimum sleep duration and sleep quality. To account for this, we will include the time of the day corresponding to the most active periods of physical activity as a moderator. The most active 60, 30, 15, 10 and 5 minutes within 4 windows of time; midnight to 6am (early), 6am to 12pm (normal), 12pm -6pm (normal), 6pm -midnight (late) will be extracted from GGIR and used to test the effect of physical activity proximity to bedtime and wake time on sleep.

#' @test-arguments outcome = "sleep_duration" predictors = c("pa_volume", "pa_intensity")

model_builder <-
  function(data_imp,
           outcome,
           predictors,
           control_vars = c(),
           table_only = TRUE) {

    require(broom.mixed)
    require(lme4)
    require(data.table)

    formula <-
      glue::glue(
        "{outcome} ~ {paste(predictors, collapse = ' + ')} + {paste(control_vars, collapse = ' + ')} + (1|participant_id) + (1|measurement_day)"
      )

    formula <- gsub("\\+  \\+", "+", formula)

    fit_model <- function(..., data, max_iter = 1e6){
      require(optimx)
      require(lme4)
      require(dfoptim)

      conv <- FALSE
      exhausted <- FALSE
      i <- 1
      meth.tab <- lme4:::meth.tab.0
      meth.tab <- cbind(meth.tab, maxit_name = c("maxfun", "maxfun", "maxit", "maxfeval", "maxit", "maxeval","maxeval"))
      meth.tab <- meth.tab[sample(seq_len(nrow(meth.tab)), nrow(meth.tab), replace = FALSE), ]

      while(!conv & i <= nrow(meth.tab)) {

        if(meth.tab[i , 2] != ""){
          optCtrl <- list(method = meth.tab[i , 2])
        }else{
          optCtrl <- list()
        }

        optCtrl[[meth.tab[i , 3]]] <- max_iter

        mod <- lme4::lmer(
          ...,
          data,
          control = lmerControl(
            optimizer = meth.tab[i, 1],
            optCtrl = optCtrl
          ))

          if (performance::check_convergence(mod) & !performance::check_singularity(mod)){
            conv <- TRUE
            return(mod)
          }
          i = i + 1
      }

      if(!conv) stop("No convergence with any optimizer:" , ...)

      mod
    }

    imp_list <- complete(data_imp, "all")

    m <- lapply(imp_list, function(x){

      fit_model(formula = eval(parse(text = formula)), data = x)

    })


    m_pooled <- pool(m)
    pool_summary <- data.table(summary(m_pooled))

    crit.val <- qnorm(1 - 0.05 / 2)

    pool_summary$lower <-
      print_num(with(pool_summary, estimate - crit.val * std.error))
    pool_summary$upper <-
      print_num(with(pool_summary, estimate + crit.val * std.error))

    tabby <- data.table(pool_summary)[,
                                      list(
                                        term = term,
                                        "b [95\\% CI]" = with(
                                          pool_summary,
                                          glue::glue("{print_num(estimate)} [{lower}, {upper}]")
                                        ),
                                        se = print_num(std.error),
                                        t = print_num(statistic),
                                        p = print_p(p.value)

                                      )]

    if (table_only)
      return(tabby)

    list(model = m,
         pooled_model = pool(m),
         table = tabby)

  }

#' get_effects
#'
#' Plot an effects display for a RQ1 model
#' @param model a list delivered by model_builder_RQ1
#' @param terms character. variables to plot
#' @param engine ggeffects function
#' @param plot if TRUE plot is returned
#' @param ... additional arguments passed to the ggeffect engine
#' @example model = rq1_example_model, terms = c("pa_intensity", "pa_volume")

get_effects <- function(model, terms, engine = ggeffects::ggpredict, plot = FALSE,
                            ...){

  effects <-
    lapply(seq_len(length(model$model)), function(i) {
      engine(model$model[[i]], terms = attr(model, "terms"), ...)
    }) |>
    ggeffects::pool_predictions()

  if (!plot)
    return(effects)
  effects |>
    plot() +
    figure_theme()

}
