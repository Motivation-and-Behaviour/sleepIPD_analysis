#' produce_purdy_pictures
#' @param model_list model_list
#' @example  model_list <- model_list_by_age
#' @details I check the proporiton of models that converged. If less then 75% of models converged
#' then I overlay the message "DID NOT CONVERGE" providing the percent of models which did not converge

produce_purdy_pictures <- function(model_list, ...) {
  dat_list <- lapply(seq_len(length(model_list)), function(i) {
    model_list[[i]]$model_assets$effects
  })

  moderator <- attr(model_list[[1]], "moderator")

  add_filename <- attr(model_list, "filename_suffix")

  # Additional labels
  f_df <- data.frame(
    moderator = c("age", "daylight_hours"),
    facet_label = c(" years", " hours")
  )

  if (moderator %in% f_df$moderator) {
    paste_facet_labels <- f_df$facet_label[f_df$moderator == moderator]
  } else {
    paste_facet_labels <- ""
  }

  plot_dat <- data.table::rbindlist(dat_list) |>
    prepare_plot_data(paste_facet_labels, scale_descriptives = attr(model_list, "scale_descriptives"))

  # Im the moderator is age, then, retrieve granular age predictions

  if (moderator == "age") {
    tile_dat <- lapply(seq_len(length(model_list)), function(i) {
      model_list[[i]]$model_assets$pred_matrix
    }) |> rbindlist()
    tile_dat$group <- as.numeric(tile_dat$group)
    # hide non-sig predictions

    tile_dat <- tile_dat |>
      prepare_plot_data(paste_facet_labels,
                        scale_descriptives = attr(model_list, "scale_descriptives"),
                        debug = TRUE)

    tile_dat$includes_zero <- tile_dat$conf.low <= 0 & tile_dat$conf.high >= 0
    tile_dat$predicted[tile_dat$includes_zero == TRUE] <- 0
    tile_dat$predicted[tile_dat$predicted >= 2] <- 2
    tile_dat$predicted[tile_dat$predicted <= -2] <- -2
  }

  # Plotting function
  require(ggplot2)

  p <- function(x_var, x_lab, rq) {

    pdat <- plot_dat[x_name == x_var & RQ == rq]

    conv_message_dat <- unique(pdat[, c("outcome", "group", "message")])
    fig <- ggplot(
      pdat,
      aes(
        x = x, y = predicted,
        ymin = conf.low, ymax = conf.high,
        group = group, fill = group
      )
    ) +
      geom_line() +
      geom_ribbon(alpha = .5) +
      facet_grid(rows = vars(outcome), cols = vars(group)) +
      labs(
        x = x_lab,
        fill = stringr::str_to_sentence(unique(plot_dat$moderator))
      ) +
      figure_theme() +
      theme(legend.position = "none") +
      scale_x_continuous(limits = c(-2, 2)) +
      scale_y_continuous(limits = c(-2, 2)) +
      geom_text(
        data = conv_message_dat, aes(x = 0, y = 0, label = message, ),
        hjust = .5, vjust = .5, size = 2, color = "black", family = "serif",
        fontface = "bold",
        show.legend = FALSE, inherit.aes = FALSE
      )

    # If moderator is equal to age, append a heat map to the main figure
    if (moderator == "age") {
      require(cowplot)

      tdat <- tile_dat[x_name == x_var & RQ == rq]

      predictor <- gsub("\\[.*", "", unique(tdat$x_name)) |>
        gsub("_", " ", x = _) |>
        stringr::str_to_sentence() |>
        gsub("Pa", "PA", x = _)

      fig <- fig + theme(strip.text.y = element_blank())
      tdat$facet_label <- "Age continuous"
      fig2 <-
        ggplot(tdat, aes(x = x, y = group, fill = predicted)) +
        facet_grid(rows = vars(outcome), cols = vars(facet_label)) +
        scale_fill_gradient2(
          low = "#b20000",
          mid = "white",
          high = "#0000b2",
          midpoint = 0,
          limits = c(-2, 2),
          labels = c("-2 <", -1, 0, 1, "2 +")
        ) +
        scale_y_continuous(n.breaks = 5) +
        scale_x_continuous(limits = c(-2,2)) +
        labs(y = "Age", x = predictor, fill = "predicted") +
        geom_tile() +
        figure_theme()
      fig <- cowplot::plot_grid(fig, fig2, rel_widths = c(1, .72))
      width <- 3.25 * length(unique(pdat$group)) + 4.5
      dpi <- 600
    } else {
      width <- 3.25 * length(unique(pdat$group))
      dpi <- 300
    }

    outcome <- unique(gsub(" .*", "", pdat$outcome))
    if (length(outcome) > 1) stop("Outcome length is greater than 1")

    filename <- "Figures/{outcome} on {x_var} by {stringr::str_to_sentence(unique(plot_dat$moderator))}{add_filename}.jpg" |>
      glue::glue()

    if (outcome == "Sleep") {
      height <- 15
    } else {
      height <- 9
    }

    ggsave(filename, plot = fig, height = height, width = width + 5, units = "cm", dpi = dpi)
  }

  vars <- attr(model_list, "vars")

  df <- data.frame(x = c(vars$pa_vars, paste0(vars$sleep_vars, "_lag")))
  # Format xlab
  df$y <- df$x |>
    gsub("_", " ", x = _) |>
    gsub("^(.*)\\b(.*)\\b(lag)$", "\\3 \\1\\2", x = _) |>
    trimws()

  is_scale <- grepl("scale", df$y)
  df$y[is_scale] <- gsub("scale ", "", df$y[is_scale])
  df$y[is_scale] <- paste(df$y[is_scale], "(z)")

  is_log <- grepl("log", df$y)
  df$y[is_log] <- gsub("log ", "", df$y[is_log])
  df$y[is_log] <- paste(df$y[is_log], "(z*)")

  df$y <- stringr::str_to_sentence(df$y) |>
    gsub("Pa|pa\\b", "PA", x = _)

  df$RQ <- ifelse(grepl("Lag", df$y), 3, 1)

  # produce the plots
  out <- lapply(seq_len(nrow(df)), function(row) {
    p(df$x[row], df$y[row], df$RQ[row])
  })

  names(out) <- paste0("predictor_", gsub("(scale_|log_)", "", df$x))
  out
}

#' prepare_plot_data

prepare_plot_data <- function(plot_dat, paste_facet_labels,
                              scale_descriptives, debug = FALSE) {
  if(debug) browser()
  is_scale <- grepl("scale_", plot_dat$outcome)
  plot_dat$outcome[is_scale] <- gsub("scale_", "", plot_dat$outcome[is_scale])|>
    gsub("_", " ", x = _) |>
    stringr::str_to_title() |>
    paste("(z)")

  is_log_outcome <- grepl("log_", plot_dat$outcome)
  is_log_predictor <- grepl("log_", plot_dat$x_name)

  log_outcome_vars <- unique(plot_dat$outcome[is_log_outcome])
  log_predictor_vars <- unique(plot_dat$x_name[is_log_predictor])

  if(length(log_outcome_vars) > 0){
  # Rescale log outcomes variables
    for(i in seq_along(log_outcome_vars)){
      var_i <- log_outcome_vars[i]
      to_transf <- is_log_outcome & plot_dat$outcome == var_i
      dt <- scale_descriptives[var == gsub("log_","",var_i)]

      # exponentiate outcome variables
      plot_dat[to_transf, "predicted"] <-
        ln_to_z(plot_dat[to_transf, "predicted"],
                mean = dt$mean, sd = dt$sd)
      plot_dat[to_transf, "conf.low"] <-
        ln_to_z(plot_dat[to_transf, "conf.low"],
                mean = dt$mean, sd = dt$sd)
      plot_dat[to_transf, "conf.high"] <-
        ln_to_z(plot_dat[to_transf, "conf.high"],
                mean = dt$mean, sd = dt$sd)
      # reset to_transf
      to_transf <- NULL
      dt <- NULL
    }

  }

  if(length(log_predictor_vars) > 0){
    # Rescale log outcomes variables
    for(i in seq_along(log_predictor_vars)){
      var_i <- log_predictor_vars[i]
      to_transf <- is_log_predictor & plot_dat$x_name == var_i
      dt <- scale_descriptives[var == gsub("log_","",gsub("\\[.*","",var_i))]

      plot_dat[to_transf, "x"] <-
        ln_to_z(plot_dat[to_transf, "x"],
                mean = dt$mean, sd = dt$sd)
      # reset to_transf
      to_transf <- NULL
      dt <- NULL
    }

  }


plot_dat$outcome[is_log_outcome] <- gsub("log_", "", plot_dat$outcome[is_log_outcome]) |>
  gsub("_", " ", x = _) |>
  stringr::str_to_title() |>
  paste("(z*)")

  # Swap the order of volume and intensity
  plot_dat$outcome <- gsub("Pa", "PA", plot_dat$outcome)
  plot_dat$outcome <- factor(plot_dat$outcome)
  volume_level <- grep("Volume", levels(plot_dat$outcome), value = TRUE, ignore.case = TRUE)
  plot_dat$outcome <- forcats::fct_relevel(plot_dat$outcome, volume_level, after = 0)

  levels(plot_dat$group) <- paste0(levels(plot_dat$group), paste_facet_labels)
  plot_dat$x_name <- gsub("\\[.*", "", plot_dat$x_name)

  plot_dat
}

ln_to_z <- function(x, mean, sd){
  (exp(x) - mean) / sd
}

