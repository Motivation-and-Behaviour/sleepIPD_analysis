moderator_level_order <- list(
  weekday = c(
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday",
    "Sunday"
  ),
  season = c("spring", "summer", "autumn", "winter")
)

#' produce_purdy_pictures
#' @param model_list model_list
#' @example  model_list <- model_list_by_age
#' @details I check the proporiton of models that converged. If less then 75% of
#' models converged then I overlay the message "DID NOT CONVERGE" providing the
#' percent of models which did not converge
#' @return PDF paths by predictor; the JPG copies are in attr "jpg"

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
    prepare_plot_data(
      paste_facet_labels,
      scale_descriptives = attr(model_list, "scale_descriptives")
    )

  # Calendar order for the facets. The factors stay alphabetical in the data,
  # because their first level is the models' reference category.
  if (moderator %in% names(moderator_level_order)) {
    wanted <- moderator_level_order[[moderator]]
    if (!setequal(levels(plot_dat$group), wanted)) {
      stop(
        "Expected ", moderator, " levels ", paste(wanted, collapse = ", "),
        " but the predictions have ", paste(levels(plot_dat$group), collapse = ", "),
        call. = FALSE
      )
    }
    plot_dat$group <- factor(plot_dat$group, levels = wanted)
  }

  # Im the moderator is age, then, retrieve granular age predictions

  if (moderator == "age") {
    tile_dat <- lapply(seq_len(length(model_list)), function(i) {
      model_list[[i]]$model_assets$pred_matrix
    }) |>
      rbindlist()
    # The ages themselves; as.numeric() alone returns the factor's level
    # positions, which were 1.75 years low while every model shared one grid
    tile_dat$group <- as.numeric(as.character(tile_dat$group))
    # hide non-sig predictions

    tile_dat <- tile_dat |>
      prepare_plot_data(
        paste_facet_labels,
        scale_descriptives = attr(model_list, "scale_descriptives"),
        debug = FALSE
      )

    # Each model's grid spans the rows it analyses, so grids differ between
    # panels and geom_tile()'s default size (the smallest spacing anywhere in
    # the layer) would leave gaps. Tiles run halfway to each neighbour instead.
    tile_dat[,
      c("xmin", "xmax") := tile_edges(x),
      by = .(outcome, x_name, RQ, group)
    ]
    tile_dat[,
      c("ymin", "ymax") := tile_edges(group),
      by = .(outcome, x_name, RQ, x)
    ]

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
        x = x,
        y = predicted,
        ymin = conf.low,
        ymax = conf.high,
        group = group,
        fill = group
      )
    ) +
      geom_line() +
      geom_ribbon(alpha = .5) +
      facet_grid(rows = vars(outcome), cols = vars(group), switch = "y") +
      labs(
        x = x_lab,
        fill = stringr::str_to_sentence(unique(plot_dat$moderator))
      ) +
      figure_theme() +
      theme(legend.position = "none") +
      coord_cartesian(xlim = c(-2, 2), ylim = c(-2, 2)) +
      geom_text(
        data = conv_message_dat,
        aes(x = 0, y = 0, label = message, ),
        hjust = .5,
        vjust = .5,
        size = 2,
        color = "black",
        family = "serif",
        fontface = "bold",
        show.legend = FALSE,
        inherit.aes = FALSE
      )

    # If moderator is equal to age, append a heat map to the main figure
    if (moderator == "age") {
      require(cowplot)

      tdat <- tile_dat[x_name == x_var & RQ == rq]

      tdat$facet_label <- "Age continuous"
      fig2 <-
        ggplot(
          tdat,
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = predicted)
        ) +
        facet_grid(rows = vars(outcome), cols = vars(facet_label)) +
        scale_fill_gradient2(
          low = "#b20000",
          mid = "white",
          high = "#0000b2",
          midpoint = 0,
          limits = c(-2, 2),
          labels = c("-2 <", -1, 0, 1, "2 +"),
          # Draw the colour bar as shapes, so the PDF has no bitmap in it
          guide = guide_colourbar(raster = FALSE)
        ) +
        scale_y_continuous(n.breaks = 5) +
        coord_cartesian(xlim = c(-2, 2)) +
        labs(y = "Age", x = x_lab, fill = "predicted") +
        geom_rect() +
        figure_theme()
      fig <- cowplot::plot_grid(fig, fig2, rel_widths = c(1, .72))
      width <- 3.25 * length(unique(pdat$group)) + 4.5
      dpi <- 600
    } else {
      width <- 3.25 * length(unique(pdat$group))
      dpi <- 300
    }

    outcome <- unique(gsub(" .*", "", pdat$outcome))
    if (length(outcome) > 1) {
      stop("Outcome length is greater than 1")
    }

    if (moderator == "age" && add_filename == "_nolog") {
      out_folder <- "main"
    } else {
      out_folder <- "supplementary"
    }
    out_folder

    filename <- glue::glue(
      "Figures/{out_folder}/{outcome} on {x_var} by ",
      "{stringr::str_to_sentence(unique(plot_dat$moderator))}{add_filename}"
    )

    if (outcome == "Sleep") {
      height <- 15
    } else {
      height <- 9
    }

    # Vector PDF with editable text: the Lancet upload format and the appendix's
    pdf_file <- paste0(filename, ".pdf")
    ggsave(
      pdf_file,
      plot = fig,
      height = height,
      width = width + 5,
      units = "cm",
      device = cairo_pdf
    )

    # A JPG copy as well, because Word can't show PDF figures
    jpg_file <- paste0(filename, ".jpg")
    ggsave(
      jpg_file,
      plot = fig,
      height = height,
      width = width + 5,
      units = "cm",
      dpi = dpi
    )

    list(pdf = pdf_file, jpg = jpg_file)
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

  figs <- lapply(out, `[[`, "pdf")
  attr(figs, "jpg") <- unlist(lapply(out, `[[`, "jpg"))
  figs
}

#' prepare_plot_data

prepare_plot_data <- function(
  plot_dat,
  paste_facet_labels,
  scale_descriptives,
  debug = FALSE
) {
  if (debug) {
    browser()
  }
  is_scale <- grepl("scale_", plot_dat$outcome)
  plot_dat$outcome[is_scale] <-
    gsub("scale_", "", plot_dat$outcome[is_scale]) |>
    gsub("_", " ", x = _) |>
    stringr::str_to_title() |>
    paste("(z)")

  is_log_outcome <- grepl("log_", plot_dat$outcome)
  is_log_predictor <- grepl("log_", plot_dat$x_name)

  log_outcome_vars <- unique(plot_dat$outcome[is_log_outcome])
  log_predictor_vars <- unique(plot_dat$x_name[is_log_predictor])

  if (length(log_outcome_vars) > 0) {
    # Rescale log outcomes variables
    for (i in seq_along(log_outcome_vars)) {
      var_i <- log_outcome_vars[i]
      to_transf <- is_log_outcome & plot_dat$outcome == var_i
      dt <- scale_descriptives[var == gsub("log_", "", var_i)]

      # exponentiate outcome variables
      plot_dat[to_transf, "predicted"] <-
        ln_to_z(plot_dat[to_transf, "predicted"], mean = dt$mean, sd = dt$sd)
      plot_dat[to_transf, "conf.low"] <-
        ln_to_z(plot_dat[to_transf, "conf.low"], mean = dt$mean, sd = dt$sd)
      plot_dat[to_transf, "conf.high"] <-
        ln_to_z(plot_dat[to_transf, "conf.high"], mean = dt$mean, sd = dt$sd)
      # reset to_transf
      to_transf <- NULL
      dt <- NULL
    }
  }

  if (length(log_predictor_vars) > 0) {
    # Rescale log outcomes variables
    for (i in seq_along(log_predictor_vars)) {
      var_i <- log_predictor_vars[i]
      to_transf <- is_log_predictor & plot_dat$x_name == var_i
      dt <-
        scale_descriptives[var == gsub("log_", "", gsub("\\[.*", "", var_i))]

      plot_dat[to_transf, "x"] <-
        ln_to_z(plot_dat[to_transf, "x"], mean = dt$mean, sd = dt$sd)
      # reset to_transf
      to_transf <- NULL
      dt <- NULL
    }
  }

  plot_dat$outcome[is_log_outcome] <-
    gsub("log_", "", plot_dat$outcome[is_log_outcome]) |>
    gsub("_", " ", x = _) |>
    stringr::str_to_title() |>
    paste("(z*)")

  # Swap the order of volume and intensity
  plot_dat$outcome <- gsub("Pa", "PA", plot_dat$outcome)
  plot_dat$outcome <- factor(plot_dat$outcome)
  volume_level <- grep(
    "Volume",
    levels(plot_dat$outcome),
    value = TRUE,
    ignore.case = TRUE
  )
  plot_dat$outcome <- forcats::fct_relevel(
    plot_dat$outcome,
    volume_level,
    after = 0
  )

  levels(plot_dat$group) <- paste0(levels(plot_dat$group), paste_facet_labels)
  plot_dat$x_name <- gsub("\\[.*", "", plot_dat$x_name)

  plot_dat
}

# Edges halfway between neighbouring grid values; the end tiles extend by half
# the spacing to their one neighbour
tile_edges <- function(v) {
  u <- sort(unique(v))
  if (length(u) == 1) {
    return(list(v - 0.5, v + 0.5))
  }
  mid <- (u[-1] + u[-length(u)]) / 2
  lo <- c(2 * u[1] - mid[1], mid)
  hi <- c(mid, 2 * u[length(u)] - mid[length(mid)])
  i <- match(v, u)
  list(lo[i], hi[i])
}

ln_to_z <- function(x, mean, sd) {
  (exp(x) - mean) / sd
}
