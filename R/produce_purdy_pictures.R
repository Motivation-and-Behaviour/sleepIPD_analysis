#' produce_purdy_pictures
#' @param model_list model_list
#' @param paste_facet_labels character. String to paste to facet labels
#' @param add_filename character. String to add to end of filenames
#' @example  model_list <- model_list_by_age_fixedef
#' @details I check the proporiton of models that converged. If less then 75% of models converged
#' then I overlay the message "DID NOT CONVERGE" providing the percent of models which did not converge # nolint

produce_purdy_pictures <- function(
    model_list,
    paste_facet_labels = "", add_filename = "") {

  dat_list <- lapply(seq_len(length(model_list)), function(i) {
    model_list[[i]]$model_assets$effects
  })

  plot_dat <- data.table::rbindlist(dat_list) |>
    prepare_plot_data(paste_facet_labels)

  moderator <- attr(model_list[[1]], "moderator")

  # Im the moderator is age, then, retrieve granular age predictions

  if (moderator == "age") {
    tile_dat <- lapply(seq_len(length(model_list)), function(i) {
      model_list[[i]]$model_assets$pred_matrix
    }) |> rbindlist()
    tile_dat$group <- as.numeric(tile_dat$group)
    tile_dat$includes_zero <- tile_dat$conf.low <= 0 & tile_dat$conf.high >= 0
    # hide non-sig predictions
    tile_dat$predicted[tile_dat$includes_zero == TRUE] <- 0
    tile_dat$predicted[tile_dat$predicted >= 2] <- 2
    tile_dat$predicted[tile_dat$predicted <= -2] <- -2
    tile_dat <- tile_dat |> prepare_plot_data(paste_facet_labels)
  }

  # Plotting function
  require(ggplot2)

  p <- function(x_var, x_lab, rq, debug = FALSE) {
    if(debug) browser()
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
      scale_x_continuous(limits = c(-4, 4)) +
      scale_y_continuous(limits = c(-4, 4)) +
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
      gsub("\\[.*", "", unique(tile_dat$x_name)) |>
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

    filename <- "Figures/{outcome} on {x_var} by {stringr::str_to_sentence(unique(plot_dat$moderator))}{add_filename}.jpg" |> # nolint: line_length_linter.
      glue::glue()

    if(attr(model_list, "use_log")) {
      filename <- gsub("\\.jpg","_log.jpg", filename)
    }

    if (outcome == "Sleep") {
      height <- 15
    } else {
      height <- 9
    }

    ggsave(filename, plot = fig, height = height, width = width + 5, units = "cm", dpi = dpi)
  }

  # Create an input data.frame
  df <- data.frame(x = c("pa_intensity","pa_volume",
                   "sleep_duration_lag", "sleep_efficiency_lag",
                   "sleep_onset_lag", "sleep_regularity_lag"))
  # Format xlab
  df$y <- df$x |>
    gsub("_", " ", x = _ ) |>
    gsub("^(.*)\\b(.*)\\b(lag)$", "\\3 \\1\\2", x = _ ) |>
    trimws() |>
    stringr::str_to_sentence() |>
    gsub("Pa", "PA", x = _) |>
    paste("(z)")
  df$RQ <- ifelse(grepl("Lag", df$y), 3, 1)

  if(attr(model_list, "use_log")){
    row <- which(df$x == "pa_volume")
    df$x[row] <- "log_pa_volume"
    df$y[row] <- "PA volume (log)"
  }

  # Produce the plots
  for (row in seq_len(nrow(df))) {
    p(df$x[row], df$y[row], df$RQ[row])
  }

  NULL
}

prepare_plot_data <- function(plot_dat, paste_facet_labels) {
  plot_dat$x_name <- gsub("scale_", "", plot_dat$x_name)
  plot_dat$outcome <- gsub("scale_", "", plot_dat$outcome) |>
    gsub("_", " ", x = _) |>
    stringr::str_to_title() |>
    paste("(z)")
  # Swap the order of volume and intensity
  plot_dat$outcome <- gsub("Pa", "PA", plot_dat$outcome)
  plot_dat$outcome <- factor(plot_dat$outcome)
  volume_level <- grep("Volume", levels(plot_dat$outcome), value = TRUE, ignore.case = TRUE)
  plot_dat$outcome <- forcats::fct_relevel(plot_dat$outcome, volume_level, after = 0)

  levels(plot_dat$group) <- paste0(levels(plot_dat$group), paste_facet_labels)
  plot_dat$x_name <- gsub("\\[.*", "", plot_dat$x_name)
  plot_dat
}
