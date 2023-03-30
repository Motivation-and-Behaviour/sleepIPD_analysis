#' produce_purdy_pictures
#' @param model_list model_list
#' @param paste_facet_labels character. String to paste to facet labels
#' @param add_filename character. String to add to end of filenames
#' @example  model_list <- model_list_by_daylight
#' @details I check the proporiton of models that converged. If less then 75% of models converged
#' then I overlay the message "DID NOT CONVERGE" providing the percent of models which did not converge

produce_purdy_pictures <- function(model_list, paste_facet_labels = "", add_filename = ""){
  dat_list <- lapply(seq_len(length(model_list)), function(i){
    model_list[[i]]$model_assets$effects
  })

  plot_dat <- data.table::rbindlist(dat_list)
  plot_dat$x_name <- gsub("scale_","",plot_dat$x_name)
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
  plot_dat$x_name <- gsub("\\[.*","", plot_dat$x_name)


  require(ggplot2)

  p <- function(x_var, x_lab, rq){
    pdat <- plot_dat[x_name == x_var & RQ == rq]

  conv_message_dat <- unique(pdat[, c("outcome", "group", "message")])

  fig <- ggplot(pdat, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, group = group, fill = group)) +
    geom_line() +
    geom_ribbon(alpha = .5) +
    facet_grid(rows = vars(outcome), cols = vars(group)) +
      labs(x = x_lab,
           fill = stringr::str_to_sentence(unique(plot_dat$moderator))) +
      figure_theme() +
    theme(legend.position = "none") +
    scale_x_continuous(limits = c(-4,4)) +
    scale_y_continuous(limits = c(-4,4)) +
    geom_text(data = conv_message_dat, aes(x = 0, y = 0, label = message, ),
              hjust = .5, vjust = .5, size = 2, color = "black", family = "serif",
              fontface = "bold",
              show.legend = FALSE, inherit.aes = FALSE)

    outcome <- unique(gsub(" .*","", pdat$outcome))
    if(length(outcome) > 1) stop("Outcome length is greater than 1")

    filename <- "Figures/{outcome} on {x_var} by {stringr::str_to_sentence(unique(plot_dat$moderator))}{add_filename}.jpg" |>
      glue::glue()

    if(outcome == "Sleep"){
      height = 15
    } else{
      height = 9
    }

    ggsave(filename, plot = fig, height = height, width = 3.25 * length(unique(pdat$group)) + 5, units = "cm", dpi = 300)

  }

  # Research Question 1
  p("pa_intensity", "PA intensity (z)", rq = 1)
  p("pa_volume", "PA volume (z)", rq = 1)

  # Research Question 3
  p("sleep_duration_lag", "lag sleep duration (z)", rq = 3)
  p("sleep_efficiency_lag", "lag sleep efficiency (z)", rq = 3)
  p("sleep_onset_lag", "lag sleep onset (z)", rq = 3)
  p("sleep_regularity_lag", "lag sleep regularity (z)", rq = 3)

  NULL

}
