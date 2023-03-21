#' produce_purdy_pictures
#' @param model_list list of RQ1 models
#' @example  model_list <- model_list_by_daylight
#' @details I check the proporiton of models that converged. If less then 75% of models converged
#' then I overlay the message "DID NOT CONVERGE" providing the percent of models which did not converge

produce_purdy_pictures <- function(model_list, paste_facet_labels = "", add_filename = ""){

  dat_list <- lapply(seq_len(length(model_list)), function(i){

    m <- model_list[[i]]
    terms <- attr(m, "terms")
    conv <- attr(m$pooled_model, "conv")
    conv_print <- paste0( papaja::print_num((1 -conv) * 100), "%")

    dt <- data.table(get_effects(m, terms = terms))
    dt$x_name = terms[1]
    dt$group_name = terms[2]
    dt$outcome = gsub(" .*","",names(model_list)[[i]])
    dt$RQ <- attr(m, "RQ")
    dt$moderator <- attr(m, "moderator")
    dt$conv_p <- conv
    if(conv < .75){
      # dt[,c("predicted","conf.low","conf.high")] <- NA
      dt$message <- as.character(glue::glue("DID NOT CONVERGE ({conv_print})"))
    }else{
      dt$message <- " "
    }
    attr(dt, "conv") <- conv
    dt

  })

  plot_dat <- data.table::rbindlist(dat_list)
  plot_dat$x_name <- gsub("scale_","",plot_dat$x_name)
  plot_dat$outcome <- gsub("scale_", "", plot_dat$outcome) |>
    gsub("_", " ", x = _) |>
    stringr::str_to_title() |>
    paste("(z)")
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
              hjust = .5, vjust = .5, size = 3, color = "black", family = "serif",
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
