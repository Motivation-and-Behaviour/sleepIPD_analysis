#' produce_purdy_pictures
#' @param model_list list of RQ1 models

produce_purdy_pictures <- function(model_list){

  dat_list <- lapply(seq_len(length(model_list)), function(i){

    m <- model_list[[i]]
    terms <- attr(m, "terms")

    dt <- data.table(get_effects(m, terms = terms))
    dt$x_name = terms[1]
    dt$group_name = terms[2]
    dt$outcome = gsub(" .*","",names(model_list)[[i]])
    dt$RQ <- attr(m, "RQ")
    dt

  })

  plot_dat <- data.table::rbindlist(dat_list)
  plot_dat$x_name <- gsub("scale_","",plot_dat$x_name)
  plot_dat$outcome <- gsub("scale_", "", plot_dat$outcome) |>
    gsub("_", " ", x = _) |>
    stringr::str_to_title() |>
    paste("(z)")
  plot_dat$group <- paste(plot_dat$group, "years")
  plot_dat$x_name <- gsub("\\[.*","", plot_dat$x_name)


  require(ggplot2)

  p <- function(x_var, x_lab, rq){
    pdat <- plot_dat[x_name == x_var & RQ == rq]

  fig <- ggplot(pdat, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high, group = group, fill = group)) +
    geom_line() +
    geom_ribbon(alpha = .5) +
    facet_grid(rows = vars(outcome), cols = vars(group)) +
      labs(x = x_lab,
           fill = "Age") +
      figure_theme() +
    theme(legend.position = "none")

    outcome <- unique(gsub(" .*","", pdat$outcome))
    if(length(outcome) > 1) stop("Outcome length is greater than 1")

    filename <- "Figures/{outcome} on {x_var}.jpg" |>
      glue::glue()

    if(outcome == "Sleep"){
      height = 15
    } else{
      height = 9
    }

    ggsave(filename, plot = fig, height = height, width = 18, units = "cm", dpi = 300)

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
