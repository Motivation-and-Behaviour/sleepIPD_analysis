# ),
# tar_target(
#   purdy_pictures_by_daylight,
#   produce_purdy_pictures(model_list_by_daylight, paste_facet_labels = " hours")
# ),


target_factory <-
  function(moderators = c("age", "bmi"),
           data = "data_imp") {
    out <-
      lapply(moderators, function(x)
        create_analytic_targets(x, data = data))
    # Generate model_names based on data object
    model_names <- paste0("model_list_by_", moderators)
    model_names_data <- paste0(model_names,"_", data)

    # model_tables
    model_table_names <- paste0("model_tables_", moderators)
    model_table_names_data <-  paste0(model_table_names,"_", data)

    # model_figures
    model_figure_names <- paste0("purdy_pictures_by_", moderators)
    model_figure_names_data <- paste0(model_figure_names, "_", data)

    # Create a manuscript using those names
    manuscript_path <- "doc/manuscript.Rmd"
    skeleton_manuscript <- readLines(manuscript_path)

    names_to_change <- c(model_names, model_table_names, model_figure_names)
    new_names <- c(model_names_data, model_table_names_data, model_figure_names_data)

    for(i in seq_along(names_to_change)){
    skeleton_manuscript <- gsub(names_to_change[i], new_names[i], skeleton_manuscript)
    }

    manuscript_path_new <- gsub("\\.Rmd",glue::glue("_{data}.Rmd"),manuscript_path)
    write(skeleton_manuscript, manuscript_path_new)

    append(out,
           list(
             tar_target_raw(
               name = glue::glue("model_diagnostics_{data}"),
               command = parse(
                 text = glue::glue(
                   "make_model_diagnostics({paste(model_names_data, collapse = ',')})"
                 )
               ),
               deployment = "main"
             ),
             tar_render_raw(
               name = glue::glue("manuscript_{data}"),
               manuscript_path_new,
               render_arguments = quote(list(
                 output_format = c("papaja::apa6_docx",
                                   "papaja::apa6_pdf")
               ))
             )
           ))

  }

create_analytic_targets <- function(moderator = "age", data = "data_imp"){
  # Detine control vars
  control_vars <- c("ses", "age", "sex", "bmi")
  control_vars <- control_vars[!control_vars == moderator]

  # Define moderator term
  i_df <-
    data.frame(
      moderator = c("age", "bmi", "daylight_hours","pa_mostactivehr"),
      moderator_term = c("11, 18, 35, 65",
                         "18, 22, 25, 30",
                         "8, 10, 12, 14",
                         "5, 9, 14, 19")
    )

  if(moderator %in% i_df$moderator){
    moderator_term <- i_df$moderator_term[i_df$moderator == moderator]
  }else{
    moderator_term <- "all"
  }

  # facet_labels

  f_df <- data.frame(
    moderator = c("age", "daylight_hours"),
    facet_label = c(" years", " hours")
  )

  if(moderator %in% f_df$moderator) {
    facet_label <- f_df$facet_label[f_df$moderator == moderator]
  }else{
    facet_label <- ""
  }

  list(
    tar_target_raw(
      name = glue::glue("model_list_by_{moderator}_{data}"),
      command = parse(
        text = glue::glue(
          "make_model_list({data},
                      moderator = \"{moderator}\",
                      moderator_term = \"{moderator_term}\",
                      control_vars = {jput(control_vars)}
                      )"
        )
      )

    ),
    tar_target_raw(
      name = glue::glue("purdy_pictures_by_{moderator}_{data}"),
      command = parse(
        text =
          glue::glue(
            "produce_purdy_pictures(model_list_by_{moderator}_{data}, paste_facet_labels = \"{facet_label}\",
            add_filename = \"_{data}\")
# )"
          )
      )
    ),
    tar_target_raw(name = glue::glue("model_tables_{moderator}_{data}"),
               command = parse(
                 text = glue::glue(
                   "make_model_tables(model_list_by_{moderator}_{data})"
                 )
               ))
  )
}

jput <- function(x){
  x <- paste0("\"", x, "\"")
  paste0("c(", paste(x, collapse = ", "), ")")
}


