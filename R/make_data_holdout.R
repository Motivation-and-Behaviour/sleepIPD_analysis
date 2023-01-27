#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param data_clean
#' @return
#' @author tarensanders
#' @export
make_data_holdout <- function(data_clean) {
  rand_ids <-
    data_clean %>%
    filter(eligible == TRUE) %>%
    distinct(studyid, filename, age) %>%
    mutate(age_cat = cut(age, 15)) %>%
    group_by(age_cat) %>%
    slice_sample(prop = 0.1) %>%
    ungroup() %>%
    mutate(keep = TRUE) %>%
    select(-c(age, age_cat))

  data_clean %>%
    left_join(rand_ids, by = c("studyid", "filename")) %>%
    filter(keep) %>%
    select(-keep)
}
