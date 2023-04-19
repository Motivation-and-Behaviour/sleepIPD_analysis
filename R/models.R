# Model parameters
dflt_pa <- c("scale_pa_volume", "scale_pa_intensity")
log_pa <- c("log_pa_volume", "scale_pa_intensity")
dflt_sleep <- c(
  "scale_sleep_duration", "scale_sleep_efficiency", "scale_sleep_onset",
  "scale_sleep_regularity"
)
dflt_ranef <- "(1|studyid) + (1|participant_id)"
fixedef <- "(1|participant_id)"
dflt_con <- c("ses", "age", "sex", "bmi")

# nolint start styler: off
models_df <- dplyr::tribble(
  ~model_name,          ~moderator,        ~mod_term,        ~mod_formal,        ~pa_vars, ~sleep_vars, ~ranef,     ~cont_vars,
  "by_age",             "age",             "11, 18, 35, 65", "age",              dflt_pa,  dflt_sleep,  dflt_ranef, dflt_con,
  "by_age_fixedef",     "age",             "11, 18, 35, 65", "age",              dflt_pa,  dflt_sleep,  fixedef,    c(dflt_con, "studyid"),
  "by_age_log",         "age",             "11, 18, 35, 65", "age",              log_pa,   dflt_sleep,  dflt_ranef, dflt_con,
  "by_bmi",             "bmi",             "18, 22, 25, 30", "BMI",              dflt_pa,  dflt_sleep,  dflt_ranef, dflt_con,
  "by_ses",             "ses",             "all",            "SES",              dflt_pa,  dflt_sleep,  dflt_ranef, dflt_con,
  "by_sex",             "sex",             "all",            "sex",              dflt_pa,  dflt_sleep,  dflt_ranef, dflt_con,
  "by_weekday",         "weekday",         "all",            "weekday",          dflt_pa,  dflt_sleep,  dflt_ranef, dflt_con,
  "by_season",          "season",          "all",            "season",           dflt_pa,  dflt_sleep,  dflt_ranef, dflt_con,
  "by_region",          "region",          "all",            "region",           dflt_pa,  dflt_sleep,  dflt_ranef, dflt_con,
  "by_daylight",        "daylight_hours",  "8, 10, 12, 14",  "daylight hours",   dflt_pa,  dflt_sleep,  dflt_ranef, dflt_con,
  "by_wear_location",   "acc_wear_loc",    "all",            "wear location",    dflt_pa,  dflt_sleep,  dflt_ranef, dflt_con,
  "by_pa_mostactivehr", "pa_mostactivehr", "5, 9, 14, 19",   "most active hour", dflt_pa,  dflt_sleep,  dflt_ranef, dflt_con
)
# nolint end styler: on
