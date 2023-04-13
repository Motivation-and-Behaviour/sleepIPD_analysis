
\clearpage

## _::moderator_formal::_

```{r}
tar_load(model_tables__::model_name::_)
tar_load(purdy_pictures__::model_name::_)
```

### The effects of physical activity volume on sleep by _::moderator_formal::_

We estimated the effects of physical activity on sleep (RQ1) using mixed-effects models.
We estimated the effect of physical activity volume on sleep by _::moderator_formal::_, and the results are presented in Table \@ref(tab:sleep-outcomes-by-_::moderator_fig::_) and Figure \@ref(fig:sleep-by-volume-fig-by-_::moderator_fig::_)

```{r sleep-outcomes-by-_::moderator_fig::_}
papaja::apa_table(
  model_tables__::model_name::_$sleep$data,
  caption = model_tables__::model_name::_$sleep$caption,
  note = model_tables__::model_name::_$sleep$note,
  col_spanners = model_tables__::model_name::_$sleep$col_spanners,
  escape = FALSE,
  landscape = TRUE
)
```

```{r sleep-by-volume-fig-by-_::moderator_fig::_, fig.cap = "Sleep metrics on Physical activity volume by _::moderator_formal::_"}
knitr::include_graphics(here::here(purdy_pictures__::model_name::_$predictor_pa_volume), error = FALSE)
```

### The effects of physical activity intensity on sleep by _::moderator_formal::_

We estimated how physical activity intensity affects sleep across _::moderator_formal::_.
We present the results in Table \@ref(tab:sleep-outcomes-by-_::moderator_fig::_) and Figure \@ref(fig:sleep-by-intensity-fig-by-_::moderator_fig::_).


```{r sleep-by-intensity-fig-by-_::moderator_fig::_, fig.cap = "Sleep metrics on Physical activity intensity moderated by _::moderator_formal::_"}
knitr::include_graphics(here::here(purdy_pictures__::model_name::_$predictor_pa_intensity), error = FALSE)
```

### The effects of sleep duration on physical activity

We estimated the effect of sleep duration on physical activity by _::moderator_formal::_.
Results, controlling for sex, SES, and BMI are presented in Table \@ref(tab:pa-outcomes-by-_::moderator_fig::_) and Figure \@ref(fig:PA-by-sleep-duration-fig-by-_::moderator_fig::_).


```{r pa-outcomes-by-_::moderator_fig::_}
papaja::apa_table(
  model_tables__::model_name::_$physical_activity$data,
  caption = model_tables__::model_name::_$physical_activity$caption,
  note = model_tables__::model_name::_$physical_activity$note,
  col_spanners = model_tables__::model_name::_$physical_activity$col_spanners,
  escape = FALSE,
  landscape = TRUE
)
```

```{r PA-by-sleep-duration-fig-by-_::moderator_fig::_, fig.cap = "Physical activity by sleep duration moderated by _::moderator_formal::_"}
knitr::include_graphics(here::here(purdy_pictures__::model_name::_$predictor_sleep_duration_lag), error = FALSE)
```

### The effects of sleep efficiency on physical activity

We estimated the effect of sleep efficiency on physical activity by _::moderator_formal::_.
Results, controlling for sex, SES, and BMI are presented in Table \@ref(tab:pa-outcomes-by-_::moderator_fig::_) and Figure \@ref(fig:PA-by-sleep-efficiency-fig-by-_::moderator_fig::_).


```{r PA-by-sleep-efficiency-fig-by-_::moderator_fig::_, fig.cap = "Physical activity by sleep efficiency moderated by _::moderator_formal::_"}
knitr::include_graphics(here::here(purdy_pictures__::model_name::_$predictor_sleep_efficiency_lag), error = FALSE)
```

### The effects of sleep onset on physical activity

We estimated the effect of sleep onset on physical activity by _::moderator_formal::_.
Results, controlling for sex, SES, and BMI are presented in Table \@ref(tab:pa-outcomes-by-_::moderator_fig::_) and Figure \@ref(fig:PA-by-sleep-onset-fig-by-_::moderator_fig::_).


```{r PA-by-sleep-onset-fig-by-_::moderator_fig::_, fig.cap = "Physical activity by sleep onset moderated by _::moderator_formal::_"}
knitr::include_graphics(here::here(purdy_pictures__::model_name::_$predictor_sleep_onset_lag), error = FALSE)
```

### The effects of sleep regularity on physical activity

We estimated the effect of sleep regularity on physical activity by _::moderator_formal::_.
Results are presented in Table \@ref(tab:pa-outcomes-by-_::moderator_fig::_) and Figure \@ref(fig:PA-by-sleep-regularity-fig-by-_::moderator_fig::_).

```{r PA-by-sleep-regularity-fig-by-_::moderator_fig::_, fig.cap = "Physical activity by sleep regularity moderated by _::moderator_formal::_"}
knitr::include_graphics(here::here(purdy_pictures__::model_name::_$predictor_sleep_regularity_lag), error = FALSE)
```

