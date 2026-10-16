
{{< pagebreak >}}

## _::moderator_formal::_

```{r}
tar_load(model_tables__::model_name::_)
tar_load(purdy_pictures__::model_name::_)
```

### The association between physical activity volume and sleep by _::moderator_formal::_

We estimated the associations between physical activity and sleep (RQ1) using mixed-effects models.
We estimated the association between physical activity volume and sleep by _::moderator_formal::_, and the results are presented in Table [-@tbl-sleep-outcomes-by-_::moderator_fig::_] and Figure [-@fig-sleep-by-volume-by-_::moderator_fig::_]

::: {.landscape}
```{r}
#| label: tbl-sleep-outcomes-by-_::moderator_fig::_
#| tbl-cap: !expr model_tables__::model_name::_$sleep$caption
d <- dplyr::bind_rows(model_tables__::model_name::_$sleep$data, .id = "group")
tinytable::tt(
  subset(d, select = -group),
  notes = model_tables__::model_name::_$sleep$note
) |>
  tinytable::group_tt(i = d$group)
```
:::

```{r}
#| label: fig-sleep-by-volume-by-_::moderator_fig::_
#| fig-cap: "Sleep metrics on Physical activity volume by _::moderator_formal::_"
knitr::include_graphics(here::here(purdy_pictures__::model_name::_$predictor_pa_volume), error = FALSE)
```

### The association between physical activity intensity and sleep by _::moderator_formal::_

We estimated how physical activity intensity is associated with sleep across _::moderator_formal::_.
We present the results in Table [-@tbl-sleep-outcomes-by-_::moderator_fig::_] and Figure [-@fig-sleep-by-intensity-by-_::moderator_fig::_].


```{r}
#| label: fig-sleep-by-intensity-by-_::moderator_fig::_
#| fig-cap: "Sleep metrics on Physical activity intensity moderated by _::moderator_formal::_"
knitr::include_graphics(here::here(purdy_pictures__::model_name::_$predictor_pa_intensity), error = FALSE)
```

### The association between sleep duration and next-day physical activity

We estimated the association between sleep duration and next-day physical activity by _::moderator_formal::_.
Results, controlling for _::control_text::_ are presented in Table [-@tbl-pa-outcomes-by-_::moderator_fig::_] and Figure [-@fig-pa-by-sleep-duration-by-_::moderator_fig::_].


::: {.landscape}
```{r}
#| label: tbl-pa-outcomes-by-_::moderator_fig::_
#| tbl-cap: !expr model_tables__::model_name::_$physical_activity$caption
d <- dplyr::bind_rows(model_tables__::model_name::_$physical_activity$data, .id = "group")
tinytable::tt(
  subset(d, select = -group),
  notes = model_tables__::model_name::_$physical_activity$note
) |>
  tinytable::group_tt(i = d$group)
```
:::

```{r}
#| label: fig-pa-by-sleep-duration-by-_::moderator_fig::_
#| fig-cap: "Physical activity by sleep duration moderated by _::moderator_formal::_"
knitr::include_graphics(here::here(purdy_pictures__::model_name::_$predictor_sleep_duration_lag), error = FALSE)
```

### The association between sleep efficiency and next-day physical activity

We estimated the association between sleep efficiency and next-day physical activity by _::moderator_formal::_.
Results, controlling for _::control_text::_ are presented in Table [-@tbl-pa-outcomes-by-_::moderator_fig::_] and Figure [-@fig-pa-by-sleep-efficiency-by-_::moderator_fig::_].


```{r}
#| label: fig-pa-by-sleep-efficiency-by-_::moderator_fig::_
#| fig-cap: "Physical activity by sleep efficiency moderated by _::moderator_formal::_"
knitr::include_graphics(here::here(purdy_pictures__::model_name::_$predictor_sleep_efficiency_lag), error = FALSE)
```

### The association between sleep onset and next-day physical activity

We estimated the association between sleep onset and next-day physical activity by _::moderator_formal::_.
Results, controlling for _::control_text::_ are presented in Table [-@tbl-pa-outcomes-by-_::moderator_fig::_] and Figure [-@fig-pa-by-sleep-onset-by-_::moderator_fig::_].


```{r}
#| label: fig-pa-by-sleep-onset-by-_::moderator_fig::_
#| fig-cap: "Physical activity by sleep onset moderated by _::moderator_formal::_"
knitr::include_graphics(here::here(purdy_pictures__::model_name::_$predictor_sleep_onset_lag), error = FALSE)
```

### The association between sleep regularity and next-day physical activity

We estimated the association between sleep regularity and next-day physical activity by _::moderator_formal::_.
Results are presented in Table [-@tbl-pa-outcomes-by-_::moderator_fig::_] and Figure [-@fig-pa-by-sleep-regularity-by-_::moderator_fig::_].

```{r}
#| label: fig-pa-by-sleep-regularity-by-_::moderator_fig::_
#| fig-cap: "Physical activity by sleep regularity moderated by _::moderator_formal::_"
knitr::include_graphics(here::here(purdy_pictures__::model_name::_$predictor_sleep_regularity_lag), error = FALSE)
```

