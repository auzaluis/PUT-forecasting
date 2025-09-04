pacman::p_load(
  tidymodels,
  modeltime,
  tsibble
)


train_arimax <- function(ts_data, split_prop = 0.9) {
  splits <- initial_time_split(ts_data, prop = split_prop)
  
  wf_arimax <- workflow() |> 
    add_model(arima_reg() |> set_engine("auto_arima")) |>
    add_recipe(
      recipe(PUTs ~ ., data = training(splits)) |> 
        step_corr(all_numeric_predictors(), threshold = 0.9) |> 
        step_naomit(all_predictors())
    )
  
  fit_arimax <- wf_arimax |> fit(data = training(splits))
  
  list(
    fit = fit_arimax,
    splits = splits
  )
}