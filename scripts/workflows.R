pacman::p_load(
  tidymodels,
  modeltime,
  tsibble
)

workflow_arimax <- function(ts_data) {
  recipe_arimax <- recipe(PUTs ~ ., data = ts_data) |> 
    step_corr(all_numeric_predictors(), threshold = 0.9) |> 
    step_naomit(all_predictors())
  
  workflow() |> 
    add_model(arima_reg() |> set_engine("auto_arima")) |>
    add_recipe(recipe_arimax)
}

train_arimax <- function(ts_data, split_prop = 0.9, fit_data = NULL) {
  splits <- initial_time_split(ts_data, prop = split_prop)
  wf_arimax <- workflow_arimax(training(splits))
  if (is.null(fit_data)) {
    fit_data <- training(splits)
  }
  fit_arimax <- wf_arimax |> fit(data = fit_data)
  list(
    fit = fit_arimax,
    splits = splits,
    workflow = wf_arimax
  )
}

workflow_glmnet <- function(ts_data) {
  recipe_glmnet <- recipe(PUTs ~., data = ts_data) |>
    update_role(.date_var, new_role = "index")|>
    step_zv(all_predictors()) |> 
    step_dummy(all_nominal_predictors()) |> 
    step_normalize(all_numeric_predictors())|> 
    step_naomit(all_predictors())
  
  modelo <- linear_reg(
    penalty = 0.0001,   
    mixture = 0 
  ) %>%
    set_engine("glmnet") %>%
    set_mode("regression")
  
  workflow() |>
    add_recipe(recipe_glmnet) |>
    add_model(modelo)
}

train_glmnet <- function(ts_data, split_prop = 0.9, fit_data = NULL) {
  splits <- initial_time_split(ts_data, prop = split_prop)
  wf_glmnet <- workflow_glmnet(training(splits))
  if (is.null(fit_data)) {
    fit_data <- training(splits)
  }
  fit_glmnet <- wf_glmnet |> fit(data = fit_data)
  list(
    fit = fit_glmnet,
    splits = splits,
    workflow = wf_glmnet
  )
}
