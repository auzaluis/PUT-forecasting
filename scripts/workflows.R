pacman::p_load(
  tidymodels,
  modeltime,
  tsibble,
  yardstick,
  lubridate
)

workflow_arimax <- function(ts_data) {
  recipe_arimax <- recipe(PUTs ~ ., data = ts_data) |> 
    step_rm(lag_5, lag_7) |> 
    step_naomit(all_predictors()) |> 
    step_zv(all_predictors()) |> 
    step_dummy(all_nominal_predictors()) |> 
    step_corr(all_numeric_predictors(), threshold = 0.9)
  
  workflow() |> 
    add_model(arima_reg() |> set_engine("auto_arima")) |>
    add_recipe(recipe_arimax)
}

workflow_glmnet <- function(ts_data) {
  recipe_glmnet <- recipe(PUTs ~ ., data = ts_data) |>
    update_role(.date_var, new_role = "index") |>
    step_zv(all_predictors()) |> 
    step_dummy(all_nominal_predictors()) |> 
    step_normalize(all_numeric_predictors()) |> 
    step_naomit(all_predictors())
  
  modelo <- linear_reg(
    penalty = 0.0001,   
    mixture = 1 
  ) |>
    set_engine("glmnet") |>
    set_mode("regression")
  
  workflow() |>
    add_recipe(recipe_glmnet) |>
    add_model(modelo)
}

# ⚡ Función General Rolling Origin Validation INCREMENTAL (Autogestionada por el RDS)
train_cv_models <- function(ts_data, workflow_fn, decay_factor = 0.85, old_fit_obj = NULL) {
  max_date <- max(ts_data$.date_var)
  min_date <- min(ts_data$.date_var)
  
  # 1. Definir la barrera estricta de 13 meses hacia atrás
  start_date <- max(min_date, max_date %m-% months(13))
  
  # 2. Leer el historial del RDS y detectar el punto flotante
  if (!is.null(old_fit_obj) && !is.null(old_fit_obj$cv_metrics) && nrow(old_fit_obj$cv_metrics) > 0) {
    cv_old <- old_fit_obj$cv_metrics |> arrange(cutoff_date)
    
    # 🟢 DETECCIÓN DE PUNTO FLOTANTE: Medir la distancia entre los últimos dos puntos
    if (nrow(cv_old) > 1) {
      n_rows <- nrow(cv_old)
      last_date <- cv_old$cutoff_date[n_rows]
      prev_date <- cv_old$cutoff_date[n_rows - 1]
      
      distancia_dias <- as.numeric(difftime(last_date, prev_date, units = "days"))
      
      # Si la distancia es menor a 90 días, el último punto era flotante temporal -> Lo eliminamos
      if (distancia_dias < 90) {
        cv_old <- cv_old[-n_rows, ] 
      }
    }
    
    # 3. Poda de la regla de los 13 meses
    cv_old <- cv_old |> filter(cutoff_date >= start_date)
    
    if (nrow(cv_old) > 0) {
      last_anchor <- max(cv_old$cutoff_date)
      
      # 4. Calcular nuevos cortes EXACTAMENTE a 90 días de la última ancla válida guardada
      new_anchors <- c()
      next_anchor <- last_anchor + days(90)
      
      while(next_anchor <= max_date) {
        new_anchors <- c(new_anchors, next_anchor)
        next_anchor <- next_anchor + days(90)
      }
      
      # Añadir la fecha máxima actual como el nuevo punto flotante (si no es ya un ancla exacta)
      cutoffs_to_train <- new_anchors
      if (length(cutoffs_to_train) == 0 && max_date > last_anchor) {
        cutoffs_to_train <- c(max_date)
      } else if (length(cutoffs_to_train) > 0 && max(cutoffs_to_train) < max_date) {
        cutoffs_to_train <- c(cutoffs_to_train, max_date)
      }
      
    } else {
      # Si la poda vació la tabla, reconstruimos el grid desde el inicio
      grid_all <- seq(min_date, max_date, by = "90 days")
      cutoffs_to_train <- grid_all[grid_all >= start_date]
      if (length(cutoffs_to_train) == 0 || max(cutoffs_to_train) < max_date) {
        cutoffs_to_train <- unique(c(cutoffs_to_train, max_date))
      }
    }
    
  } else {
    # Primer entrenamiento histórico absoluto (sin RDS viejo)
    cv_old <- tibble()
    grid_all <- seq(min_date, max_date, by = "90 days")
    cutoffs_to_train <- grid_all[grid_all >= start_date]
    if (length(cutoffs_to_train) == 0 || max(cutoffs_to_train) < max_date) {
      cutoffs_to_train <- unique(c(cutoffs_to_train, max_date))
    }
  }
  
  cutoffs_to_train <- as.Date(cutoffs_to_train, origin = "1970-01-01")
  
  # Si no hay fechas nuevas por evaluar, retornar reciclando lo existente
  if (length(cutoffs_to_train) == 0 && nrow(cv_old) > 0) {
    n_win <- nrow(cv_old)
    weights <- decay_factor^((n_win - 1):0)
    weights <- weights / sum(weights)
    weighted_mape <- sum(cv_old$mape * weights, na.rm = TRUE)
    return(list(
      fit = old_fit_obj$fit, splits = old_fit_obj$splits, cv_metrics = cv_old, 
      weighted_mape = weighted_mape, final_metrics = old_fit_obj$final_metrics
    ))
  }
  
  # Heredar objetos por defecto
  last_fit <- if(!is.null(old_fit_obj)) old_fit_obj$fit else NULL
  last_splits <- if(!is.null(old_fit_obj)) old_fit_obj$splits else NULL
  final_metrics <- if(!is.null(old_fit_obj)) old_fit_obj$final_metrics else NULL
  
  cv_results <- list()
  
  # 5. Entrenar ÚNICAMENTE las ventanas nuevas faltantes
  for (i in seq_along(cutoffs_to_train)) {
    cutoff <- cutoffs_to_train[i]
    window_data <- ts_data |> filter(.date_var <= cutoff)
    
    splits <- initial_time_split(window_data, prop = 0.9)
    train_data <- training(splits)
    test_data <- testing(splits)
    
    wf <- workflow_fn(train_data)
    fit_model <- wf |> fit(data = train_data)
    
    preds <- predict(fit_model, test_data) |> bind_cols(test_data)
    
    mape_val <- mape_vec(preds$PUTs, preds$.pred)
    rmse_val <- rmse_vec(preds$PUTs, preds$.pred)
    mae_val  <- mae_vec(preds$PUTs, preds$.pred)
    mse_val  <- rmse_val^2
    
    cv_results[[i]] <- tibble(
      cutoff_date = cutoff,
      mape = mape_val,
      rmse = rmse_val,
      mae = mae_val,
      mse = mse_val
    )
    
    # Conservar el modelo y las métricas finales solo del punto más reciente (el flotante o la nueva ancla final)
    if (cutoff == max(cutoffs_to_train)) {
      last_fit <- fit_model
      last_splits <- splits
      
      resids <- preds$PUTs - preds$.pred
      rsq_val <- rsq_vec(preds$PUTs, preds$.pred)
      
      norm_pvalue <- tryCatch(shapiro.test(resids)$p.value, error = function(e) NA)
      homo_pvalue <- tryCatch({
        lm_mod <- lm(resids^2 ~ seq_along(resids))
        lmtest::bptest(lm_mod)$p.value
      }, error = function(e) NA)
      ac_pvalue <- tryCatch(Box.test(x = resids, lag = 7, type = "Ljung-Box")$p.value, error = function(e) NA)
      
      final_metrics <- tibble(
        rsq = rsq_val, norm_pvalue = norm_pvalue, 
        homo_pvalue = homo_pvalue, ac_pvalue = ac_pvalue
      )
    }
  }
  
  # 6. Unir el historial limpio con las evaluaciones recién entrenadas
  new_cv_df <- bind_rows(cv_results)
  cv_df <- bind_rows(cv_old, new_cv_df) |> 
    arrange(cutoff_date) |> 
    mutate(window = row_number()) # Reasignar número secuencial
  
  # 7. Recalcular el MAPE Ponderado Exponencial
  n_win <- nrow(cv_df)
  weights <- decay_factor^((n_win - 1):0)
  weights <- weights / sum(weights) 
  
  weighted_mape <- sum(cv_df$mape * weights, na.rm = TRUE)
  
  list(
    fit = last_fit,
    splits = last_splits,
    cv_metrics = cv_df,
    weighted_mape = weighted_mape,
    final_metrics = final_metrics
  )
}