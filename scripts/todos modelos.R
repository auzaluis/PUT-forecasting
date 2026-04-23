# Cargar librerías necesarias
pacman::p_load(
  tidyverse,
  modeltime,
  tidymodels,
  arrow,
  lmtest,
  timetk
)

# Cargar funciones y datos
source("scripts/preprocess.R")
source("scripts/features.R")
source("scripts/future_ts.R")
source("scripts/utils.R")

# Rutas y modelos
models_path <- "data/models"
model_files_arimax <- list.files(models_path, pattern = "(^arimax_.*)\\.rds$", full.names = TRUE)
model_files_glmnet <- list.files(models_path, pattern = "(^glmnet_.*)\\.rds$", full.names = TRUE)

model_lookup_arimax <- setNames(model_files_arimax, gsub("arimax_|\\.rds", "", basename(model_files_arimax)))
model_lookup_glmnet <- setNames(model_files_glmnet, gsub("glmnet_|\\.rds", "", basename(model_files_glmnet)))

raw_data_path <- "data/raw_data_PUTs_Panel.parquet"
df <- load_data(raw_data_path) |> filter(daypart == "total_day")

# Valores únicos
dates <- c("2022-01-01", "2023-01-01")
ages <- unique(df$age_range)
dayparts <- unique(df$daypart)
hours <- sort(unique(df$hour))
conf_levels <- c(80, 90, 95)

# Función para intervalos manuales
calc_manual_intervals <- function(forecast_tbl, level, model_tbl, calibration_data) {
  z <- qnorm((1 + level) / 2)
  resid_tbl <- model_tbl |>
    modeltime_calibrate(new_data = calibration_data) |>
    modeltime_residuals() |>
    group_by(.model_id) |>
    summarise(sd_resid = sd(.residuals, na.rm = TRUE))
  
  forecast_tbl |>
    left_join(resid_tbl, by = ".model_id") |>
    mutate(
      .conf_lo = .value - z * sd_resid,
      .conf_hi = .value + z * sd_resid
    )
}

# Validación: claves disponibles
claves_arimax <- names(model_lookup_arimax)
claves_glmnet <- names(model_lookup_glmnet)

resultados <- list()
contador <- 0
total_combinaciones <- length(dates) * length(ages) * length(dayparts) * length(hours) * length(conf_levels)

cat("Procesando", total_combinaciones, "combinaciones...\n")

for (d in dates) {
  for (a in ages) {
    for (dp in dayparts) {
      for (h in hours) {
        for (cl in conf_levels) {
          
          contador <- contador + 1
          cat("Combinación", contador, "de", total_combinaciones, ":", d, a, h, cl, "\n")
          
          key <- get_model_key(dp, a, h, d)
          
          if (!(key %in% claves_arimax) || !(key %in% claves_glmnet)) {
            next
          }
          
          arimax_obj <- readRDS(model_lookup_arimax[[key]])
          glmnet_obj <- readRDS(model_lookup_glmnet[[key]])
          
          if (is.null(arimax_obj$fit) || is.null(glmnet_obj$fit)) next
          
          arimax_fit <- arimax_obj$fit
          glmnet_fit <- glmnet_obj$fit
          splits <- arimax_obj$splits
          
          model_tbl <- tryCatch(
            modeltime_table(arimax_fit, glmnet_fit),
            error = function(e) {
              cat("Error creando modeltime_table para", key, "\n")
              return(NULL)
            }
          )
          
          if (is.null(model_tbl)) next
          
          # Preprocesar datos y generar futuro
          df_temp <- preprocess_data(df, dp, h, a, d)
          ts_temp <- add_features(df_temp, superbowl_dates)
          future <- generate_future_ts(ts_temp |> as_tibble(), h, features)
          new_data <- bind_rows(testing(splits), future)
          
          forecast_tbl <- model_tbl |>
            modeltime_calibrate(new_data, quiet = TRUE) |>
            modeltime_forecast(new_data = new_data, actual_data = ts_temp, conf_interval = FALSE)
          
          forecast_tbl <- calc_manual_intervals(forecast_tbl, cl/100, model_tbl, new_data)
          
          ts_obs <- ts_temp |> select(.date_var, observado = PUTs)
          
          forecast_tbl <- forecast_tbl |>
            select(.index, .model_desc, .value, .conf_lo, .conf_hi) |>
            left_join(ts_obs, by = c(".index" = ".date_var")) |>
            mutate(
              fecha = .index,
              age_range = a,
              daypart = dp,
              hour = h,
              conf_level = cl,
              modelo = .model_desc,
              prediccion = .value,
              intervalo_inf = .conf_lo,
              intervalo_sup = .conf_hi,
              fecha_base = ifelse(d == "2023-01-01", "2023-01-01", "2022-01-01")  # ✅ Nueva columna
            ) |>
            select(fecha, fecha_base, age_range, daypart, hour, conf_level, modelo, observado, prediccion, intervalo_inf, intervalo_sup)
          
          resultados[[length(resultados) + 1]] <- forecast_tbl
        }
      }
    }
  }
}

final_data <- bind_rows(resultados)

if (nrow(final_data) == 0) {
  stop("No se generaron datos. Verifica que existan modelos para las combinaciones.")
}

write.csv(final_data, "forecast_longterm_completo_panel.csv", row.names = FALSE)
cat("✅ Archivo generado: forecast_longterm_completo.csv\n")
