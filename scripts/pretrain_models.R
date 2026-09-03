pacman::p_load(
  tidyverse,
  arrow,
  future,
  furrr
)

source("scripts/preprocess.R")
source("scripts/features.R")
source("scripts/workflows.R")
source("scripts/utils.R")

# 🟢 NUEVO: Control maestro de reentrenamiento
# TRUE = Sobreescribe todos los modelos (Reentrenamiento total)
# FALSE = Solo entrena los que falten (Ideal si se interrumpió el proceso)
force_retrain <- TRUE 

# Load raw_data
models_path <- "data/models"
if(!dir.exists(models_path)) dir.create(models_path, recursive = TRUE)

raw_data_path <- "data/raw_data_PUTs_Panel.parquet"
df <- load_data(raw_data_path)

df_models <- df |>
  filter(daypart == "total_day") 

# 🟢 Ajuste a fecha única (como vimos en los pasos anteriores)
min_data_date <- min(df_models$.date_var)

combinations <- expand.grid(
  daypart = unique(df_models$daypart),
  age_range = unique(df_models$age_range),
  hour = unique(df_models$hour),
  date = min_data_date,
  stringsAsFactors = FALSE
)

combinations <- combinations |>
  mutate(
    key = paste(
      clean_name(as.character(daypart)),
      clean_name(as.character(age_range)),
      clean_name(as.character(hour)),
      clean_name(as.character(date)),
      sep = "_"
    ),
    arimax_exists = file.exists(file.path(models_path, paste0("arimax_", key, ".rds"))),
    glmnet_exists = file.exists(file.path(models_path, paste0("glmnet_", key, ".rds")))
  ) 

# 🟢 LÓGICA MODIFICADA: Si NO estamos forzando el reentrenamiento, entonces sí filtramos
if (!force_retrain) {
  combinations <- combinations |>
    filter(!(arimax_exists & glmnet_exists))
}

# Limpiar columnas auxiliares
combinations <- combinations |> select(-key, -arimax_exists, -glmnet_exists)

cat("✅ Modelos pendientes por entrenar:", nrow(combinations), "\n")

if(nrow(combinations) == 0) {
  stop("¡Todas las combinaciones ya están listas! No hay nada que entrenar.")
}
# =========================================================================
# 2. Configurar Paralelización Real
num_cores <- max(1, future::availableCores() - 1)
plan(multisession, workers = num_cores)
cat("⚡ Entrenando en paralelo usando", num_cores, "núcleos CPU...\n")

# Usar future_walk para aprovechar todos los núcleos
future_walk(1:nrow(combinations), function(i) {
  combo <- combinations[i, ]
  cat("Procesando:", combo$daypart, combo$age_range, combo$hour, as.character(combo$date), "\n")
  
  # Construir las rutas a los archivos .rds
  key <- paste(
    clean_name(as.character(combo$daypart)),
    clean_name(as.character(combo$age_range)),
    clean_name(as.character(combo$hour)),
    clean_name(as.character(combo$date)),
    sep = "_"
  )
  
  arimax_path <- file.path(models_path, paste0("arimax_", key, ".rds"))
  glmnet_path <- file.path(models_path, paste0("glmnet_", key, ".rds"))
  
  # 🟢 NUEVO: Cargar los modelos viejos si existen en la carpeta
  old_arimax <- if (file.exists(arimax_path)) readRDS(arimax_path) else NULL
  old_glmnet <- if (file.exists(glmnet_path)) readRDS(glmnet_path) else NULL
  
  # Preprocesar los datos actualizados
  data <- preprocess_data(df_models, combo$daypart, combo$hour, combo$age_range, combo$date)
  ts_data <- add_features(data, features)
  
  # 🟢 NUEVO: Pasar el modelo viejo a la función para reciclar las métricas históricas
  fit_arimax <- train_cv_models(ts_data, workflow_arimax, old_fit_obj = old_arimax)
  fit_glmnet <- train_cv_models(ts_data, workflow_glmnet, old_fit_obj = old_glmnet)
  
  # Sobreescribir el archivo .rds con el modelo actualizado
  saveRDS(fit_arimax, file = arimax_path)
  saveRDS(fit_glmnet, file = glmnet_path)
  
}, .options = furrr_options(seed = TRUE))

cat("✅ Entrenamiento finalizado.\n")