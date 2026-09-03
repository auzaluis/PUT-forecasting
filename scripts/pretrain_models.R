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

# TRUE = Obliga a evaluar todo el grid, procesando la validación incremental
force_retrain <- TRUE

models_path <- "data/models"
if(!dir.exists(models_path)) dir.create(models_path, recursive = TRUE)

raw_data_path <- "data/raw_data_PUTs_Panel.parquet"
df <- load_data(raw_data_path)

# 🟢 SE MANTIENEN LAS DOS FECHAS ORIGINALES
date_values <- as.Date(c("2022-01-01", "2023-01-01"))

df_models <- df |>
  filter(daypart == "total_day") 

combinations <- expand.grid(
  daypart = unique(df_models$daypart),
  age_range = unique(df_models$age_range),
  hour = unique(df_models$hour),
  date = date_values,
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

if (!force_retrain) {
  combinations <- combinations |>
    filter(!(arimax_exists & glmnet_exists))
}

combinations <- combinations |> select(-key, -arimax_exists, -glmnet_exists)

cat("✅ Modelos a procesar:", nrow(combinations), "\n")

if(nrow(combinations) == 0) {
  stop("¡Todas las combinaciones ya están listas! No hay nada que entrenar.")
}

# =========================================================================
# 2. Configurar Paralelización Real
num_cores <- max(1, future::availableCores() - 1)
plan(multisession, workers = num_cores)
cat("⚡ Entrenando en paralelo usando", num_cores, "núcleos CPU...\n")

future_walk(1:nrow(combinations), function(i) {
  combo <- combinations[i, ]
  cat("Procesando:", combo$daypart, combo$age_range, combo$hour, as.character(combo$date), "\n")
  
  key <- paste(
    clean_name(as.character(combo$daypart)),
    clean_name(as.character(combo$age_range)),
    clean_name(as.character(combo$hour)),
    clean_name(as.character(combo$date)),
    sep = "_"
  )
  
  arimax_path <- file.path(models_path, paste0("arimax_", key, ".rds"))
  glmnet_path <- file.path(models_path, paste0("glmnet_", key, ".rds"))
  
  # 🟢 Cargar los modelos viejos para aplicar la actualización incremental
  old_arimax <- if (file.exists(arimax_path)) readRDS(arimax_path) else NULL
  old_glmnet <- if (file.exists(glmnet_path)) readRDS(glmnet_path) else NULL
  
  data <- preprocess_data(df_models, combo$daypart, combo$hour, combo$age_range, combo$date)
  ts_data <- add_features(data, features)
  
  # 🟢 Pasarle los objetos viejos
  fit_arimax <- train_cv_models(ts_data, workflow_arimax, old_fit_obj = old_arimax)
  fit_glmnet <- train_cv_models(ts_data, workflow_glmnet, old_fit_obj = old_glmnet)
  
  saveRDS(fit_arimax, file = arimax_path)
  saveRDS(fit_glmnet, file = glmnet_path)
}, .options = furrr_options(seed = TRUE))

cat("✅ Entrenamiento finalizado.\n")