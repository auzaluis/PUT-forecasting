pacman::p_load(
  tidyverse,
  arrow
)
source("scripts/preprocess.R")
source("scripts/features.R")
source("scripts/train_arimax.R")

# Load raw_data
models_path <- "data/models"
raw_data_path <- "data/raw_data.parquet"
df <- load_data(raw_data_path)

# clean_name function
clean_name <- function(x) {
  x |> 
    str_replace_all("[^A-Za-z0-9]", "_") |> 
    str_replace_all("_+", "_") |> 
    str_replace_all("^_|_$", "")
}

date_values <- as.Date(c("2022-01-01", "2023-01-01"))

df_models <- df |> 
  filter(daypart == 'total_day')

combinations <- expand.grid(
  daypart = unique(df_models$daypart),
  age_range = unique(df_models$age_range),
  hour = unique(df_models$hour),
  date = date_values
)

models <- list()

for (i in seq_len(nrow(combinations))) {
  combo <- combinations[i, ]
  print(combo)
  data <- preprocess_data(df_models, combo$daypart, combo$hour, combo$age_range, combo$date)
  ts_data <- add_features(data, superbowl_dates)
  fit <- train_arimax(ts_data)
  key <- paste(
    clean_name(as.character(combo$daypart)),
    clean_name(as.character(combo$age_range)),
    clean_name(as.character(combo$hour)),
    clean_name(as.character(combo$date)),
    sep = "_"
  )
  saveRDS(fit, file = file.path("data/models", paste0("arimax_", key, ".rds")))
}
