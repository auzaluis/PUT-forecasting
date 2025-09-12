pacman::p_load(
  tidyverse
)

clean_name <- function(x) {
  x |> 
    str_replace_all("[^A-Za-z0-9]", "_") |> 
    str_replace_all("_+", "_") |> 
    str_replace_all("^_|_$", "")
}

get_model_key <- function(daypart, age_range, hour, date) {
  paste(
    clean_name(as.character(daypart)),
    clean_name(as.character(age_range)),
    clean_name(as.character(hour)),
    clean_name(as.character(date)),
    sep = "_"
  )
}

parse_model_filename <- function(file_name) {
  tibble(model = file_name) |>
    extract(
      col = model,
      into = c("model", "date"),
      regex = "^(.*)_([0-9]{4}_[0-9]{2}_[0-9]{2})\\.rds$",
      remove = TRUE
    ) |>
    mutate(date = str_remove(date, "\\.rds$")) |> 
    extract(
      col = model,
      into = c("model", "hour"),
      regex = "^(.*)_(\\d+)$",
      remove = TRUE,
      convert = TRUE
    ) |>
    extract(
      col = model,
      into = c("model", "age_range"),
      regex = "^(.*)_(P.*)$",
      remove = TRUE
    ) |> 
    extract(
      col = model,
      into = c("model", "daypart"),
      regex = "^([^_]+)_(.+)$",
      remove = TRUE
    )
}