pacman::p_load(
  arrow,
  tidyverse
)

load_data <- function(path) {
  read_parquet(path) |>
    mutate(
      intervals_dim = ymd_hms(intervals_dim, tz = "EST"),
      dst_flag = dst(with_tz(intervals_dim, "America/New_York")) |> as.integer(),
      .date_var = as_date(intervals_dim),
      hour = hour(intervals_dim)
    )
}

preprocess_data <- function(data, daypart_input, hour_input, age_input, date_input) {
  data |>
    filter(
      daypart == daypart_input,
      hour %in% hour_input,
      age_range == age_input,
      .date_var >= date_input
    ) |> 
    group_by(.date_var, dst_flag) |> 
    summarise(PUTs = mean(PUTs, na.rm = TRUE)) |>
    arrange(.date_var) |>
    ungroup()
}
