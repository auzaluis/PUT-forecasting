library(arrow)
library(tidyverse)

load_data <- function(path) {
  read_parquet(path) |>
    mutate(
      intervals_dim = ymd_hms(intervals_dim, tz = "EST"),
      dst_flag = dst(with_tz(intervals_dim, "America/New_York")) |> as.integer(),
      .date_var = as_date(intervals_dim),
      hour = hour(intervals_dim)
    )
}

preprocess_data <- function(df, daypart, hours, age_range, date) {
  df |>
    filter(
      daypart == daypart,
      hour %in% hours,
      age_range == age_range,
      .date_var >= date
    ) |> 
    group_by(.date_var, dst_flag) |> 
    summarise(PUTs = mean(PUTs, na.rm = TRUE)) |>
    arrange(.date_var) |>
    ungroup()
}
