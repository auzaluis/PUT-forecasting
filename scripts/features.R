pacman::p_load(
  timetk,
  tidyverse,
  tsibble
)

superbowl_dates <- as.Date(c(
  "2022-02-13",
  "2023-02-12",
  "2024-02-11",
  "2025-02-09",
  "2026-02-08"
))

add_features <- function(data, superbowl_input) {
  data |> 
    tk_augment_timeseries_signature(.date_var = .date_var) |>
    tk_augment_holiday_signature(
      .date_var = .date_var,
      .holiday_pattern = "none",
      .locale_set = "US",
      .exchange_set = "none"
    ) |>
    mutate(
      wknd_holiday = ifelse(
        locale_US == 1 & wday.lbl %in% c("Friday", "Saturday", "Sunday", "Monday"),
        1, 0
      ),
      wday_holiday = ifelse(
        locale_US == 1 & wday.lbl %in% c("Tuesday", "Wednesday", "Thursday"),
        1, 0
      ),
      superbowl = ifelse(.date_var %in% superbowl_input, 1, 0),
      trend = row_number()
    ) |>
    select(
      .date_var,
      PUTs,
      year,
      month.lbl,
      wday.lbl,
      dst_flag,
      wknd_holiday,
      wday_holiday,
      superbowl,
      trend
    ) |> 
    as_tsibble(index = .date_var)
}