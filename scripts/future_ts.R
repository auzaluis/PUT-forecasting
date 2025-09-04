library(timetk)
library(tsibble)
library(tidyverse)

generate_future_ts <- function(data, hours, superbowl) {
  
  print(data)
  
  max_datetime <- data |>
    pull(.date_var) |> 
    max()
  
  future_ts <- tibble(
    intervals_dim = seq(
      from = max_datetime + minutes(15),
      to = ymd_hms(paste(year(max_datetime) + 1, "-12-31 23:59:59"), tz = "EST"),
      by = "1 hour"
    )
  ) |>
    mutate(
      dst_flag = dst(with_tz(intervals_dim, "America/New_York")) |> as.integer(),
      .date_var = as_date(intervals_dim)
    ) |> 
    filter(hour(intervals_dim) %in% hours) |> 
    tk_augment_timeseries_signature(.date_var = .date_var) |>
    tk_augment_holiday_signature(
      .date_var = .date_var,
      .holiday_pattern = "none",
      .locale_set = "US",
      .exchange_set = "none"
    ) |>
    mutate(
      wknd_holiday = ifelse(
        test = locale_US == 1 & wday.lbl %in% c("Friday", "Saturday", "Sunday", "Monday"),
        yes = 1,
        no = 0
      ) |> as.integer(),
      wday_holiday = ifelse(
        test = locale_US == 1 & wday.lbl %in% c("Tuesday", "Wednesday", "Thursday"),
        yes = 1,
        no = 0
      ) |> as.integer()
    ) |> 
    select(
      .date_var,
      year,
      month.lbl,
      wday.lbl,
      dst_flag,
      wknd_holiday,
      wday_holiday
    ) |> 
    mutate(superbowl = ifelse(.date_var %in% superbowl, 1, 0) |> as.integer()) |> 
    arrange(.date_var) |> 
    mutate(trend = max(data$trend) + row_number()) |> 
    as_tsibble(index = .date_var)
  
  future_ts
  
}