pacman::p_load(
  timetk,
  tidyverse,
  tsibble,
  readxl
)

feature_dates<-read_excel("data/features.xlsx") 
features<-list()
for(i in 1:ncol(feature_dates)){
  features[[i]]<-(feature_dates %>% pull(i) %>% na.omit()%>% as.Date())
}

add_features <- function(data, features_input) {
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
      endyear = ifelse(.date_var %in% features[[1]], 1, 0),
      newyear = ifelse(.date_var %in% features[[2]], 1, 0),
      christmas = ifelse(.date_var %in% features[[3]], 1, 0),
      christmas_eve = ifelse(.date_var %in% features[[4]], 1, 0),
      thanksgiving = ifelse(.date_var %in% features[[5]], 1, 0),
      mundial = ifelse(.date_var %in% features[[6]], 1, 0),
      olimpics = ifelse(.date_var %in% features[[7]], 1, 0),
      mnf = ifelse(.date_var %in% features[[8]], 1, 0),
      superbowl = ifelse(.date_var %in% features[[9]], 1, 0),
      sunday_nfl = ifelse(.date_var %in% features[[10]], 1, 0),
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
      endyear,
      newyear,
      christmas,
      christmas_eve,
      thanksgiving,
      mundial,
      olimpics,
      mnf,
      superbowl,
      sunday_nfl,
      trend
    )
}