pacman::p_load(timetk, tidyverse, tsibble, readxl)

# Carga limpia de features desde Excel
feature_dates <- read_excel("data/features_2.xlsx") 

features <- map(1:ncol(feature_dates), ~ {
  feature_dates %>% 
    pull(.x) %>% 
    na.omit() %>% 
    as.Date()
})


add_features <- function(data, features_input = features) {
  
  has_user_hour <- "hour" %in% names(data)
  if (has_user_hour) {
    data <- data %>% rename(hour_orig = hour)
  }
  
  df_feat <- data |> 
    tk_augment_timeseries_signature(.date_var = .date_var) |>
    tk_augment_holiday_signature(
      .date_var = .date_var,
      .holiday_pattern = "none",
      .locale_set = "US",
      .exchange_set = "none"
    ) |>
    mutate(
      wknd_holiday = ifelse(locale_US == 1 & wday.lbl %in% c("Friday", "Saturday", "Sunday", "Monday"), 1L, 0L),
      wday_holiday = ifelse(locale_US == 1 & wday.lbl %in% c("Tuesday", "Wednesday", "Thursday"), 1L, 0L),
      
      endyear       = ifelse(.date_var %in% features_input[[1]], 1L, 0L),
      newyear       = ifelse(.date_var %in% features_input[[2]], 1L, 0L),
      christmas     = ifelse(.date_var %in% features_input[[3]], 1L, 0L),
      christmas_eve = ifelse(.date_var %in% features_input[[4]], 1L, 0L),
      thanksgiving  = ifelse(.date_var %in% features_input[[5]], 1L, 0L),
      
      football_cup  = case_when(
        .date_var %in% features_input[[6]]  ~ 1L,
        .date_var %in% features_input[[11]] ~ 2L,
        .date_var %in% features_input[[12]] ~ 3L,
        TRUE ~ 0L
      ),
      
      olimpics   = ifelse(.date_var %in% features_input[[7]], 1L, 0L),
      mnf        = ifelse(.date_var %in% features_input[[8]], 1L, 0L),
      superbowl  = ifelse(.date_var %in% features_input[[9]], 1L, 0L),
      sunday_nfl = ifelse(.date_var %in% features_input[[10]], 1L, 0L),
      tnf        = ifelse(.date_var %in% features_input[[13]], 1L, 0L),
      nba        = ifelse(.date_var %in% features_input[[14]], 1L, 0L),
      mlb        = ifelse(.date_var %in% features_input[[15]], 1L, 0L),
      trend      = row_number(),
      
      # Variable de cambio de universo
      cambiodeuniverso = ifelse(.date_var > as.Date("2026-01-31"), 1L, 0L),
      
      # 🟢 NUEVO: Lags precalculados en los datos históricos
      lag_5 = lag(PUTs, 5),
      lag_7 = lag(PUTs, 7)
    )
  
  if ("hour_orig" %in% names(df_feat)) {
    df_feat <- df_feat %>% 
      select(-hour) %>% 
      rename(hour = hour_orig)
  }
  
  df_feat %>% 
    select(
      any_of(c("daypart", "age_range", "hour")),
      .date_var, PUTs, year, month.lbl, wday.lbl, dst_flag,
      wknd_holiday, wday_holiday, endyear, newyear, christmas,
      christmas_eve, thanksgiving, football_cup, olimpics, mnf,
      superbowl, sunday_nfl, tnf, nba, mlb, trend,
      cambiodeuniverso,
      lag_5, lag_7 # 🟢 Incluir los lags en la selección
    )
}