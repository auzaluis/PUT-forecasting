pacman::p_load(
  shiny,
  tidyverse,
  modeltime,
  tidymodels,
  rsample,
  arrow,
  plotly,
  shinyWidgets,
  DT,
  lmtest,
  timetk,
  writexl
)

# ==========================================
# 1. CARGA DE MÓDULOS Y FUNCIONES EXTERNAS
# ==========================================
source("scripts/preprocess.R")
source("scripts/features.R")
source("scripts/future_ts.R")
source("scripts/utils.R")

# ==========================================
# 2. FUNCIONES AUXILIARES
# ==========================================

# Parsing robusto de nombres de modelos (Evita NAs si age_range varía)
parse_model_filename_robust <- function(file_name) {
  clean_fn <- str_remove(file_name, "\\.rds$")
  
  model_type <- str_extract(clean_fn, "^(arimax|glmnet)")
  date_val   <- str_extract(clean_fn, "\\d{4}_\\d{2}_\\d{2}$")
  
  mid_str <- clean_fn |> 
    str_remove("^(arimax|glmnet)_") |> 
    str_remove("_\\d{4}_\\d{2}_\\d{2}$")
  
  hour_val <- as.integer(str_extract(mid_str, "\\d+$"))
  mid_no_hour <- str_remove(mid_str, "_\\d+$")
  
  if (str_detect(mid_no_hour, "^total_day_")) {
    daypart_val <- "total_day"
    age_val     <- str_remove(mid_no_hour, "^total_day_")
  } else {
    parts <- str_split(mid_no_hour, "_")[[1]]
    daypart_val <- parts[1]
    age_val     <- paste(parts[-1], collapse = "_")
  }
  
  tibble(
    model = model_type,
    daypart = daypart_val,
    age_range = age_val,
    hour = hour_val,
    date = date_val
  )
}

# Función auxiliar para calcular intervalos múltiples (80%, 90%, 95%)
calc_multi_intervals <- function(forecast_tbl, model_tbl, calibration_data) {
  resid_tbl <- model_tbl |>
    modeltime_calibrate(new_data = calibration_data, quiet = TRUE) |>
    modeltime_residuals() |>
    group_by(.model_id) |>
    summarise(sd_resid = sd(.residuals, na.rm = TRUE), .groups = "drop")
  
  z80 <- qnorm((1 + 0.80) / 2)
  z90 <- qnorm((1 + 0.90) / 2)
  z95 <- qnorm((1 + 0.95) / 2)
  
  forecast_tbl |>
    left_join(resid_tbl, by = ".model_id") |>
    mutate(
      lo_80 = .value - z80 * sd_resid,
      hi_80 = .value + z80 * sd_resid,
      lo_90 = .value - z90 * sd_resid,
      hi_90 = .value + z90 * sd_resid,
      lo_95 = .value - z95 * sd_resid,
      hi_95 = .value + z95 * sd_resid
    ) |>
    select(-sd_resid)
}

# 🟢 NUEVO: Función para calcular Promedio Ponderado Exponencial
calc_weighted_metric <- function(vec, decay_factor = 0.85) {
  n_win <- length(vec)
  if(n_win == 0) return(NA)
  weights <- decay_factor^((n_win - 1):0)
  weights <- weights / sum(weights)
  sum(vec * weights, na.rm = TRUE)
}

# ==========================================
# 3. PRECARGA DE DATOS Y RUTAS
# ==========================================
models_path <- "data/models"
model_files_arimax <- list.files(models_path, pattern = "(^arimax_.*)\\.rds$", full.names = TRUE)
model_files_glmnet <- list.files(models_path, pattern = "(^glmnet_.*)\\.rds$", full.names = TRUE)
model_files <- c(model_files_arimax, model_files_glmnet)

raw_data_path <- "data/raw_data_PUTs_Panel.parquet"
df <- load_data(raw_data_path) |> filter(daypart == "total_day")

# Asumiendo que la columna de fechas en 'df' se llama 'date' o '.date_var':
min_data_date <- min(df$.date_var, na.rm = TRUE)

model_lookup_arimax <- setNames(model_files_arimax, gsub("arimax_|\\.rds", "", basename(model_files_arimax)))
model_lookup_glmnet <- setNames(model_files_glmnet, gsub("glmnet_|\\.rds", "", basename(model_files_glmnet)))


#date_values <- as.Date(c("2022-01-01", "2023-01-01"))

# ==========================================
# 4. INTERFAZ DE USUARIO (UI)
# ==========================================
ui <- fluidPage(
  titlePanel("People Using TV Forecasting (Rolling Validation 90/10)"),
  sidebarLayout(
    sidebarPanel(
      # En la sección sidebarPanel():
      # Opción recomendable: Desactivar o fijar a la única fecha disponible
      pickerInput("date", "Data Start Date:", choices = min_data_date, selected = min_data_date),
      pickerInput("age_range", "Age Range:", choices = unique(df$age_range)),
      pickerInput("daypart", "Daypart:", choices = unique(df$daypart)[1], selected = unique(df$daypart)[1]),
      pickerInput("hours", "Hours:", choices = sort(unique(df$hour)), selected = 20),
      pickerInput("conf_level", "Nivel de intervalo:", choices = c(80, 90, 95), selected = 95)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Chart", plotlyOutput("forecastPlot")),
        tabPanel("Long-term", plotlyOutput("refit_forecastPlot")),
        tabPanel("MAPE Evolution (CV)", plotlyOutput("mape_cv_plot")),
        tabPanel("Summary", verbatimTextOutput("printFit")),
        tabPanel("Accuracy", verbatimTextOutput("printAccurary")),
        tabPanel("Residuals", br(),
                 tabsetPanel(
                   tabPanel("Time Plot", plotlyOutput("timeplot_resid")),
                   tabPanel("ACF", plotOutput("acf_resid"))
                 )
        ),
        tabPanel("All models performance", br(),
                 tabsetPanel(
                   tabPanel("Metrics Table", 
                            DT::dataTableOutput("mape_table"),
                            br(),
                            downloadButton("download_metrics", "📥 Descargar métricas")
                   ),
                   tabPanel("Metrics Distribution", br(),
                            selectInput("metric_hist_input", "Select metric:", 
                                        choices = c("MAPE" = "mape", "RMSE" = "rmse", "MSE" = "mse", "MAE" = "mae", "R²" = "rsq", "Normalidad" = "norm_pvalue", "Homocedasticidad" = "homo_pvalue", "Autocorrelación" = "ac_pvalue"), selected = "mape"),
                            plotlyOutput("metric_hist")
                   ),
                   tabPanel("Metrics Facet", br(),
                            selectInput("metric_facet_input", "Select metric:", 
                                        choices = c("MAPE" = "mape", "RMSE" = "rmse", "MSE" = "mse", "MAE" = "mae", "R²" = "rsq", "Normalidad" = "norm_pvalue", "Homocedasticidad" = "homo_pvalue", "Autocorrelación" = "ac_pvalue"), selected = "mape"),
                            plotlyOutput("metric_facet")
                   )
                 )
        ),
        tabPanel("Best Models & Export", br(),
                 h4("Mejor modelo por combinación (Menor MAPE Ponderado)"),
                 p("Selecciona una fila de la tabla para activar la descarga de predicciones a largo plazo."),
                 DT::dataTableOutput("best_models_table"),
                 br(),
                 uiOutput("download_best_pred_ui")
        )
      )
    )
  )
)

# ==========================================
# 5. LÓGICA DEL SERVIDOR (SERVER)
# ==========================================
server <- function(input, output, session) {
  
  # Si removiste input$date de la UI, reemplázalo por min_data_date
  df1 <- reactive({ 
    preprocess_data(df, input$daypart, input$hours, input$age_range, min_data_date) 
  })
  ts <- reactive({ add_features(df1(), features) })
  
  arimax_obj <- reactive({
    key <- get_model_key(input$daypart, input$age_range, input$hours, input$date)
    model_file <- model_lookup_arimax[[key]]
    if (is.null(model_file) || !file.exists(model_file)) return(NULL)
    readRDS(model_file)
  })
  
  glmnet_obj <- reactive({
    key <- get_model_key(input$daypart, input$age_range, input$hours, input$date)
    model_file <- model_lookup_glmnet[[key]]
    if (is.null(model_file) || !file.exists(model_file)) return(NULL)
    readRDS(model_file)
  })
  
  arimax_model_tbl <- reactive({
    modeltime_table(arimax_obj()$fit, glmnet_obj()$fit)
  })
  
  calc_manual_intervals <- function(forecast_tbl, level, model_tbl, calibration_data) {
    z <- qnorm((1 + level) / 2)
    resid_tbl <- model_tbl |>
      modeltime_calibrate(new_data = calibration_data, quiet = TRUE) |>
      modeltime_residuals() |>
      group_by(.model_id) |>
      summarise(sd_resid = sd(.residuals, na.rm = TRUE))
    
    forecast_tbl |>
      left_join(resid_tbl, by = ".model_id") |>
      mutate(
        .conf_lo = .value - z * sd_resid,
        .conf_hi = .value + z * sd_resid
      )
  }
  
  output$forecastPlot <- renderPlotly({
    splits <- arimax_obj()$splits
    ts_data <- ts()
    
    forecast_tbl <- arimax_model_tbl() |>
      modeltime_calibrate(new_data = testing(splits), quiet = TRUE) |>
      modeltime_forecast(new_data = testing(splits), actual_data = ts_data, conf_interval = FALSE)
    
    forecast_tbl <- calc_manual_intervals(forecast_tbl, as.numeric(input$conf_level) / 100, arimax_model_tbl(), testing(splits))
    
    forecast_long <- bind_rows(
      forecast_tbl |> mutate(tipo = "Predicción", valor = .value),
      forecast_tbl |> mutate(tipo = "Límite inferior", valor = .conf_lo),
      forecast_tbl |> mutate(tipo = "Límite superior", valor = .conf_hi)
    )
    
    p <- ggplot(forecast_long, aes(x = .index, y = valor, color = .model_desc, linetype = tipo, group = interaction(.model_desc, tipo))) +
      geom_line(size = 0.4) + 
      scale_color_manual(values = c("darkblue", "forestgreen", "red")) +
      scale_linetype_manual(values = c("Predicción" = "solid", "Límite inferior" = "dashed", "Límite superior" = "dashed")) +
      theme_minimal() + 
      labs(title = "Pronóstico Última Ventana", x = "Fecha", y = "Valor")
    
    ggplotly(p, tooltip = c("x", "y", ".model_desc", "tipo")) |> 
      layout(
        legend = list(orientation = "h", x = 0.5, y = -0.3, xanchor = "center"),
        margin = list(b = 80)
      )
  })
  
  output$refit_forecastPlot <- renderPlotly({
    splits <- arimax_obj()$splits
    ts_data <- ts()
    
    future_raw <- generate_future_ts(ts_data |> as_tibble(), input$hours, features)
    
    full_ts <- bind_rows(ts_data, future_raw) |> 
      mutate(
        lag_5 = lag(PUTs, 5),
        lag_7 = lag(PUTs, 7)
      ) |> 
      fill(lag_5, lag_7, .direction = "down")
    
    future_prepared <- full_ts |> filter(.date_var > max(ts_data$.date_var))
    
    new_data <- bind_rows(testing(splits), future_prepared)
    
    forecast_tbl <- arimax_model_tbl() |>
      modeltime_calibrate(new_data, quiet = TRUE) |>
      modeltime_forecast(new_data = new_data, actual_data = ts_data, conf_interval = FALSE)
    
    forecast_tbl <- calc_manual_intervals(forecast_tbl, as.numeric(input$conf_level) / 100, arimax_model_tbl(), new_data)
    
    forecast_long <- bind_rows(
      forecast_tbl |> mutate(tipo = "Predicción", valor = .value),
      forecast_tbl |> mutate(tipo = "Límite inferior", valor = .conf_lo),
      forecast_tbl |> mutate(tipo = "Límite superior", valor = .conf_hi)
    )
    
    p <- ggplot(forecast_long, aes(x = .index, y = valor, color = .model_desc, linetype = tipo, group = interaction(.model_desc, tipo))) +
      geom_line(size = 0.4) + 
      scale_color_manual(values = c("darkblue", "forestgreen", "red")) +
      scale_linetype_manual(values = c("Predicción" = "solid", "Límite inferior" = "dashed", "Límite superior" = "dashed")) + 
      theme_minimal()
    
    ggplotly(p, tooltip = c("x", "y", ".model_desc", "tipo")) |> 
      layout(
        legend = list(orientation = "h", x = 0.5, y = -0.3, xanchor = "center"),
        margin = list(b = 80)
      )
  })
  
  # 🟢 GRÁFICO CV MAPE PONDERADO
  output$mape_cv_plot <- renderPlotly({
    req(arimax_obj(), glmnet_obj())
    
    df_ari <- arimax_obj()$cv_metrics |> mutate(model = "ARIMAX")
    df_glm <- glmnet_obj()$cv_metrics |> mutate(model = "GLMNET")
    
    cv_data <- bind_rows(df_ari, df_glm)
    
    w_ari <- round(calc_weighted_metric(df_ari$mape), 4)
    w_glm <- round(calc_weighted_metric(df_glm$mape), 4)
    
    mape_text <- paste0("<b>MAPE Ponderado (CV 13 meses):</b><br>ARIMAX: ", w_ari, "<br>GLMNET: ", w_glm)
    
    p <- ggplot(cv_data, aes(x = cutoff_date, y = mape, color = model, group = model)) +
      geom_line(size = 1) + 
      geom_point(size = 2) + 
      theme_minimal() + 
      labs(title = "MAPE Evolution across Validation Windows", x = "Cutoff Date", y = "MAPE")
    
    ggplotly(p) |> 
      layout(
        annotations = list(
          x = 0.02, y = 0.98, text = mape_text, showarrow = FALSE,
          xref = "paper", yref = "paper", bgcolor = "black",
          bordercolor = "black", borderwidth = 2,
          font = list(color = "white", size = 12), align = "left"
        ),
        legend = list(orientation = "h", x = 0.5, y = -0.3, xanchor = "center"),
        margin = list(b = 80)
      )
  })
  
  output$printFit <- renderPrint({
    cat("🔹 Modelo ARIMAX (Última Ventana):\n")
    print(arimax_obj()$fit |> extract_fit_parsnip())
    cat("\n\n🔹 Modelo GLMNET (Última Ventana):\n")
    print(glmnet_obj()$fit |> extract_fit_parsnip() |> tidy(), n = 24)
  })
  
  output$printAccurary <- renderPrint({
    cat("MAPE Ponderado Exponencial (CV) - ARIMAX:", calc_weighted_metric(arimax_obj()$cv_metrics$mape), "\n")
    cat("MAPE Ponderado Exponencial (CV) - GLMNET:", calc_weighted_metric(glmnet_obj()$cv_metrics$mape), "\n")
  })
  
  output$timeplot_resid <- renderPlotly({
    splits <- arimax_obj()$splits
    ggplotly(
      arimax_model_tbl() |>
        modeltime_calibrate(new_data = testing(splits), quiet = TRUE) |>
        modeltime_residuals() |> 
        plot_modeltime_residuals(.type = "timeplot", .interactive = FALSE) +
        theme(legend.position = "none")
    )
  })
  
  output$acf_resid <- renderPlot({
    splits <- arimax_obj()$splits 
    calibrated_tbl <- arimax_model_tbl() |> modeltime_calibrate(new_data = testing(splits), quiet = TRUE)
    residuals_tbl <- modeltime_residuals(calibrated_tbl)
    
    model_ids <- unique(residuals_tbl$.model_id)
    model_descs <- unique(residuals_tbl$.model_desc)
    par(mfrow = c(1, length(model_ids)))  
    for (i in seq_along(model_ids)) {
      resids <- residuals_tbl |> filter(.model_id == model_ids[i]) |> pull(.residuals)
      acf(resids, main = paste("ACF -", model_descs[i]))
    }
  })
  
  # 🟢 TODAS LAS MÉTRICAS CALCULADAS COMO PROMEDIO PONDERADO EXPONENCIAL
  all_metrics <- reactive({
    map_dfr(model_files, function(f) {
      obj <- readRDS(f)
      
      w_mape <- calc_weighted_metric(obj$cv_metrics$mape)
      w_rmse <- calc_weighted_metric(obj$cv_metrics$rmse)
      w_mse  <- calc_weighted_metric(obj$cv_metrics$mse)
      w_mae  <- calc_weighted_metric(obj$cv_metrics$mae)
      
      last_met <- obj$final_metrics
      parsed <- parse_model_filename_robust(basename(f))
      
      parsed |>
        mutate(
          file_path = f,
          mape = round(w_mape, 4),
          rmse = round(w_rmse, 1),
          mse = round(w_mse, 1),
          mae = round(w_mae, 1),
          rsq = round(last_met$rsq, 2),
          norm_pvalue = round(last_met$norm_pvalue, 2),
          homo_pvalue = round(last_met$homo_pvalue, 2),
          ac_pvalue = round(last_met$ac_pvalue, 2)
        )
    })
  })
  
  output$mape_table <- DT::renderDataTable({ 
    all_metrics() |> select(-file_path)
  }, options = list(pageLength = 10))
  
  output$download_metrics <- downloadHandler(
    filename = function() { paste0("metricas_modelos_", Sys.Date(), ".xlsx") },
    content = function(file) { writexl::write_xlsx(all_metrics() |> select(-file_path), path = file) },
    contentType = "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
  )
  
  # Gráfico de Histograma
  output$metric_hist <- renderPlotly({
    metric <- input$metric_hist_input
    p <- ggplot(all_metrics(), aes(x = .data[[metric]])) +
      geom_histogram(fill = "#0073C2FF", color = "white", bins = 20) + 
      facet_wrap(~ model) + # 🟢 Cambiado: Ya no requiere ~ date
      theme_minimal()
    ggplotly(p)
  })
  
  # Gráfico de Facetas
  output$metric_facet <- renderPlotly({
    metric <- input$metric_facet_input
    p <- ggplot(all_metrics(), aes(x = hour, y = .data[[metric]], color = age_range, group = age_range)) +
      geom_point(size = 3) + geom_line() + 
      facet_wrap(~ model) + # 🟢 Cambiado: Ya no requiere ~ date
      theme_minimal()
    ggplotly(p)
  })
  
  # 🟢 TABLA Y DESCARGA DE MEJORES MODELOS (Filtra por el menor mape ponderado)
  best_models_df <- reactive({
    all_metrics() |>
      group_by(daypart, age_range, hour) |>
      slice_min(order_by = mape, n = 1, with_ties = FALSE) |>
      ungroup() |>
      select(model, daypart, age_range, hour, date, mape, file_path)
  })
  
  output$best_models_table <- DT::renderDataTable({
    best_models_df() |> select(model, daypart, age_range, hour, date, mape)
  }, selection = "single", options = list(pageLength = 10))
  
  output$download_best_pred_ui <- renderUI({
    req(input$best_models_table_rows_selected)
    downloadButton("download_best_predictions", "📥 Descargar Predicciones (Excel)")
  })
  
  output$download_best_predictions <- downloadHandler(
    filename = function() {
      req(input$best_models_table_rows_selected)
      idx <- input$best_models_table_rows_selected
      b_df <- best_models_df()
      req(idx <= nrow(b_df))
      selected_row <- b_df[idx, ]
      base_name <- tools::file_path_sans_ext(basename(selected_row$file_path))
      paste0("prediccion_", base_name, ".xlsx")
    },
    content = function(file) {
      req(input$best_models_table_rows_selected)
      idx <- input$best_models_table_rows_selected
      b_df <- best_models_df()
      req(idx <= nrow(b_df))
      selected_row <- b_df[idx, ]
      
      # 1. Cargar el RDS seleccionado
      model_obj <- readRDS(selected_row$file_path)
      
      # 2. Mapeo inverso seguro a las variables originales
      df_ages <- unique(df$age_range)
      matched_age <- df_ages[clean_name(df_ages) == selected_row$age_range]
      if (length(matched_age) == 0) matched_age <- selected_row$age_range
      
      df_dayparts <- unique(df$daypart)
      matched_daypart <- df_dayparts[clean_name(df_dayparts) == selected_row$daypart]
      if (length(matched_daypart) == 0) matched_daypart <- selected_row$daypart
      
      start_date <- as.Date(gsub("_", "-", selected_row$date))
      
      # 3. Reconstrucción de series
      df_sub <- preprocess_data(df, matched_daypart, selected_row$hour, matched_age, start_date)
      if (nrow(df_sub) == 0) stop("Error: preprocess_data devolvió 0 filas.")
      
      ts_data <- add_features(df_sub, features)
      future_raw <- generate_future_ts(ts_data |> as_tibble(), selected_row$hour, features)
      
      full_ts <- bind_rows(ts_data, future_raw) |> 
        mutate(
          lag_5 = lag(PUTs, 5),
          lag_7 = lag(PUTs, 7)
        ) |> 
        fill(lag_5, lag_7, .direction = "down")
      
      max_hist_date <- max(ts_data$.date_var)
      future_prepared <- full_ts |> filter(.date_var > max_hist_date)
      
      # 4. Calibración
      window_data <- ts_data |> filter(.date_var <= max_hist_date)
      test_splits <- rsample::initial_time_split(window_data, prop = 0.9)
      test_data <- rsample::testing(test_splits)
      
      model_tbl <- modeltime_table(model_obj$fit)
      
      forecast_tbl <- model_tbl |>
        modeltime_calibrate(new_data = test_data, quiet = TRUE) |>
        modeltime_forecast(new_data = future_prepared, actual_data = ts_data, conf_interval = FALSE) |>
        filter(.key == "prediction")
      
      # 5. Generar intervalos de confianza (80%, 90%, 95%)
      predictions_with_intervals <- calc_multi_intervals(forecast_tbl, model_tbl, test_data)
      
      # 6. Formatear y exportar a Excel
      export_df <- predictions_with_intervals |>
        transmute(
          fecha = .index,
          daypart = matched_daypart,
          age_range = matched_age,
          hour = selected_row$hour,
          prediccion = .value,
          limite_inf_80 = lo_80,
          limite_sup_80 = hi_80,
          limite_inf_90 = lo_90,
          limite_sup_90 = hi_90,
          limite_inf_95 = lo_95,
          limite_sup_95 = hi_95
        )
      
      writexl::write_xlsx(export_df, path = file)
    }
  )
}

shinyApp(ui, server)