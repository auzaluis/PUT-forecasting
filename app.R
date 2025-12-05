pacman::p_load(
  shiny,
  tidyverse,
  modeltime,
  tidymodels,
  arrow,
  plotly,
  shinyWidgets,
  DT,
  lmtest,
  timetk,
  writexl
)

# Modules
source("scripts/preprocess.R")
source("scripts/features.R")
source("scripts/future_ts.R")
source("scripts/utils.R")

# Load raw_data
models_path <- "data/models"
model_files_arimax <- list.files(models_path, pattern = "(^arimax_.*)\\.rds$", full.names = TRUE)
model_files_glmnet <- list.files(models_path, pattern = "(^glmnet_.*)\\.rds$", full.names = TRUE)
model_files<-c(model_files_arimax,model_files_glmnet)

raw_data_path <- "data/raw_data.parquet"
df <- load_data(raw_data_path) |>
  filter(daypart=="total_day")

# Lookup for models
model_lookup_arimax <- setNames(model_files_arimax, gsub("arimax_|\\.rds", "", basename(model_files_arimax)))
model_lookup_glmnet <- setNames(model_files_glmnet, gsub("glmnet_|\\.rds", "", basename(model_files_glmnet)))

# date input values
date_values <- as.Date(c("2022-01-01", "2023-01-01"))

# UI
ui <- fluidPage(
  
  titlePanel("People Using TV Forecasting"),
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        "date", "From",
        choices = unique(date_values),
        selected = unique(date_values)[2]
      ),
      pickerInput(
        "age_range", "Age Range:",
        choices = unique(df$age_range)
      ),
      pickerInput(
        "daypart", "Daypart:",
        choices = unique(df$daypart)[1], # limit to total_day for simplicity
        selected = unique(df$daypart)[1]
      ),
      pickerInput(
        "hours", "Hours:",
        choices = sort(unique(df$hour)),
        selected = 20
      ),
      pickerInput(
        "conf_level", "Nivel de intervalo:",
        choices = c(80, 90, 95),
        selected = 95
      )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Chart",
          plotlyOutput("forecastPlot")
        ),
        tabPanel(
          "Long-term",
          plotlyOutput("refit_forecastPlot")
        ),
        tabPanel(
          "Summary",
          verbatimTextOutput("printFit")
        ),
        tabPanel(
          "Accuracy",
          verbatimTextOutput("printAccurary")
        ),
        tabPanel(
          "Residuals", br(),
          tabsetPanel(
            tabPanel("Time Plot", plotlyOutput("timeplot_resid")),
            tabPanel("ACF", plotOutput("acf_resid"))
          )
        ),
        tabPanel(
          "All models performance", br(),
          tabsetPanel(
            tabPanel("Metrics Table", 
                     DT::dataTableOutput("mape_table"),
                     br(),
                     downloadButton("download_metrics", "📥 Descargar métricas")
            ),
            tabPanel("Metrics Distribution", 
                     br(),
                     selectInput(
                       "metric_hist_input",
                       "Select metric:",
                       choices = c(
                         "MAPE" = "mape",
                         "RMSE" = "rmse",
                         "MSE" = "mse",
                         "MAE" = "mae",
                         "R²" = "rsq",
                         "Normalidad (Shapiro p-value)" = "norm_pvalue",
                         "Homocedasticidad (BP p-value)" = "homo_pvalue",
                         "Autocorrelación (Ljung-Box p-value)" = "ac_pvalue"
                       ),
                       selected = "mape"
                     ),
                     plotlyOutput("metric_hist")
            ),
            tabPanel("Metrics Facet", 
                     br(),
                     selectInput(
                       "metric_facet_input",
                       "Select metric:",
                       choices = c(
                         "MAPE" = "mape",
                         "RMSE" = "rmse",
                         "MSE" = "mse",
                         "MAE" = "mae",
                         "R²" = "rsq",
                         "Normalidad (Shapiro p-value)" = "norm_pvalue",
                         "Homocedasticidad (BP p-value)" = "homo_pvalue",
                         "Autocorrelación (Ljung-Box p-value)" = "ac_pvalue"
                       ),
                       selected = "mape"
                     ),
                     plotlyOutput("metric_facet")
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  df1 <- reactive({
    preprocess_data(df, input$daypart, input$hours, input$age_range, input$date)
  })
  
  ts <- reactive({
    add_features(df1(), superbowl_dates)
  })
  
  # Initial fitting
  arimax <- reactive({
    key <- get_model_key(input$daypart, input$age_range, input$hours, input$date)
    print(paste("Buscando modelo con clave:", key))
    model_file <- model_lookup_arimax[[key]]
    if (is.null(model_file) || !file.exists(model_file)) {
      showNotification("No hay modelo pre-entrenado para esta combinación.", type = "error")
      return(NULL)
    }
    readRDS(model_file)
  })
  
  glmnet <- reactive({
    key <- get_model_key(input$daypart, input$age_range, input$hours, input$date)
    print(paste("Buscando modelo con clave:", key))
    model_file <- model_lookup_glmnet[[key]]
    if (is.null(model_file) || !file.exists(model_file)) {
      showNotification("No hay modelo pre-entrenado para esta combinación.", type = "error")
      return(NULL)
    }
    readRDS(model_file)
  })
  arimax_fit <- reactive({
    arimax()$fit
  })
  glmnet_fit <- reactive({
    glmnet()$fit
  })
  
  arimax_model_tbl <- reactive({
    modeltime_table(arimax_fit(),glmnet_fit())
  })
  
  # ✅ Función para calcular intervalos manualmente
  calc_manual_intervals <- function(forecast_tbl, level, model_tbl, calibration_data) {
    z <- qnorm((1 + level) / 2)
    
    resid_tbl <- model_tbl |>
      modeltime_calibrate(new_data = calibration_data) |>
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
  
  # ✅ Gráfico Chart
  output$forecastPlot <- renderPlotly({
    splits <- arimax()$splits
    ts_data <- ts()
    
    forecast_tbl <- arimax_model_tbl() |>
      modeltime_calibrate(new_data = testing(splits)) |>
      modeltime_forecast(
        new_data = testing(splits),
        actual_data = ts_data,
        conf_interval = FALSE # Desactivamos intervalos originales
      )
    
    forecast_tbl <- calc_manual_intervals(forecast_tbl, as.numeric(input$conf_level) / 100, arimax_model_tbl(), testing(splits))
    
    forecast_long <- bind_rows(
      forecast_tbl |> mutate(tipo = "Predicción", valor = .value),
      forecast_tbl |> mutate(tipo = "Límite inferior", valor = .conf_lo),
      forecast_tbl |> mutate(tipo = "Límite superior", valor = .conf_hi)
    )
    
    p <- ggplot(forecast_long, aes(x = .index, y = valor,
                                   color = .model_desc, linetype = tipo,
                                   group = interaction(.model_desc, tipo))) +
      geom_line(size = 0.4) +
      scale_color_manual(values = c("darkblue","forestgreen","red")) +
      scale_linetype_manual(values = c("Predicción" = "solid",
                                       "Límite inferior" = "dashed",
                                       "Límite superior" = "dashed")) +
      labs(
        title = paste("Pronóstico con Intervalo", input$conf_level, "%"),
        subtitle = "Líneas sólidas: predicción | Líneas punteadas: límites",
        x = "Fecha", y = "Valor", color = "Modelo", linetype = "Tipo"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 11))
    
    ggplotly(p, tooltip = c("x", "y", ".model_desc", "tipo")) |>
      layout(legend = list(orientation = "h", x = 0, y = -0.2, font = list(size = 12)))
  })
  
  # ✅ Gráfico Long-Term
  output$refit_forecastPlot <- renderPlotly({
    splits <- arimax()$splits
    ts_data <- ts()
    future <- generate_future_ts(ts() |> as_tibble(), input$hours, features)
    new_data <- bind_rows(testing(splits), future)
    
    forecast_tbl <- arimax_model_tbl() |>
      modeltime_calibrate(new_data, quiet = FALSE) |>
      modeltime_forecast(
        new_data = new_data,
        actual_data = ts_data,
        conf_interval = FALSE # Desactivamos intervalos originales
      )
    
    forecast_tbl <- calc_manual_intervals(forecast_tbl, as.numeric(input$conf_level) / 100, arimax_model_tbl(), new_data)
    
    forecast_long <- bind_rows(
      forecast_tbl |> mutate(tipo = "Predicción", valor = .value),
      forecast_tbl |> mutate(tipo = "Límite inferior", valor = .conf_lo),
      forecast_tbl |> mutate(tipo = "Límite superior", valor = .conf_hi)
    )
    
    p <- ggplot(forecast_long, aes(x = .index, y = valor,
                                   color = .model_desc, linetype = tipo,
                                   group = interaction(.model_desc, tipo))) +
      geom_line(size = 0.4) +
      scale_color_manual(values = c("darkblue","forestgreen","red")) +
      scale_linetype_manual(values = c("Predicción" = "solid",
                                       "Límite inferior" = "dashed",
                                       "Límite superior" = "dashed")) +
      labs(
        title = paste("Pronóstico Long-Term con Intervalo", input$conf_level, "%"),
        subtitle = "Líneas sólidas: predicción | Líneas punteadas: límites",
        x = "Fecha", y = "Valor", color = "Modelo", linetype = "Tipo"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom",
            legend.text = element_text(size = 10),
            legend.title = element_text(size = 11))
    
    ggplotly(p, tooltip = c("x", "y", ".model_desc", "tipo")) |>
      layout(legend = list(orientation = "h", x = 0, y = -0.2, font = list(size = 12)))
  })
  
  
  output$printFit <- renderPrint({
    cat("🔹 Modelo ARIMAX:\n")
    print(arimax_fit() |> extract_fit_parsnip())
    
    cat("\n\n🔹 Modelo GLMNET:\n")
    print(glmnet_fit() |> extract_fit_parsnip()|> 
            tidy(),n=24)
  })
  
  output$printAccurary <- renderPrint({
    splits <- arimax()$splits
    arimax_model_tbl() |>
      modeltime_accuracy(testing(splits))
  })
  
  output$timeplot_resid <- renderPlotly({
    splits <- arimax()$splits
    ggplotly(
      arimax_model_tbl() |>
        modeltime_calibrate(new_data = testing(splits)) |>
        modeltime_residuals() |> 
        plot_modeltime_residuals(.type = "timeplot", .interactive = F) +
        labs(title = NULL) +
        theme(legend.position = "none")
    )
  })
  
  output$acf_resid <- renderPlot({
    splits <- arimax()$splits 
    calibrated_tbl <- arimax_model_tbl() |>
      modeltime_calibrate(new_data = testing(splits))
    
    residuals_tbl <- modeltime_residuals(calibrated_tbl)
    
    model_ids <- unique(residuals_tbl$.model_id)
    model_descs <- unique(residuals_tbl$.model_desc)
    
    par(mfrow = c(1,length(model_ids)))  
    
    # Graficar ACF
    for (i in seq_along(model_ids)) {
      resids <- residuals_tbl |>
        filter(.model_id == model_ids[i]) |>
        pull(.residuals)
      
      acf(resids, main = paste("ACF -", model_descs[i]))
    }
  })
  
  # Metrics for all models
  all_metrics <- reactive({
    map_dfr(model_files, function(f) {
      
      fit <- readRDS(f)
      calibrated <- 
        modeltime_table(fit$fit) |>
        modeltime_calibrate(testing(fit$splits), quiet = TRUE)
      
      acc <- modeltime_accuracy(calibrated)
      mape_val <- acc$mape |> round(1)
      rmse_val <- acc$rmse |> round(1)
      mse_val  <- (acc$rmse)^2 |> round(1)
      rsq_val  <- acc$rsq  |> round(2)
      mae_val  <- acc$mae  |> round(1)
      
      resids <- modeltime_residuals(calibrated)$.residuals
      preds <- modeltime_residuals(calibrated)$.prediction
      
      norm_pvalue <-
        tryCatch(shapiro.test(resids)$p.value, error = function(e) NA) |>
        round(2)
      
      homo_pvalue <-
        tryCatch({
          lm_mod <- lm(resids^2 ~ seq_along(resids))
          lmtest::bptest(lm_mod)$p.value |> round(2)
        }, error = function(e) NA)
      
      ac_pvalue <-
        tryCatch(
          Box.test(x = resids, lag = 7, type = "Ljung-Box")$p.value,
          error = function(e) NA
        ) |>
        round(2)
      
      parse_model_filename(basename(f)) |>
        mutate(
          mape = mape_val,
          rmse = rmse_val,
          mse = mse_val,
          rsq = rsq_val,
          mae = mae_val,
          norm_pvalue = norm_pvalue,
          homo_pvalue = homo_pvalue,
          ac_pvalue = ac_pvalue
        )
      
    })
  })
  
  output$mape_table <- DT::renderDataTable({
    all_metrics()
  }, options = list(
    pageLength = 10,
    orderClasses = T,
    dom = 'tp',
    columnDefs = list(list(className = 'dt-left', targets = "_all"))
  ), rownames = F)
  output$download_metrics <- downloadHandler(
    filename = function() {
      paste("metricas_modelos_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      writexl::write_xlsx(all_metrics(), path = file)
    }
  )
  
  output$metric_hist <- renderPlotly({
    metric <- input$metric_hist_input
    df <- all_metrics()
    ggplotly(
      ggplot(df, aes(x = .data[[metric]])) +
        geom_histogram(fill = "#0073C2FF", color = "white", bins = 20) +
        labs(
          x = names(which(c(
            mape = "MAPE",
            rmse = "RMSE",
            mse = "MSE",
            mae = "MAE",
            rsq = "R²",
            norm_pvalue = "Normalidad (Shapiro p-value)",
            homo_pvalue = "Homocedasticidad (BP p-value)",
            ac_pvalue = "Autocorrelación (Ljung-Box p-value)"
          ) == metric)),
          y = "Frecuencia"
        ) +facet_wrap(model~date)+
        theme_minimal()
    )
  })
  
  output$metric_facet <- renderPlotly({
    metric <- input$metric_facet_input
    df <- all_metrics()
    ggplotly(
      ggplot(
        all_metrics(),
        aes(
          x = hour,
          y = .data[[metric]],
          color = age_range,
          group = age_range,
          text = paste(
            toupper(metric), ": ", .data[[metric]],
            "<br>Age Range:", age_range,
            "<br>Date:", date,
            "<br>Hour:", hour
          )
        )
      ) +
        geom_point(size = 3) +
        geom_line() +
        facet_wrap(model~date) +
        labs(
          x = "Hour",
          y = names(which(c(
            mape = "MAPE",
            rmse = "RMSE",
            mse = "RMSE",
            mae = "MAE",
            rsq = "R²",
            norm_pvalue = "Normalidad (Shapiro p-value)",
            homo_pvalue = "Homocedasticidad (BP p-value)",
            ac_pvalue = "Autocorrelación (Ljung-Box p-value)"
          ) == metric)),
          color = "Age Range"
        ) +
        theme_minimal(),
      tooltip = "text"
    ) |>
      layout(
        legend = list(
          orientation = "h",
          x = 0,
          y = 1.25,
          xanchor = "left",
          yanchor = "top"
        )
      )
  })
  
}

shinyApp(ui, server)