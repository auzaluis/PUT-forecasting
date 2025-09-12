pacman::p_load(
  shiny,
  tidyverse,
  modeltime,
  tidymodels,
  arrow,
  plotly,
  shinyWidgets,
  DT,
  lmtest
)

# Modules
source("scripts/preprocess.R")
source("scripts/features.R")
source("scripts/future_ts.R")
source("scripts/utils.R")

# Load raw_data
models_path <- "data/models"
model_files <- list.files(models_path, pattern = "^arimax_.*\\.rds$", full.names = TRUE)

raw_data_path <- "data/raw_data.parquet"
df <- load_data(raw_data_path)

# Lookup for models
model_lookup <- setNames(model_files, gsub("arimax_|\\.rds", "", basename(model_files)))

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
        choices = unique(df$daypart)[2], # limit to total_day for simplicity
        selected = unique(df$daypart)[2]
      ),
      pickerInput(
        "hours", "Hours:",
        choices = sort(unique(df$hour)),
        selected = 20
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
            tabPanel("Metrics Table", DT::dataTableOutput("mape_table")),
            tabPanel("Metrics Distribution", 
                     br(),
                     selectInput(
                       "metric_hist_input",
                       "Select metric:",
                       choices = c(
                         "MAPE" = "mape",
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
    model_file <- model_lookup[[key]]
    if (is.null(model_file) || !file.exists(model_file)) {
      showNotification("No hay modelo pre-entrenado para esta combinación.", type = "error")
      return(NULL)
    }
    readRDS(model_file)
  })
  
  arimax_fit <- reactive({
    arimax()$fit
  })
  
  arimax_model_tbl <- reactive({
    modeltime_table(arimax_fit())
  })
  
  # Plots
  output$forecastPlot <- renderPlotly({
    splits <- arimax()$splits
    ts_data <- ts()
    ggplotly(
      arimax_model_tbl() |>
        modeltime_calibrate(new_data = testing(splits)) |>
        modeltime_forecast(
          new_data = testing(splits),
          actual_data = ts_data,
          conf_interval = F
        ) |>
        plot_modeltime_forecast(.interactive = F) +
        labs(title = NULL)
    ) |> 
      layout(
        legend = list(
          orientation = "h",
          x = 0,
          y = 1.1,
          xanchor = "left",
          yanchor = "top"
        )
      )
  })
  
  output$refit_forecastPlot <- renderPlotly({
    splits <- arimax()$splits
    ts_data <- ts()
    future <- generate_future_ts(ts() |> as_tibble(), input$hours, superbowl_dates)
    new_data <- bind_rows(testing(splits), future)
    ggplotly(
      arimax_model_tbl() |>
        modeltime_calibrate(new_data, quiet = F) |>
        modeltime_forecast(
          new_data = new_data,
          actual_data = ts_data,
          conf_interval = F
        ) |>
        plot_modeltime_forecast(.interactive = F) +
        labs(title = NULL)
    ) |> 
      layout(
        legend = list(
          orientation = "h",
          x = 0,
          y = 1.1,
          xanchor = "left",
          yanchor = "top"
        )
      )
  })
  
  output$printFit <- renderPrint({
    arimax_fit() |>
      extract_fit_parsnip()
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
    
    resid <- 
      arimax_model_tbl() |>
      modeltime_calibrate(new_data = testing(splits)) |>
      modeltime_residuals() |> pull()
    
    plot(acf(resid, lag = 7, main = NULL))
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
  
  output$metric_hist <- renderPlotly({
    metric <- input$metric_hist_input
    df <- all_metrics()
    ggplotly(
      ggplot(df, aes(x = .data[[metric]])) +
        geom_histogram(fill = "#0073C2FF", color = "white", bins = 20) +
        labs(
          x = names(which(c(
            mape = "MAPE",
            norm_pvalue = "Normalidad (Shapiro p-value)",
            homo_pvalue = "Homocedasticidad (BP p-value)",
            ac_pvalue = "Autocorrelación (Ljung-Box p-value)"
          ) == metric)),
          y = "Frecuencia"
        ) +
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
        facet_wrap(~date) +
        labs(
          x = "Hour",
          y = names(which(c(
            mape = "MAPE",
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