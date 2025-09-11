pacman::p_load(
  shiny,
  tidyverse,
  modeltime,
  tidymodels,
  arrow,
  plotly,
  shinyWidgets
)

# Modules
source("scripts/preprocess.R")
source("scripts/features.R")
source("scripts/future_ts.R")

# Load raw_data
models_path <- "data/models"
model_files <- list.files(models_path, pattern = "^arimax_.*\\.rds$", full.names = TRUE)

raw_data_path <- "data/raw_data.parquet"
df <- load_data(raw_data_path)

# clean_name function
clean_name <- function(x) {
  x |> 
    str_replace_all("[^A-Za-z0-9]", "_") |> 
    str_replace_all("_+", "_") |> 
    str_replace_all("^_|_$", "")
}

# get_model_key function
get_model_key <- function(daypart, age_range, hour, date) {
  paste(
    clean_name(as.character(daypart)),
    clean_name(as.character(age_range)),
    clean_name(as.character(hour)),
    clean_name(as.character(date)),
    sep = "_"
  )
}

date_values <- as.Date(c("2022-01-01", "2023-01-01"))

# Lookup for models
model_lookup <- setNames(model_files, gsub("arimax_|\\.rds", "", basename(model_files)))

# UI
ui <- fluidPage(
  
  titlePanel("People Using TV Forecasting"),
  sidebarLayout(
    sidebarPanel(
      pickerInput(
        "date",
        "From",
        choices = unique(date_values)
      ),
      pickerInput(
        "age_range",
        "Age Range:",
        choices = unique(df$age_range)
      ),
      pickerInput(
        "daypart",
        "Daypart:",
        choices = unique(df$daypart)[2], # limit to total_day for simplicity
        selected = "total_day"
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
          "Residuals",
          fluidRow(column(12, plotlyOutput("timeplot_resid"))),
          fluidRow(column(12, plotlyOutput("acf_resid")))
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
        labs(title = NULL) +
        theme(legend.position = "none")
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
        labs(title = NULL) +
        theme(legend.position = "none")
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
  
  output$acf_resid <- renderPlotly({
    splits <- arimax()$splits
    ggplotly(
      arimax_model_tbl() |>
        modeltime_calibrate(new_data = testing(splits)) |>
        modeltime_residuals() |> 
        plot_modeltime_residuals(.type = "acf", .interactive = F) +
        labs(title = NULL) +
        theme(legend.position = "none")
    )
  })
  
}

shinyApp(ui, server)