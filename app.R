pacman::p_load(
  shiny,
  tidyverse,
  arrow,
  plotly,
  shinyWidgets
)

# Modules
source("scripts/preprocess.R")
source("scripts/features.R")
source("scripts/train_arimax.R")

# Load raw_data
path <- "data/raw_data.parquet"
df <- load_data(path)

# UI
ui <- fluidPage(
  
  titlePanel("People Using TV Forecasting"),
  sidebarLayout(
    sidebarPanel(
      dateInput(
        "date", "From:",
        value = min(df$.date_var, na.rm = T)
      ),
      pickerInput(
        "age_range",
        "Age Range:",
        choices = unique(df$age_range)
      ),
      pickerInput(
        "daypart",
        "Daypart:",
        choices = unique(df$daypart),
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
          # plotlyOutput("putPlot")
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

  # observeEvent(input$daypart, {
  #   x <- df |>
  #     filter(daypart == input$daypart)
  #   
  #   hours_choices <- sort(unique(x$hour))
  # 
  #   updatePickerInput(
  #     session,
  #     "hours",
  #     choices = sort(unique(x$hour)),
  #     selected = sort(unique(x$hour))[1]
  #   )
  # })
  
  df1 <- reactive({
    preprocess_data(df, input$daypart, input$hours, input$age_range, input$date)
  })
  
  ts <- reactive({
    add_features(df1(), superbowl_dates)
  })

  arimax <- reactive({
    train_arimax(ts())
  })
  
  fit_arimax <- reactive({
    arimax()$fit
  })
  
  model_tbl_arimax <- reactive({
    modeltime_table(fit_arimax())
  })
  
  output$forecastPlot <- renderPlotly({
    splits <- arimax()$splits
    ts_data <- ts()
    ggplotly(
      model_tbl_arimax() |>
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
  
  output$putPlot <- renderPlotly({
    ggplotly(
      ggplot(df1(), aes(x = .date_var, y = PUTs)) +
        geom_line() +
        labs(title = "PUTs Over Time", x = "", y = "")
    )
  })
  
  output$printFit <- renderPrint({
    fit_arimax() |>
      extract_fit_parsnip()
  })
  
  output$printAccurary <- renderPrint({
    splits <- arimax()$splits
    model_tbl_arimax() |>
      modeltime_accuracy(testing(splits))
  })
  
  output$timeplot_resid <- renderPlotly({
    splits <- arimax()$splits
    ggplotly(
      model_tbl_arimax() |>
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
      model_tbl_arimax() |>
        modeltime_calibrate(new_data = testing(splits)) |>
        modeltime_residuals() |> 
        plot_modeltime_residuals(.type = "acf", .interactive = F) +
        labs(title = NULL) +
        theme(legend.position = "none")
    )
  })
  
}

shinyApp(ui, server)