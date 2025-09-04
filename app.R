
pacman::p_load(
  shiny,
  tidyverse,
  arrow,
  plotly,
  shinyWidgets
)

df <- read_parquet("data/raw_data.parquet") |> 
  mutate(
    intervals_dim = ymd_hms(intervals_dim),
    date = as_date(intervals_dim),
    hour = hour(intervals_dim)
  )

ui <- fluidPage(
  
  titlePanel("People Using TV Forecasting"),
  sidebarLayout(
    sidebarPanel(
      dateInput(
        "date", "From:",
        value = min(df$date)
      ),
      selectInput(
        "age_range",
        "Age Range:",
        choices = unique(df$age_range)
      ),
      selectInput(
        "daypart",
        "Daypart:",
        choices = unique(df$daypart)
      ),
      pickerInput(
        "hour", "Hour:",
        choices = NULL,
        selected = NULL,
        multiple = T,
        options = pickerOptions(
          actionsBox = T,
          liveSearch = T,
          size = 8
        )
      )
    ),
    mainPanel(
      plotlyOutput("putPlot"),
      tableOutput("summaryTable")
    )
  )
)

server <- function(input, output, session) {
  
  observe({
    x <- df |> 
      filter(
        daypart == input$daypart,
        age_range == input$age_range,
        date >= input$date
      )
    
    updatePickerInput(
      session,
      "hour",
      choices = sort(unique(x$hour)),
      selected = sort(unique(x$hour))
    )
  })
  
  df1 <- reactive({
    df %>%
      filter(
        daypart == input$daypart,
        age_range == input$age_range,
        hour %in% input$hour
      )
  })
  
  output$putPlot <- renderPlotly({
    ggplotly(
      ggplot(df1(), aes(x = date, y = PUTs)) +
        geom_line() +
        labs(title = "PUTs Over Time",
             x = "Date", y = "PUTs")
    )
  })
  
  output$summaryTable <- renderTable({
    df1() |> 
      summarise(
        mean_PUTs = mean(PUTs, na.rm = TRUE),
        sd_PUTs = sd(PUTs, na.rm = TRUE),
        min_PUTs = min(PUTs, na.rm = TRUE),
        max_PUTs = max(PUTs, na.rm = TRUE)
      )
  })
  
}

shinyApp(ui, server)