
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
      selectInput("age_range", "Age Range:", choices = unique(df$age_range)),
      selectInput("daypart", "Daypart:", choices = unique(df$daypart)),
      pickerInput(
        "hour", "Hour:",
        choices = sort(unique(df$hour)),
        selected = 20,
        multiple = T,
        options = pickerOptions(
          actionsBox = T,
          liveSearch = T,
          size = 8
        )
      ),
      dateRangeInput("date_range", "Date Range:",
                     start = min(df$date),
                     end = max(df$date))
    ),
    mainPanel(
      plotOutput("putPlot"),
      tableOutput("summaryTable")
    )
  )
)

server <- function(input, output, session) {
  filtered <- reactive({
    df %>%
      filter(
        daypart == input$daypart,
        age_range == input$age_range,
        date >= input$date_range[1],
        date <= input$date_range[2]
      )
  })
  
  output$putPlot <- renderPlot({
    ggplot(filtered(), aes(x = date, y = PUTs)) +
      geom_line() +
      labs(title = "PUTs Over Time",
           x = "Date", y = "PUTs")
  })
  
  output$summaryTable <- renderTable({
    filtered() |> 
      summarise(
        mean_PUTs = mean(PUTs, na.rm = TRUE),
        sd_PUTs = sd(PUTs, na.rm = TRUE),
        min_PUTs = min(PUTs, na.rm = TRUE),
        max_PUTs = max(PUTs, na.rm = TRUE)
      )
  })
}

shinyApp(ui, server)