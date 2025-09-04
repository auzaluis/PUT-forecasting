
pacman::p_load(
  shiny,
  tidyverse,
  arrow,
  plotly,
  shinyWidgets
)

# Modules
source("scripts/preprocess.R")

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
        "hours", "Hours:",
        choices = sort(unique(df$hour)),
        selected = sort(unique(df$hour)),
        # choices = NULL,
        # selected = NULL,
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

# Server
server <- function(input, output, session) {

  observe({
    x <- df |> 
      filter(daypart == input$daypart)
    
    updatePickerInput(
      session,
      "hours",
      choices = sort(unique(x$hour)),
      selected = sort(unique(x$hour))
    )
  })
  
  df1 <- reactive({
    preprocess_data(df, input$daypart, input$hours, input$age_range, input$date)
  })
  
  output$putPlot <- renderPlotly({
    ggplotly(
      ggplot(df1(), aes(x = .date_var, y = PUTs)) +
        geom_line() +
        labs(title = "PUTs Over Time", x = "", y = "")
    )
  })
  
  output$summaryTable <- renderTable({
    df1() |> 
      summarise(
        mean_PUTs = mean(PUTs, na.rm = T),
        sd_PUTs = sd(PUTs, na.rm = T),
        min_PUTs = min(PUTs, na.rm = T),
        max_PUTs = max(PUTs, na.rm = T)
      )
  })
  
}

shinyApp(ui, server)