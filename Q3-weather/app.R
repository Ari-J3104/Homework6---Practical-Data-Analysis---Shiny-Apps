
library(shiny)
library(ggplot2)
library(dplyr)
library(macleish)

data("whately_2015")
data("orchard_2015")

weather_vars <- c(
  "Temperature (C)" = "temperature",
  "Wind Speed (m/s)" = "wind_speed",
  "Relative Humidity" = "rel_humidity",
  "Rainfall (mm)" = "rainfall",
  "Pressure (mb)" = "pressure"
)

ui <- fluidPage(
  
  titlePanel("MacLeish Weather Explorer"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "station",
        "Select weather station:",
        choices = c(
          "Whately" = "whately",
          "Orchard" = "orchard"
        )
      ),
      
      dateRangeInput(
        "dates",
        "Select date range:",
        start = "2015-06-01",
        end = "2015-06-07",
        min = "2015-01-01",
        max = "2015-12-31"
      ),
      
      selectInput(
        "variable",
        "Select weather measurement:",
        choices = weather_vars,
        selected = "temperature"
      )
      
    ),
    
    mainPanel(
      plotOutput("weatherplot"),
      textOutput("observations")
    )
  )
)

# Server
server <- function(input, output) {
  
  # Filter based on station and dates
  filtered_weather <- reactive({
    
    if (input$station == "whately") {
      df <- whately_2015
    } else {
      df <- orchard_2015
    }
    
    req(input$dates)
    
    df |>
      filter(
        as.Date(when) >= input$dates[1],
        as.Date(when) <= input$dates[2]
      )
  })
  
  # Generate time series
  output$weatherplot <- renderPlot({
    
    df <- filtered_weather()
    
    validate(
      need(nrow(df) > 0, "No data in this date range.")
    )
    
    ggplot(
      df,
      aes(
        x = when,
        y = .data[[input$variable]]
      )
    ) +
      geom_line(
        color = "steelblue",
        linewidth = 0.5,
        na.rm = TRUE
      ) +
      labs(
        title = paste(
          "Weather at",
          input$station,
          "station"
        ),
        x = "Date and Time",
        y = names(weather_vars)[
          match(input$variable, weather_vars)
        ]
      ) +
      theme_minimal()
  })
  
  output$observations <- renderText({
    paste(
      "Number of weather records:",
      nrow(filtered_weather())
    )
  })
  
}

shinyApp(ui = ui, server = server)
