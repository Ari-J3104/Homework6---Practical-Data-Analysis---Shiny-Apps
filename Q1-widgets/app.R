
library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Interactive Car Data Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      

      
      selectInput(
        "variable",
        "Choose a variable:",
        choices = c(
          "Miles per Gallon" = "mpg",
          "Horsepower" = "hp",
          "Weight" = "wt"
        )
      ),
      


      sliderInput(
        "bins",
        "Number of histogram bins:",
        min = 5,
        max = 20,
        value = 10
      ),
      
     
      checkboxInput(
        "density",
        "Show density curve",
        value = FALSE
      ),
      

      radioButtons(
        "color",
        "Choose histogram color:",
        choices = c(
          "Blue" = "steelblue",
          "Green" = "seagreen",
          "Orange" = "orange"
        )
      ),
      
      
      numericInput(
        "n",
        "Number of cars to include:",
        value = 32,
        min = 5,
        max = 32
      )
      
    ),
    
    mainPanel(
      plotOutput("histogram"),
      textOutput("summary")
    )
  )
)

server <- function(input, output) {
  
  selected_data <- reactive({
    n <- max(5, min(32, input$n))
    mtcars[seq_len(n), , drop = FALSE]
  })
  
  output$histogram <- renderPlot({
    
    data <- selected_data()
    
    p <- ggplot(
      data,
      aes(x = .data[[input$variable]])
    ) +
      geom_histogram(
        aes(y = after_stat(density)),
        bins = input$bins,
        fill = input$color,
        color = "white"
      ) +
      labs(
        title = "Distribution of Car Measurements",
        x = input$variable,
        y = "Density"
      ) +
      theme_minimal()
    
    if (input$density) {
      p <- p +
        geom_density(
          color = "red",
          linewidth = 1
        )
    }
    
    p
  })
  
  output$summary <- renderText({
    paste(
      "Number of cars displayed:",
      nrow(selected_data())
    )
  })
  
}

shinyApp(ui = ui, server = server)
