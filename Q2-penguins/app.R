
library(shiny)
library(ggplot2)
library(dplyr)
library(palmerpenguins)


? palmerpenguins

data("penguins")

head (penguins)
glimpse (penguins)


measurements <- c(
  "Bill Length (mm)" = "bill_length_mm",
  "Bill Depth (mm)" = "bill_depth_mm",
  "Flipper Length (mm)" = "flipper_length_mm",
  "Body Mass (g)" = "body_mass_g"
)

ui <- fluidPage(
  
  titlePanel("Palmer Penguins Explorer"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "species",
        "Select penguin species:",
        choices = c(
          "All",
          "Adelie",
          "Chinstrap",
          "Gentoo"
        )
      ),
      
      selectInput(
        "sex",
        "Select sex:",
        choices = c(
          "All",
          "male",
          "female"
        )
      ),
      
      selectInput(
        "xvar",
        "Choose X-axis measurement:",
        choices = measurements,
        selected = "bill_length_mm"
      ),
      
      selectInput(
        "yvar",
        "Choose Y-axis measurement:",
        choices = measurements,
        selected = "flipper_length_mm"
      )
      
    ),
    
    mainPanel(
      plotOutput("scatterplot"),
      textOutput("count")
    )
  )
)






server <- function(input, output) {
    filtered_data <- reactive({
    
    df <- penguins
    
    if (input$species != "All") {
      df <- df |>
        filter(species == input$species)
    }
    
    if (input$sex != "All") {
      df <- df |>
        filter(sex == input$sex)
    }
    
    df
  })
  
  output$scatterplot <- renderPlot({
    
    df <- filtered_data()
    
    validate(
      need(
        any(complete.cases(
          df[, c(input$xvar, input$yvar)]
        )),
        "No complete observations for this selection."
      )
    )
    
    ggplot(
      df,
      aes(
        x = .data[[input$xvar]],
        y = .data[[input$yvar]],
        color = species
      )
    ) +
      geom_point(
        size = 3,
        alpha = 0.7,
        na.rm = TRUE
      ) +
      labs(
        title = "Penguin Measurements",
        x = names(measurements)[
          match(input$xvar, measurements)
        ],
        y = names(measurements)[
          match(input$yvar, measurements)
        ],
        color = "Species"
      ) +
      theme_minimal()
  })
  
  output$count <- renderText({
    paste(
      "Penguins in selected group:",
      nrow(filtered_data())
    )
  })
  
}

shinyApp(ui = ui, server = server)
