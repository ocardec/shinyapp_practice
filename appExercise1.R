# r Shiny

library(shiny)

ui <- fluidPage(
  titlePanel("Personalized Greeting & Dataset Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("name", "What's your name?"),
      numericInput("age", "How old are you?", value = NA),
      textOutput("greeting"),
      
      selectInput(
        inputId = "dataset",
        label = "Choose a dataset",
        choices = ls("package:datasets")
      )
    ),
    
    mainPanel(
      verbatimTextOutput("summary"),
      tableOutput("table"),
      plotOutput("histogram")
    )
  )
)

server <- function(input, output, session) {
  
  # Greeting message
  output$greeting <- renderText({
    req(input$name)  # Ensure name is entered
    paste0("Hello ", input$name, "!")
  })
  
  # Load selected dataset
  dataset <- reactive({
    get(input$dataset, "package:datasets")
  })
  
  # Display summary of dataset
  output$summary <- renderPrint({
    summary(dataset())
  })
  
  # Display dataset as a table
  output$table <- renderTable({
    dataset()
  })
  
  # Generate histogram
  output$histogram <- renderPlot({
    hist(rnorm(1000), main = "Histogram of Random Normal Data", col = "blue", border = "white")
  }, res = 96)
}

shinyApp(ui = ui, server = server)

