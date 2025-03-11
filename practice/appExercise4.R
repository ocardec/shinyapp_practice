# r Shiny

library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Explore ggplot2 Datasets"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dataset", "Select a dataset:", 
                  choices = c("economics", "faithfuld", "seals"))
    ),
    
    mainPanel(
      verbatimTextOutput("summary"),
      plotOutput("plot")
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive expression to load the selected dataset
  dataset <- reactive({
    get(input$dataset, "package:ggplot2")
  })
  
  # Render summary of the dataset
  output$summary <- renderPrint({
    summary(dataset())
  })
  
  # Render a simple plot based on dataset type
  output$plot <- renderPlot({
    data <- dataset()
    
    if (input$dataset == "economics") {
      ggplot(data, aes(x = date, y = unemploy)) +
        geom_line(color = "blue") +
        labs(title = "Unemployment Over Time", x = "Date", y = "Unemployed People")
      
    } else if (input$dataset == "faithfuld") {
      ggplot(data, aes(x = waiting, y = eruptions, fill = density)) +
        geom_tile() +
        labs(title = "Old Faithful Eruptions", x = "Waiting Time", y = "Eruption Duration")
      
    } else if (input$dataset == "seals") {
      ggplot(data, aes(x = long, y = lat)) +
        geom_point(color = "red") +
        labs(title = "Seal Locations", x = "Longitude", y = "Latitude")
    }
  })
}

shinyApp(ui, server)




