# r Shiny

library(shiny)

ui <- fluidPage(
  sliderInput("x", label = "Choose x:", min = 1, max = 50, value = 10),
  sliderInput("y", label = "Choose y:", min = 1, max = 50, value = 5),
  "Then x * y is:", 
  textOutput("product"),
  "and, (x * y) + 5 is", textOutput("product_plus5"), 
  "and (x * y) + 10 is", textOutput("product_plus10")
)

server <- function(input, output, session) {
  output$product <- renderText({
    product <- input$x * input$y  
    product
  })
  output$product_plus5 <- renderText({
    product <- input$x * input$y  
    product + 5
  })
  output$product_plus10 <- renderText({
    product <- input$x * input$y  
    product + 10
  })
  
  
}

shinyApp(ui, server)



