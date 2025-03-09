# This is my first shiny app walk-thru

library(shiny)

ui <- fluidPage(
  "Hello Amazing World!"
)
server <- function(input, output, session) {
}
shinyApp(ui, server)