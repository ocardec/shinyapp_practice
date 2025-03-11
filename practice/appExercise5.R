library(shiny)
library(ggplot2)
library(FactoMineR)
library(factoextra)

ui <- fluidPage(
  titlePanel("Exploration of Datasets with PCA & ggplot Visualization"),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,  # Left panel covers 33% of width
      selectInput("dataset", "Select a dataset:", 
                  choices = c("economics", "faithfuld", "seals"))
    ),
    
    mainPanel(
      width = 8,  # Right panel covers 66% of width
      
      fluidRow(
        column(6, verbatimTextOutput("desc_summary")),  # Descriptive summary
        column(6, verbatimTextOutput("summary"))       # Dataset summary
      ),
      
      fluidRow(
        column(6, verbatimTextOutput("pca_summary")),  # PCA summary
        column(6, plotOutput("eigen_plot"))           # Eigenvector plot
      ),
      
      fluidRow(
        column(12, plotOutput("plot"))  # Data visualization
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive dataset selection
  dataset <- reactive({
    get(input$dataset, "package:ggplot2")
  })
  
  # Descriptive Summary
  output$desc_summary <- renderPrint({
    data <- dataset()
    cat("Dataset Name:", input$dataset, "\n")
    cat("Number of Rows:", nrow(data), "\n")
    cat("Number of Columns:", ncol(data), "\n")
    cat("Column Names:", paste(names(data), collapse = ", "), "\n")
  })
  
  # Summary statistics of dataset
  output$summary <- renderPrint({
    summary(dataset())
  })
  
  # Perform PCA and summarize results
  pca_result <- reactive({
    data <- dataset()
    
    # Select only numeric columns
    num_data <- data[sapply(data, is.numeric)]
    
    # Ensure at least two numeric variables exist for PCA
    if (ncol(num_data) >= 2) {
      PCA(num_data, graph = FALSE)
    } else {
      return(NULL)
    }
  })
  
  # Render PCA summary
  output$pca_summary <- renderPrint({
    pca <- pca_result()
    if (!is.null(pca)) {
      summary(pca)
    } else {
      "PCA cannot be performed due to insufficient numeric variables."
    }
  })
  
  # Render Eigenvalues plot
  output$eigen_plot <- renderPlot({
    pca <- pca_result()
    if (!is.null(pca)) {
      fviz_eig(pca, addlabels = TRUE, main = "Eigenvalues of Principal Components")
    }
  })
  
  # Render a ggplot visualization based on dataset
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





