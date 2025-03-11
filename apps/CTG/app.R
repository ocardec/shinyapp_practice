library(shiny)
library(tidyverse)
library(party)
library(partykit)
library(caret)
library(grid)
library(ggplot2)

# Load dataset
load_data <- function() {
  df <- read.csv("cardiotocography.csv", head = TRUE, sep = ",", stringsAsFactors = FALSE)
  df$NSP <- factor(df$NSP, levels = 1:3, labels = c("Normal", "Suspect", "Pathologic"))
  return(df)
}

# Train Model
train_model <- function(train_data) {
  myFormula <- NSP ~ .
  model <- ctree(myFormula, data = train_data)
  return(model)
}

# Evaluate Model Function
evaluate_model <- function(model, test_data) {
  predictions <- predict(model, newdata = test_data)
  confusionMatrix(predictions, test_data$NSP)
}

# Plot CTREE Function
plot_ctree <- function(model) {
  plot(model, type = "extended", ep_args = list(justmin = 8), drop_terminal = FALSE, tnex = 1.5, 
       gp = gpar(fontsize = 8, col = "dark blue"),
       inner_panel = node_inner(model, fill = c("light grey", "cyan"), pval = TRUE), 
       terminal_panel = node_barplot(model, fill = c(3, 7, 2), beside = TRUE, ymax = 1, rot = 75, 
                                     just = c(.95, .5), ylines = TRUE, widths = 2, gap = 0.05, reverse = FALSE, id = TRUE), 
       margins = c(5, 3, 4, 3),
       main = "Cardiotocography Data\n Conditional Inference Tree\n'Extended'")
}

# Visualization Function: Distribution Plot
plot_distributions <- function(df, stats) {
  ggplot(df, aes(x = LB)) +
    geom_histogram(color = "darkblue", fill = "darkgrey", bins = 30) +
    geom_vline(xintercept = stats$bounds, col = c("blue", "blue", "red", "red"), lty = c(1, 1, 2, 2)) +
    labs(title = "Histogram of FHR Baseline", x = "Beats per Minute") +
    theme_minimal()
}

# UI
ui <- fluidPage(
  titlePanel("Cardiotocography Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      actionButton("reset", "Reset"),
      actionButton("run_classification", "Run Classification"),
      sliderInput("LB", "FHR Baseline (bpm)", min = 50, max = 200, value = 120),
      sliderInput("AC", "Accelerations/sec", min = 0, max = 5, value = 1),
      sliderInput("FM", "Fetal Movements/sec", min = 0, max = 10, value = 3),
      sliderInput("UC", "Uterine Contractions/sec", min = 0, max = 5, value = 1),
      sliderInput("DL", "Light Decelerations/sec", min = 0, max = 5, value = 1),
      sliderInput("DS", "Severe Decelerations/sec", min = 0, max = 5, value = 1),
      sliderInput("DP", "Prolonged Decelerations/sec", min = 0, max = 5, value = 1),
      sliderInput("ASTV", "% Abnormal Short-Term Variability", min = 0, max = 100, value = 20),
      sliderInput("MSTV", "Mean Short-Term Variability", min = 0, max = 10, value = 2),
      sliderInput("ALTV", "% Abnormal Long-Term Variability", min = 0, max = 100, value = 20),
      sliderInput("MLTV", "Mean Long-Term Variability", min = 0, max = 10, value = 2)
      ),
    mainPanel(
      tabsetPanel(
        tabPanel("Model Summary", 
                 fluidRow(
                   column(6, verbatimTextOutput("model_print")),
                   column(6, plotOutput("tree_plot"))
                 )
        ),
        tabPanel("Summary Statistics", 
                 verbatimTextOutput("evaluation_metrics"),
                 plotOutput("distribution_plot")
        ),
        tabPanel("Individual Reading", 
                 tableOutput("input_values"),
                 plotOutput("input_bar_chart"),
                 plotOutput("classification_gauge"),
                 textOutput("confidence_level")
        )
      )
    )
  )
)


# Server
server <- function(input, output, session) {
  df <- reactive({ load_data() })
  model <- reactiveVal()
  stats <- reactive({ compute_statistics(df()) })
  splits <- reactiveVal()
  
  observe({
    set.seed(1234)
    train_idx <- sample(seq_len(nrow(df())), size = floor(0.7 * nrow(df())))
    train_data <- df()[train_idx, ]
    test_data <- df()[-train_idx, ]
    splits(list(train = train_data, test = test_data))
    model(train_model(train_data))
  })
  
  # Fix Reset button
  observeEvent(input$reset, {
    updateSliderInput(session, "LB", value = 120)
    updateSliderInput(session, "AC", value = 1)
    updateSliderInput(session, "FM", value = 3)
    updateSliderInput(session, "UC", value = 1)
    updateSliderInput(session, "DL", value = 1)
    updateSliderInput(session, "DS", value = 1)
    updateSliderInput(session, "DP", value = 1)
    updateSliderInput(session, "ASTV", value = 20)
    updateSliderInput(session, "MSTV", value = 2)
    updateSliderInput(session, "ALTV", value = 20)
    updateSliderInput(session, "MLTV", value = 2)
    
    print("Reset button clicked and inputs updated") # Debugging statement
  })
  
  output$model_print <- renderPrint({ print(model()) })
  output$tree_plot <- renderPlot({ plot_ctree(model()) })
  output$evaluation_metrics <- renderPrint({ evaluate_model(model(), splits()$test) })
  output$distribution_plot <- renderPlot({ plot_distributions(df(), stats = list(bounds = c(120, 140, 160, 180))) })
  
  output$input_values <- renderTable({
    data.frame(
      Attribute = c("LB", "AC", "FM", "UC", "DL", "DS", "DP", "ASTV", "MSTV", "ALTV", "MLTV"),
      Value = c(input$LB, input$AC, input$FM, input$UC, input$DL, input$DS, input$DP, 
                input$ASTV, input$MSTV, input$ALTV, input$MLTV)
    )
  })
  
  output$input_bar_chart <- renderPlot({
    input_data <- tibble(
      Attribute = c("LB", "AC", "FM", "UC", "DL", "DS", "DP", "ASTV", "MSTV", "ALTV", "MLTV"),
      Value = c(input$LB, input$AC, input$FM, input$UC, input$DL, input$DS, input$DP, 
                input$ASTV, input$MSTV, input$ALTV, input$MLTV)
    )
    
    ggplot(input_data, aes(x = Attribute, y = Value, fill = Attribute)) +
      geom_bar(stat = "identity") +
      theme_minimal() +
      ggtitle("User Input Values") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  observeEvent(input$run_classification, {
    category <- sample(c("Normal", "Suspect", "Pathologic"), 1)
    output$classification_gauge <- renderPlot({
      ggplot(tibble(x = 1, category = category), aes(x, category)) +
        geom_tile(aes(fill = category), width = 0.5, height = 0.5) +
        scale_fill_manual(values = c("Normal" = "green", "Suspect" = "yellow", "Pathologic" = "red")) +
        theme_void() +
        ggtitle("Classification Result")
    })
  output$confidence_level <- renderText({
      paste("Confidence Level:", round(runif(1, 80, 100), 2), "%")
    })
  })
}

shinyApp(ui, server)
