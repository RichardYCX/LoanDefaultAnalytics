# Import libraries
library(shiny)
library(data.table)
library(rpart)
library(rpart.plot)

# Read in the RF model
model <- readRDS("model.rds")

####################################
# User interface                   #
####################################

ui <- pageWithSidebar(
  
  # Page header
  headerPanel('Loan Default Predictor'),
  
  # Input values
  sidebarPanel(
    HTML("<h3>Input parameters</h4>"),
    sliderInput("avg_fico_range_high", label = "avg fico score", value = 700,
                min = 0,
                max = 850),
    sliderInput("avg_num_rev_accts", label = "number of revolving accounts", value = 5,
                min = 1,
                max = 101),
    sliderInput("bc_open_to_buy", label = "total open to buy on revolving bankcards", value = 20000,
                min = 0,
                max = 559912),
    sliderInput("combined_dti", label = "combined debt to income ratio", value = 10,
                min = 0,
                max = 63.66),
    sliderInput("loan_amnt", label = "loan amount", value = 10000,
                min = 1000,
                max = 40000),
    selectInput("term", label = "term length", choices = c('36 months', '60 months')),
    sliderInput("total_acc ", label = "total number of credit lines available", value = 10,
                min = 2,
                max = 169),
    
    actionButton("submitbutton", "Submit", class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
  )
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    test <- TrainSet[2]
    test[, ":="("avg_fico_range_high" = input$avg_fico_range_high,
                "avg_num_rev_accts" = input$avg_num_rev_accts,
                "bc_open_to_buy" = input$bc_open_to_buy,
                "combined_dti" = input$combined_dti,
                "loan_amnt" = input$loan_amnt,
                "term" = input$term,
                "total_acc" = total_acc)]
    
    input <- test
    write.table(input,"input.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = TRUE)
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    Output <- data.table(round(rpart.predict(model,test,type="prob")*100, 3))
    names(Output)[1] <- "Perentage chance of loan defaulting"
    names(Output)[2] <- "Perentage chance of loan fully paid back"
    ifelse(Output[,1] >= Output[,2], Output <- Output[,1], Output <- Output[,2])
    print(Output)
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)
