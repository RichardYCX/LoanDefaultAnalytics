library(shiny)
library(data.table)
library(rpart)
library(rpart.plot)

# Read in the RF model
model <- readRDS("model.rds")

####################################
# Server                           #
####################################

TrainSet <- fread("input.csv", header = TRUE, stringsAsFactors = T)
server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    test <- TrainSet[1]
    test[, ":="("avg_fico_range_high" = input$avg_fico_range_high,
                "avg_num_rev_accts" = input$avg_num_rev_accts,
                "bc_open_to_buy" = input$bc_open_to_buy,
                "combined_dti" = input$combined_dti,
                "loan_amnt" = input$loan_amnt,
                "term" = input$term,
                "total_acc" = total_acc)]
    
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
