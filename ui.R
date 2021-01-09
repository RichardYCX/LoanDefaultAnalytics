# Import libraries
library(shiny)

####################################
# User interface                   #
####################################
ui <- pageWithSidebar(
  
  # Page header
  headerPanel('Loan Default Predictor'),
  
  # Input values
  sidebarPanel(
    HTML("<h3>Input parameters</h3>"),
    tags$head(
      HTML(
        "
          <script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
          "
      )
    ),
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
    textOutput("keepAlive"),
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
  )
)
