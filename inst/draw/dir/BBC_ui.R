BBC_ui <- function(){
  shiny::fluidRow(
    column(width = 9,
          shinydashboard::box(width = NULL, solidHeader = TRUE,
               shiny::plotOutput('bbc')%>%  shinycssloaders::withSpinner(color="#0dc5c1"),
               tags$br(),
               shiny::uiOutput('image_down_bbc'),#download button for plot download
               tags$br()
           ),
          shinydashboard::box(width = NULL, 
               title = "Basic Control Panel",
               shiny::uiOutput('cp_BBC')
               
           ),
          shinydashboard::box(width = NULL, 
               title =  "Advanced Manual Control Panel",
               tags$br(),
               p(class=  "text-muted",
                 shiny::uiOutput('manual_BBC'),
                 paste("Warning: Manual controls will appear here when manual switch is toggled in basic controls"))
               
           )
    ),
    column(width = 3,
           shinydashboard::box(width = NULL, status = "warning",
              shiny::fileInput("file1_BBC", "CSV File (upload in csv format)", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
               shiny::checkboxInput("header_BBC", "Header", TRUE),
              shiny::uiOutput('var_BBC'),
               tags$br(),
               p(
                 class = "text-muted",
                 paste("Note: Upload a csv file from your system by clicking on Browse. you can download model dataset to see the format. Read the instruction to know more"
                 )
               )
           ),
           shinydashboard::box(width = NULL, status = "warning",
               tags$br(),
               p(
                 class = "text-muted",
                 paste("Note: Download the dataset here for testing")),
               shiny::uiOutput('data_set_BBC'),
               tags$br(),
               shiny::plotOutput('colours_BBC'),
               tags$br()
           )
    )
  )
}