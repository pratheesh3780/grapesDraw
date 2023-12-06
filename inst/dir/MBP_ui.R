MBP_ui <- function() {
  #### Four Boxes are there, 1st one plot box, which is big, control box,
  
  shiny::fluidRow(
    column(width = 9,
           shinydashboard::box(width = NULL, solidHeader = TRUE,
               shiny::plotOutput('mbp')%>% shinycssloaders::withSpinner(color="#0dc5c1"),
               tags$br(),
               shiny::uiOutput('image_down_mbp'),#download button for plot download
               tags$br()
           ),
           shinydashboard::box(width = NULL, 
               title = "Basic Control Panel",
               shiny::uiOutput('cp_MBP')
               
           ),
           shinydashboard::box(width = NULL, 
               title =  "Advanced Manual Control Panel",
               shiny::uiOutput('manual_change_MBP'),
               tags$br(),
               p(class=  "text-muted",
                 paste("Warning: Manual controls will appear here when manual switch is toggled in basic controls"))
               
               
           )
    ),
    column(width = 3,
           shinydashboard::box(width = NULL, status = "warning",
               shiny::fileInput("file1_MBP", "CSV File (upload in csv format)", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
               shiny::checkboxInput("header", "Header", TRUE),
               shiny::uiOutput('var_MBP'),
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
               shiny::uiOutput('data_set_MBP'),
               tags$br(),
               shiny::plotOutput('colours_MBP'),
               tags$br()
               
           )
    )
  )
}
