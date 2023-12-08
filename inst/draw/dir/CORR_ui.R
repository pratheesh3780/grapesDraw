CORR_ui <- function(){
  shiny::fluidRow(
    column(width = 9,
           shinydashboard::box(width = NULL, solidHeader = TRUE,
               shiny::plotOutput('corr')%>% shinycssloaders::withSpinner(color="#0dc5c1"),
               tags$br(),
               tags$script("
      $(document).on('shiny:connected', function(event) {
        var myWidth = $(window).width();
        Shiny.onInputChange('shiny_width', myWidth);
      });

      $(document).on('shiny:connected', function(event) {
        var myHeight = $(window).height();
        Shiny.onInputChange('shiny_height', myHeight);
      });
    "),   
               shiny::uiOutput('image_down_corr'),#download button for plot download
               tags$br()
           ),
           shinydashboard::box(width = NULL, 
               title = "Control Panel",
               shiny::uiOutput('cp_CORR')
               
           )
    ),
    column(width = 3,
           shinydashboard::box(width = NULL, status = "warning",
               fileInput("file1_CORR", "CSV File (upload in csv format)", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
               shiny::checkboxInput("header_CORR", "Header", TRUE),
               shiny::uiOutput('var_CORR'),
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
               shiny::uiOutput('data_set_CORR'),
               tags$br(),
               shiny::plotOutput('colours_CORR'),
               tags$br()
           )
    )
  )
}