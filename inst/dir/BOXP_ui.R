BOXP_ui <- function(){
                      fluidRow(
                        column(width = 9,
                               box(width = NULL, solidHeader = TRUE,
                                   plotOutput('boxp')%>% withSpinner(color="#0dc5c1"),
                                   tags$br(),
                                   uiOutput('image_down_boxp'),#download button for plot download
                                   tags$br()
                               ),
                               box(width = NULL, 
                                   title = "Control Panel",
                                   uiOutput('cp_BOXP')
                                   
                               ),
                               
                               box(width = NULL, 
                                   title =  "Advanced Manual Control Panel",
                                   uiOutput('manual_BOXP'),
                                   tags$br(),
                                   p(class=  "text-muted",
                                     paste("Warning: Manual controls will appear here when manual switch is toggled in basic controls"))
                                   
                                   
                               )
                        ),
                        column(width = 3,
                               box(width = NULL, status = "warning",
                                   fileInput("file1_BOXP", "CSV File (upload in csv format)", accept=c("text/csv", "text/comma-separated-values,text/plain", ".csv")),
                                   checkboxInput("header", "Header", TRUE),
                                   uiOutput('var_BOXP'),
                                   tags$br(),
                                   p(
                                     class = "text-muted",
                                     paste("Note: Upload a csv file from your system by clicking on Browse. you can download model dataset to see the format. Read the instruction to know more"
                                     )
                                   )
                               ),
                               box(width = NULL, status = "warning",
                                   tags$br(),
                                   p(
                                     class = "text-muted",
                                     paste("Note: Download the dataset here for testing")),
                                   uiOutput('data_set_BOXP'),
                                   tags$br(),
                                   plotOutput('colours_BOXP'),
                                   p(class = "text-muted",
                                     br(),
                                     "Source data updates every 15 seconds."
                                   )
                               )
                        )
                      )
                      
}                  
                    
                    

