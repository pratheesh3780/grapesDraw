library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(dplyr)
library(ggplot2)
library(viridis)
library(viridisLite)
library(hrbrthemes)
library(RColorBrewer)
library(ggthemes)
library(shinythemes)
library(GGally)

CORR_server <- function(input, output,session) {
  
  
  
  ########################## corr 
  csvfile_CORR <- reactive({
    csvfile_CORR <- input$file1_CORR
    if (is.null(csvfile_CORR)){return(NULL)}
    dt_corr  <- read.csv(csvfile_CORR $datapath, header=input$header, sep=",")
    dt_corr 
  })
  
  output$var_CORR  <- renderUI({
    if(is.null(input$file1_CORR$datapath)){
      return()
    }
    else{
      list (
        checkboxGroupInput("selvar", "Please select the variables", choices = names(csvfile_CORR())),
        
        actionBttn(
          inputId = "submit_CORR",
          label = "DRAW!",
          color = "danger",
          style = "jelly"
        )
      )
    }
  })
  ############## control panel for plots
  output$cp_CORR <- renderUI({
    if (is.null(input$file1_CORR$datapath)){return()}
    if (is.null(input$submit_CORR)){return()}
    if (input$submit_CORR > 0) {
      list(
        fluidRow(
          
          column(4,
                 textInput("title_corr", "Enter required title", "title")
          ), 
          column(4,
                 selectInput("mycolors", "Choose Colour pattern (plot):",
                             c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd",
                               "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3",
                               "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral"),
                             "Blues"
                 )
          ),
          column(4,
                 selectInput("type_CORR", "Choose the type:",
                             choices = c("tile", "circle","text"),
                             selected = "tile")
          ),
          column(4,
                 sliderInput("circle_size_CORR", "Choose the size of the circle (if choosen circle as type):",
                             min = 6, max =15, value = 10)
          ),
          column(4,
                 sliderInput("label_size_CORR", "Choose the size of correlation coefficient (if choosen circle as type):",
                             min = 3, max =8, value = 4)
          ),
          column(3,
                 materialSwitch(inputId = "Show_col_switch_CORR", label = "Show available colours", status = "danger")
          ),
        )
        
      )
      
    }
  })
  
  
  
  ############### plotting
  plotInput <- reactive({
    if (is.null(input$file1_CORR$datapath)) {
      return()
    }
    if (is.null(input$submit_CORR)) {
      return()
    }
    if (input$submit_CORR > 0) 
      
    {
      x <- as.data.frame(subset(csvfile_CORR(), select = input$selvar))
      
      p<-   ggcorr(x,palette = input$mycolors, geom = input$type_CORR, nbreaks = 4,
                   label_size =input$label_size_CORR, max_size = input$circle_size_CORR) 
        
    p
       
    }
  })
  ################################plot output 
  output$corr <- renderPlot( {
    plotInput()
  },
  bg = "transparent"
  ) 
  
  
  ###############color show
  output$colours_CORR <- renderPlot( {
    if (is.null(input$Show_col_switch_CORR)){return()}
    if (input$Show_col_switch_CORR > 0) {
      par(mar=c(3,4,2,2))
      display.brewer.all(n = NULL, type = "all", select = NULL,
                         colorblindFriendly = TRUE)
    }
  },
  bg = "transparent"
  ) 
  
  
  
  ################### Download button
  output$image_down_corr <- renderUI({
    if (is.null(input$submit_CORR)) {
      return()
    }
    if (input$submit_CORR > 0) {
      list(downloadButton("downloadImage12",
                          label = "Download Plot", class = "butt1"
      ))
    }
  })
  ############# Download image
  
  output$downloadImage12 <- downloadHandler(
    filename = "Correlogram.png",
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(...,
                       width = width, height = height,
                       res = 300, units = "in"
        )
      }
      ggsave(file, plot = plotInput(), device = device)
    }
  )
  ############################# download data set
  output$data_set_CORR = renderUI({
    
    list(
      selectInput(
        "filenames_corr", "Choose a dataset:",
        list.files(
          pattern = c("Correlogram 1.csv|Correlogram 2.csv")
        )
      ),
      downloadButton("downloadData12", label = "Download csv file", class = "butt12",)
    )
    
    
    
  })
  
  datasetInput = reactive({
    switch(input$filenames_corr,
           filenames_corr
    )
  })
  
  output$downloadData12 = downloadHandler(
    filename = function() {
      input$filenames_corr
    },
    content = function(file) {
      write.csv(read.csv
                (input$filenames_corr, header = TRUE, sep = ","), file, row.names = FALSE)
    }
  ) 
}