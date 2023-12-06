DDG_server <- function(input, output,session) {
  
  
  
  ########################## cBP 
  csvfile_DDG <- shiny::reactive({
    csvfile_DDG <- input$file1_DDG
    if (is.null(csvfile_DDG)){return(NULL)}
    dt_ddg  <- read.csv(csvfile_DDG $datapath, header=input$header, sep=",")
    dt_ddg 
  })
  
  output$var_DDG  <- shiny::renderUI({
    if(is.null(input$file1_DDG$datapath)){
      return()
    }
    else{
      list (
        
        shinyWidgets::actionBttn(
          inputId = "submit_DDG",
          label = "DRAW!",
          color = "danger",
          style = "jelly"
        )
      )
    }
  })
  
  ############## control panel for plots
  output$cp_DDG <- shiny::renderUI({
    if (is.null(input$file1_DDG$datapath)){return()}
    if (is.null(input$submit_DDG)){return()}
    if (input$submit_DDG > 0) {
      list(
        fluidRow(
          column(4,
                 shiny::textInput("xlab_ddg", "Enter required x-axis title", "X-axis")
          ),
          column(4,
                 shiny::textInput("ylab_ddg", "Enter required y-axis title", "Y-axis")
          ),
          column(4,
                 shiny::textInput("title_ddg", "Enter required title", "title")
          ),
          column(4,
                 shiny::selectInput("pal.col_DDG", "Choose Colour pattern (plot):",
                             list(`Qualitative` = list("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3"),
                                  `Sequential` = list("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"),
                                  `Diverging` = list("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral")),
                              
                 )
          ), 
        
      
      column(4,
             shiny::selectInput("label_col_ddg","choose color of the label",
                         c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                         "black")
      ),
      column(4,
             shiny::selectInput("type_ddg","choose type of the plot",
                         c("rectangle",  "circular", "phylogenic"),
                         "rectangle"
                         
             )
      ),
      
      column(5,
             shiny::sliderInput("size_ddg", "Change the label size:",
                         min = 0.5, max = 2, value = 0.8)
      ),
      column(5,
             shiny::sliderInput("width_ddg", "Change the line width:",
                         min = 0.5, max = 2, value = 0.8)
      ),
      column(5,
             shiny::sliderInput("groups_ddg", "Change the number of groups:",
                         min = 1, max = 10, value = 5)
      ),
          column(3,
                 shinyWidgets::materialSwitch(inputId = "Show_col_switch_DDG", label = "Show available colours", status = "danger")
          ) 
        )
        
      )
      
    }
  })
  
  
  ############### plotting
  plotInput <- shiny::reactive({
    if (is.null(input$file1_DDG$datapath)) {
      return()
    }
    if (is.null(input$submit_DDG)) {
      return()
    }
    if (input$submit_DDG > 0) {
      #x <- as.matrix(csvfile_DDG()[, input$xddg])
      #y <- as.matrix(csvfile_DDG()[, input$yddg])
      
      
      data_ddg <- as.matrix(
        csvfile_DDG()
      )
       
      dist_matrix <- stats::dist( data_ddg)
      hclust_result <- stats::hclust(dist_matrix)
      dend <- as.dendrogram(hclust_result)
       
      p<- factoextra::fviz_dend(dend, cex=input$size_ddg, lwd=input$width_ddg, k=input$groups_ddg,
                   palette = input$pal.col_DDG,main =input$title_ddg, 
                   xlab = input$xlab_ddg,
                   ylab = input$ylab_ddg,
                   label_cols = input$label_col_ddg,
                   type=input$type_ddg
                   
      )
      
      return(p)
    }
    
  })
  ################################plot output 
  output$ddg <- shiny::renderPlot( {
    plotInput()
  },
  bg = "transparent"
  ) 
  
  
  ###############color show
  output$colours_DDG <- shiny::renderPlot( {
    if (is.null(input$Show_col_switch_DDG)){return()}
    if (input$Show_col_switch_DDG > 0) {
      par(mar=c(3,4,2,2))
      RColorBrewer::display.brewer.all(n = NULL, type = "all", select = NULL,
                         colorblindFriendly = TRUE)
    }
  },
  bg = "transparent"
  ) 
 
  ################### Download button
  output$image_down_ddg <- shiny::renderUI({
    if (is.null(input$submit_DDG)) {
      return()
    }
    if (input$submit_DDG > 0) {
      list(shiny::downloadButton("downloadImage14",
                          label = "Download Plot", class = "butt1"
      ))
    }
  })
  ############# Download image
  
  output$downloadImage14 <- shiny::downloadHandler(
    filename = "Dendrogram.png",
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(...,
                       width = width, height = height,
                       res = 300, units = "in"
        )
      }
      ggplot2::ggsave(file, plot = plotInput(), device = device)
    }
  ) 
  ############################# download data set
  output$data_set_DDG = shiny::renderUI({
    
    list(
      shiny::selectInput(
        "filenames_ddg", "Choose a dataset:",
        list.files(
          pattern = c("dendrogram_1.csv|dendrogram_2.csv")
        )
      ),
      shiny::downloadButton("downloadData14", label = "Download csv file", class = "butt14",)
    )
    
    
    
  })
  
  datasetInput = shiny::reactive({
    switch(input$filenames_ddg,
           filenames_ddg
    )
  })
  
  output$downloadData14 = shiny::downloadHandler(
    filename = function() {
      input$filenames_ddg
    },
    content = function(file) {
      write.csv(read.csv
                (input$filenames_ddg, header = TRUE, sep = ","), file, row.names = FALSE)
    }
  ) 
}
