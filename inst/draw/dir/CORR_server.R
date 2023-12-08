CORR_server <- function(input, output,session) {
  ########################## corr 
  csvfile_CORR <- shiny::reactive({
    csvfile_CORR <- input$file1_CORR
    if (is.null(csvfile_CORR)){return(NULL)}
    dt_corr  <- read.csv(csvfile_CORR $datapath, header=input$header_CORR, sep=",")
    dt_corr 
  })
  
  output$var_CORR  <- shiny::renderUI({
    if(is.null(input$file1_CORR$datapath)){
      return()
    }
    else{
      list (
        shiny::checkboxGroupInput("selvar", "Please select the variables", choices = names(csvfile_CORR())),
        
        shinyWidgets::actionBttn(
          inputId = "submit_CORR",
          label = "DRAW!",
          color = "danger",
          style = "jelly"
        )
      )
    }
  })
  ############## control panel for plots
  output$cp_CORR <- shiny::renderUI({
    if (is.null(input$file1_CORR$datapath)){return()}
    if (is.null(input$submit_CORR)){return()}
    if (input$submit_CORR > 0) {
      list(
        fluidRow(
          #### 1st row [1]
          #Method
          column(4,
                 shiny::selectInput(
                   "req2_corr", "Correlation Coefficient",
                   c(
                     PEARSON = "pearson",
                     SPEARMAN = "spearman"
                   ),
                   "pearson")
          ),
          #### 1st row [2]
          #Shape
          column(4,
                 shiny::selectInput(
                   "shape_corr", "Visualization method:",
                   c(
                     circle = "circle",
                     square = "square",
                     ellipse = "ellipse",
                     number = "number",
                     shade = "shade",
                     color = "color",
                     pie = "pie"
                   ),
                   "circle"
                 )
          ),
          #### 1st row [3]
          #layout
          column(4,
                 shiny::selectInput(
                   "layout_corr", "Correlogram Layout",
                   c(
                     full = "full",
                     upper = "upper",
                     lower = "lower"
                   ),
                   "circle")
          ),
          
          #### 2nd row [1]
          #Change Title
          column(4,
                 shiny::textInput("title_corr", "Enter required title", "title")
          ),
          #### 2nd row [2]
          #Change colours
          column(4,
                 shiny::selectInput("mycolors_corr", "Colour pattern:",
                             c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd",
                               "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3",
                               "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral"),
                             "Blues"
                 )
          ),
          #### 2nd row [3]
          #Size of correlation coefficient
          column(4,
                 shiny::sliderInput("cex_corr", "Required size of coefficient:",
                             min = 0.5, max = 3, value = 1
                 )
          ),
          #### 3rd row [1]
          #Significance level
          column(4,
                 shiny::radioButtons("sig_corr", "Pick significance level", choices = c("0.05", "0.01"))
          ),
          #### 3rd row [2]
          #Text colour
          column(4,
                 shiny::selectInput(
                   "txcol_corr", "Coefficient colour",
                   c(
                     Black = "#141413",
                     Transparent = "#00141413",
                     Red = "#ff0d1d",
                     Blue = "#0d45ff",
                     Green = "#0dff0d",
                     Yellow = "#ffdf0d",
                     Orange = "#ff8a0d"
                   ), "#141413"
                 )
          ),
          #### 3rd row [3]
          #show correlation coefficient
          column(4,
                 shiny::checkboxInput(
                   "remove_corr",
                   "Don't show correlation coefficient", FALSE)
          ),
          
          #### 4th row [1]
          #mark and umark significance
          column(4,
                 shiny::checkboxInput(
                   "significance_corr",
                   "Mark non-significant correlations in the plot", FALSE
                 )
          ),
          
          #### 4th row [2]
          #material switch colour
          column(4,
                 shinyWidgets::materialSwitch(inputId = "Show_col_switch_CORR", label = "Show available colours", status = "danger")
          ),
        )
        
      )
      
    }
  })
  
  
  
  ############### plotting for download
  plotInput_down <- shiny::reactive({
    if (is.null(input$file1_CORR$datapath)) {
      return()
    }
    if (is.null(input$submit_CORR)) {
      return()
    }
    if (input$submit_CORR > 0) 
      
    {
      x <- as.data.frame(subset(csvfile_CORR(), select = input$selvar))
      cormat1 <- cor(x, method = input$req2_corr, use = "complete.obs")
      req(input$mycolors_corr)
      corrplot::corrplot(cormat1,
                         method = input$shape_corr,
                         type = input$layout_corr, tl.col = "#000000",
                         col = brewer.pal(n = 8, name = input$mycolors_corr), addCoef.col = input$txcol_corr, number.cex = input$cex_corr
      )
      
      if (input$significance_corr > 0) {
        x <- as.data.frame(subset(csvfile_CORR(), select = input$selvar))
        cormat1 <- cor(x, method = input$req2_corr, use = "complete.obs")
        res1 <- corrplot::cor.mtest(x)
        req(input$mycolors_corr)
        corrplot::corrplot(cormat1,
                           method = input$shape_corr,
                           type = input$layout_corr, tl.col = "#000000",
                           col = brewer.pal(n = 8, name = input$mycolors_corr), addCoef.col = input$txcol_corr,
                           p.mat = res1$p, sig.level = as.numeric(input$sig_corr)
        )
      }
      
      if (input$remove_corr > 0) {
        x <- as.data.frame(subset(csvfile_CORR(), select = input$selvar))
        cormat1 <- cor(x, method = input$req2_corr, use = "complete.obs")
        res1 <- corrplot::cor.mtest(x)
        req(input$mycolors_corr)
        corrplot::corrplot(cormat1,
                           method = input$shape_corr,
                           type = input$layout_corr, tl.col = "#000000",
                           col = brewer.pal(n = 8, name =  input$mycolors_corr)
        )
      }
      
      if (input$remove_corr > 0 && input$significance_corr > 0) {
        x <- as.data.frame(subset(csvfile_CORR(), select = input$selvar))
        cormat1 <- cor(x, method = input$req2_corr, use = "complete.obs")
        res1 <- corrplot::cor.mtest(x)
        req(input$mycolors_corr)
        corrplot::corrplot(cormat1,
                           method = input$shape_corr,
                           type = input$layout_corr, tl.col = "#000000",
                           col = brewer.pal(n = 8, name = input$mycolors_corr),
                           p.mat = res1$p, sig.level = as.numeric(input$sig_corr)
        )
      }
    }
    
  })
  
  
  ############### plotting for display
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
      cormat1 <- cor(x, method = input$req2_corr, use = "complete.obs")
      req(input$mycolors_corr)
      corrplot::corrplot(cormat1,
                         method = input$shape_corr,
                         type = input$layout_corr, tl.col = "#000000",
                         col = brewer.pal(n = 8, name = input$mycolors_corr), addCoef.col = input$txcol_corr, number.cex = input$cex_corr
      )
      
      if (input$significance_corr > 0) {
        x <- as.data.frame(subset(csvfile_CORR(), select = input$selvar))
        cormat1 <- cor(x, method = input$req2_corr, use = "complete.obs")
        res1 <- corrplot::cor.mtest(x)
        req(input$mycolors_corr)
        corrplot::corrplot(cormat1,
                           method = input$shape_corr,
                           type = input$layout_corr, tl.col = "#000000",
                           col = brewer.pal(n = 8, name = input$mycolors_corr), addCoef.col = input$txcol_corr,
                           p.mat = res1$p, sig.level = as.numeric(input$sig_corr)
        )
      }
      
      if (input$remove_corr > 0) {
        x <- as.data.frame(subset(csvfile_CORR(), select = input$selvar))
        cormat1 <- cor(x, method = input$req2_corr, use = "complete.obs")
        res1 <- corrplot::cor.mtest(x)
        req(input$mycolors_corr)
        corrplot::corrplot(cormat1,
                           method = input$shape_corr,
                           type = input$layout_corr, tl.col = "#000000",
                           col = brewer.pal(n = 8, name =  input$mycolors_corr)
        )
      }
      
      if (input$remove_corr > 0 && input$significance_corr > 0) {
        x <- as.data.frame(subset(csvfile_CORR(), select = input$selvar))
        cormat1 <- cor(x, method = input$req2_corr, use = "complete.obs")
        res1 <- corrplot::cor.mtest(x)
        req(input$mycolors_corr)
        corrplot::corrplot(cormat1,
                           method = input$shape_corr,
                           type = input$layout_corr, tl.col = "#000000",
                           col = brewer.pal(n = 8, name = input$mycolors_corr),
                           p.mat = res1$p, sig.level = as.numeric(input$sig_corr)
        )
      }
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
      RColorBrewer::display.brewer.all(n = NULL, type = "all", select = NULL,
                         colorblindFriendly = TRUE)
    }
  },
  bg = "transparent"
  ) 
  
  
  
  ################### Download button
  output$image_down_corr <- shiny::renderUI({
    if (is.null(input$submit_CORR)) {
      return()
    }
    if (input$submit_CORR > 0) {
      list(shiny::downloadButton("downloadImage12",
                          label = "Download Plot", class = "butt1"
      ))
    }
  })
  ############# Download image
  output$downloadImage12 <- shiny::downloadHandler(
    filename = function() {
      timestamp <- format(Sys.time(), "%Y-%m-%d %H%M") # Format the timestamp
      paste0("corrplot_", timestamp, ".png")
    },
    content = function(file) {
      grDevices::png(file,
          width = input$shiny_width,
          height = input$shiny_height
      )
      plotInput_down()
      dev.off()
    }
  )
  
  ############################# download data set
  output$data_set_CORR = renderUI({
    
    list(
      shiny::selectInput(
        "filenames_corr", "Choose a dataset:",
        list.files(
          pattern = c("correlogram_1.csv|correlogram_2.csv")
        )
      ),
      shiny::downloadButton("downloadData12", label = "Download csv file", class = "butt12")
    )
    
    
    
  })
  
  datasetInput = shiny::reactive({
    switch(input$filenames_corr,
           filenames_corr
    )
  })
  
  output$downloadData12 = shiny::downloadHandler(
    filename = function() {
      input$filenames_corr
    },
    content = function(file) {
      write.csv(read.csv
                (input$filenames_corr, header = TRUE, sep = ","), file, row.names = FALSE)
    }
  ) 
}