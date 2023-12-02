VLP_server <- function(input, output,session) {
  
  
  
  ########################## VLP 
  csvfile_VLP <- reactive({
    csvfile_VLP <- input$file1_VLP
    if (is.null(csvfile_VLP)){return(NULL)}
    dt_MBP  <- read.csv(csvfile_VLP $datapath, header=input$header, sep=",")
    dt_MBP 
  })
  
  output$var_VLP  <- renderUI({
    if(is.null(input$file1_VLP$datapath)){
      return()
    }
    else{
      list (radioButtons("xvlp", "Select the x axis variables", choices =    names(csvfile_VLP())),
            radioButtons("yvlp", "Select the y axis variable", choices = names(csvfile_VLP())),
            actionBttn(
              inputId = "submit_VLP",
              label = "DRAW!",
              color = "danger",
              style = "jelly"
            )
      )
    }
  })
  ############## control panel for plots
  output$cp_VLP <- renderUI({
    if (is.null(input$file1_VLP$datapath)){return()}
    if (is.null(input$submit_VLP)){return()}
    if (input$submit_VLP > 0) {
      list(
        fluidRow(
          column(4,
                 textInput("xlab_vlp", "Enter required x-axis title", "X-axis")
          ),
          column(4,
                 textInput("ylab_vlp", "Enter required y-axis title", "Y-axis")
          ),
          column(4,
                 textInput("title_vlp", "Enter required title", "title")
          ),
          column(4,
                 textInput("colour1_vlp", "Enter required legend title", "legend")
          ),
          
          
          column(4,
                 selectInput("pal.col_VLP", "Choose Colour pattern (plot):",
                             list(`Sequential` = list("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"),
                                  `Qualitative` = list("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3"),
                                  `Diverging` = list("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral"))
                 )
          ),
          column(4,
                 selectInput("Legend_Position_VLP", "Choose legend position:",
                             choices = c("none","right","left","top","bottom"),
                             selected = "right")
          ),
          column(
            4,
            selectInput(
              "theme_vlp", "Choose your theme:",
              list("normal","economist","minimal","grey","light","void","tufte","stata","wsj","calc","hc")
            )
          ),
          column(3,
                 materialSwitch(inputId = "Show_col_switch_VLP", label = "Show available colours", status = "danger")
          ),
          column(
            4,
            materialSwitch(inputId = "manual_change_vlp", label = "Show me manual controls", status = "danger")
          )
        )
        
      )
      
    }
  })
  
  
  
  #################manual changes of the plot
  
  output$manual_VLP <- renderUI({
    if (is.null(input$file1_VLP$datapath)) {
      return()
    }
    if (is.null(input$submit_VLP)) {
      return()
    }
    if (is.null(input$manual_change_vlp)) {
      return()
    }
    if (input$submit_VLP > 0 && input$manual_change_vlp > 0) {
      list(
        fluidRow(
          
          column(4,
                 selectInput("face_vlp", "select font face",
                             choices = c("plain","italic","bold","bold.italic"),
                             selected = "plain")
          ),
          column(4,
                 selectInput("font_selector_vlp", "select a font",
                             choices = c(
                               "Arial" ,"Calibri","Georgia","Helvetica" ,"Palatino","Garamond",
                               "Times New Roman" ,"Baskerville","Courier New","Verdana","Century Schoolbook",
                               "Liberation Serif"
                             ),
                             selected = "Arial")),
          column(4,
                 selectInput("colour2_vlp","choose colour of axis title and label:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(4,
                 selectInput("colour4_vlp","choose colour of legend title and label:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(4,
                 selectInput("colour3_vlp","choose colour of plot title:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(5,
                 sliderInput("size1_vlp", "axis label size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 sliderInput("size2_vlp", "axis title size:",
                             min = 10, max = 20, value = 10
                 )
          ),
          column(5,
                 sliderInput("size3_vlp", "legend label size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 sliderInput("size4_vlp", "legend title size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 sliderInput("size5_vlp", "plot title size:",
                             min = 15, max = 30, value = 15)
          ),
          column(5,
                 sliderInput("angle_vlp","required angle of x-axis labels (degrees)",min=0, max=135, value= 0, step = 15)
          )
        )
      )
    }
  }) 
  
  ############### plotting
  plotInput <- reactive({
    if (is.null(input$file1_VLP$datapath)) {
      return()
    }
    if (is.null(input$submit_VLP)) {
      return()
    }
    if (input$submit_VLP > 0) {
      x <- as.matrix(csvfile_VLP()[, input$xvlp])
      y <- as.matrix(csvfile_VLP()[, input$yvlp])
      
      data_vlp <- data.frame(
        xvar = x,
        yvar = y
      )
      
      data_vlp$xvar <- factor(data_vlp$xvar, levels = unique(data_vlp$xvar)) ### sorting factors bcz T1, T2 etc can cause problem
      nb.col <- nlevels(data_vlp$xvar) # number of colouring factors
      
      if (is.null(input$pal.col_VLP)){
        mycolors <- colorRampPalette(brewer.pal(8, "Blues"))(nb.col) 
      } else {
        # Changed here for a debugging
        mycolors <- colorRampPalette(brewer.pal(8, input$pal.col_VLP))(nb.col)
      }
      
      
      
      #mycolors <- colorRampPalette(brewer.pal(8, input$pal.col_VLP))(nb.col) # colorrampallete code
      p <-ggplot2::ggplot(data_vlp, aes(fill=xvar, y=yvar, x=xvar)) + #plot using aesthetics
        geom_violin() +
        scale_fill_manual(values = mycolors) +#apply the color scale to fill the aesthetic of a plot
        xlab(input$xlab_vlp)+# to give labels on x-axis
        ylab(input$ylab_vlp)+# to give labels on y axis
        ggtitle(input$title_vlp)+  #to give title of plot
        labs(fill= input$colour1_vlp)+
        theme(axis.text.x=element_text(angle=input$angle_vlp,color = input$colour2_vlp, vjust = 0.5),
              axis.text.y=element_text(color = input$colour2_vlp))+
        theme(
          axis.text = element_text(face = input$face_vlp,family = input$font_selector_vlp, size=input$size1_vlp)
        )+
        theme(legend.text = element_text(face = input$face_vlp,color = input$colour4_vlp,family = input$font_selector_vlp,size=input$size3_vlp,)
        )+
        theme(axis.title  = element_text(size=input$size2_vlp, family = input$font_selector_vlp, face = input$face_vlp, color = input$colour2_vlp,hjust=0.5)
        )+
        theme(legend.title = element_text(face = input$face_vlp,color = input$colour4_vlp,family = input$font_selector_vlp, size=input$size4_vlp,)
        )+
        theme(plot.title = element_text(size=input$size5_vlp,face = input$face_vlp, family = input$font_selector_vlp, hjust = 0.5,color = input$colour3_vlp))+
        theme(legend.position = input$Legend_Position_VLP)
      
      
      #############################  
      
      
      if (input$submit_VLP>0) {
        if (is.null(input$theme_vlp)) {
          return(p)
        }
        if (input$theme_vlp == "normal") {
          q <- p + theme_bw()
        } else if (input$theme_vlp == "economist") {
          q <- p + ggthemes::theme_economist() 
        } else if (input$theme_vlp == "grey") {
          q <- p + theme_gray() 
        } 
        else if (input$theme_vlp == "minimal") {
          q <- p + theme_minimal()
        }else if (input$theme_vlp == "light") {
          q <- p + theme_light()
        }
        else if (input$theme_vlp == "void") {
          q <- p + theme_void()
        }
        else if (input$theme_vlp == "tufte") {
          q <- p + ggthemes::theme_tufte()
        }
        else if (input$theme_vlp == "stata") {
          q <- p + ggthemes::theme_stata()
        }
        else if (input$theme_vlp == "wsj") {
          q <- p + ggthemes::theme_wsj()
        }
        else if (input$theme_vlp == "calc") {
          q <- p + ggthemes::theme_calc()
        }
        else if (input$theme_vlp == "hc") {
          q <- p + ggthemes::theme_hc()
        }
        q<- q+theme(axis.text.x=element_text(angle=input$angle_vlp,color = input$colour2_vlp, vjust = 0.5),
                    axis.text.y=element_text(color = input$colour2_vlp))+
          theme(
            axis.text = element_text(face = input$face_vlp,family = input$font_selector_vlp, size=input$size1_vlp)
          )+
          theme(legend.text = element_text(face = input$face_vlp,color = input$colour4_vlp,family = input$font_selector_vlp,size=input$size3_vlp,)
          )+
          theme(axis.title  = element_text(size=input$size2_vlp, family = input$font_selector_vlp, face = input$face_vlp, color = input$colour2_vlp,hjust=0.5)
          )+
          theme(legend.title = element_text(face = input$face_vlp,color = input$colour4_vlp,family = input$font_selector_vlp, size=input$size4_vlp,)
          )+
          theme(plot.title = element_text(size=input$size5_vlp,face = input$face_vlp, family = input$font_selector_vlp, hjust = 0.5,color = input$colour3_vlp))+
          theme(legend.position = input$Legend_Position_VLP)
        q
      }
      else{
        return(p) 
      }
    }
    
  })
  ################################plot output 
  output$vlp <- renderPlot( {
    plotInput()
  },
  bg = "transparent"
  ) 
  
  
  ###############color show
  output$colours_VLP <- renderPlot( {
    if (is.null(input$Show_col_switch_VLP)){return()}
    if (input$Show_col_switch_VLP > 0) {
      par(mar=c(3,4,2,2))
      display.brewer.all(n = NULL, type = "all", select = NULL,
                         colorblindFriendly = TRUE)
    }
  },
  bg = "transparent"
  ) 
  
  
  ################### Download button
  output$image_down_vlp <- renderUI({
    if (is.null(input$submit_VLP)) {
      return()
    }
    if (input$submit_VLP > 0) {
      list(downloadButton("downloadImage9",
                          label = "Download Plot", class = "butt1"
      ))
    }
  })
  ############# Download image
  
  output$downloadImage9 <- downloadHandler(
    filename = "Violin plot.png",
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
  output$data_set_VLP = renderUI({
    
    
    
    list(
      selectInput(
        "filenames_vlp", "Choose a dataset:",
        list.files(
          pattern = c("Violin plot 1.csv|Violin plot 2.csv")
        )
      ),
      downloadButton("downloadData9", label = "Download csv file", class = "butt9",)
    )
    
    
    
  })
  
  datasetInput = reactive({
    switch(input$filenames_vlp,
           filenames_vlp
    )
  })
  
  output$downloadData9 = downloadHandler(
    filename = function() {
      input$filenames_vlp
    },
    content = function(file) {
      write.csv(read.csv
                (input$filenames_vlp, header = TRUE, sep = ","), file, row.names = FALSE)
    }
  )
  ######################### end data set download
}
