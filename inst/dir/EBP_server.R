EBP_server <- function(input, output,session) {
  
  
  
  ########################## EBP 
  csvfile_EBP <- reactive({
    csvfile_EBP <- input$file1_EBP
    if (is.null(csvfile_EBP)){return(NULL)}
    dt_EBP  <- read.csv(csvfile_EBP $datapath, header=input$header, sep=",")
    dt_EBP 
  })
  
  output$var_EBP  <- renderUI({
    if(is.null(input$file1_EBP$datapath)){
      return()
    }
    else{
      list (radioButtons("xebp", "Select the x axis variables", choices =    names(csvfile_EBP())),
            radioButtons("yebp", "Select the y axis variable", choices = names(csvfile_EBP())),
            radioButtons("lebp", "Select the significant letter", choices = names(csvfile_EBP())),
            radioButtons("cebp", "Select the error value (sd/se/ci)", choices = names(csvfile_EBP())),
            actionBttn(
              inputId = "submit_EBP",
              label = "DRAW!",
              color = "danger",
              style = "jelly"
            )
      )
    }
  })
  ############## control panel for plots
  output$cp_EBP <- renderUI({
    if (is.null(input$file1_EBP$datapath)){return()}
    if (is.null(input$submit_EBP)){return()}
    if (input$submit_EBP > 0) {
      list(
        fluidRow(
          column(4,
                 textInput("xlab_ebp", "Enter required x-axis title", "X-axis")
          ),
          column(4,
                 textInput("ylab_ebp", "Enter required y-axis title", "Y-axis")
          ),
          column(4,
                 textInput("title_ebp", "Enter required title", "title")
          ),
          column(4,
                 textInput("legend_ebp", "Enter required legend title", "legend")
          ),
          
          
          column(4,
                 selectInput("pal.col_EBP", "Choose Colour pattern (plot):",
                             list(`Sequential` = list("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"),
                                  `Qualitative` = list("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3"),
                                  `Diverging` = list("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral"))
                 )
          ), 
          column(4,
                 selectInput("Legend_Position_EBP", "Choose legend position:",
                             choices = c("none","right","left","top","bottom"),
                             selected = "right")
          ),
          column(
            4,
            selectInput(
              "theme_ebp", "Choose your theme:",
              list("normal","economist","minimal","grey","light","void","tufte","stata","wsj","calc","hc")
            )
          ),
          column(3,
                 materialSwitch(inputId = "Show_col_switch_EBP", label = "Show available colours", status = "danger")
          ),
          
          column(
            4,
            materialSwitch(inputId = "manual_change_EBP", label = "Show me manual controls", status = "danger")
          )
        )
        
      )
      
    }
  })
  
  #################manual changes of the plot
  
  output$manual_EBP <- renderUI({
    if (is.null(input$file1_EBP$datapath)) {
      return()
    }
    if (is.null(input$submit_EBP)) {
      return()
    }
    if (is.null(input$manual_change_EBP)) {
      return()
    }
    if (input$submit_EBP > 0 && input$manual_change_EBP > 0) {
      list(
        fluidRow(
          
          column(4,
                 selectInput("face_ebp", "select font face",
                             choices = c("plain","italic","bold","bold.italic"),
                             selected = "plain")
          ),
          column(4,
                 selectInput("font_selector_ebp", "select a font",
                             choices = c(
                               "Arial" ,"Calibri","Georgia","Helvetica" ,"Palatino","Garamond",
                               "Times New Roman" ,"Baskerville","Courier New","Verdana","Century Schoolbook",
                               "Liberation Serif"
                             ),
                             selected = "Arial")),
          column(4,
                 selectInput("colour2_ebp","choose colour of axis title and label:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(4,
                 selectInput("colour4_ebp","choose colour of legend title and label:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(4,
                 selectInput("colour3_ebp","choose colour of plot title:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(5,
                 sliderInput("size1_ebp", "axis label size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 sliderInput("size2_ebp", "axis title size:",
                             min = 10, max = 20, value = 10
                 )
          ),
          column(5,
                 sliderInput("size3_ebp", "legend label size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 sliderInput("size4_ebp", "legend title size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 sliderInput("size5_ebp", "plot title size:",
                             min = 15, max = 30, value = 15)
          ),
          column(5,
                 sliderInput("angle_ebp","required angle of x-axis labels (degrees)",min=0, max=135, value= 0, step = 15)
          ),
          column(5,
                 sliderInput("Barwidth_ebp","select required bar width",
                             min=0.1, max=1, value=0.9)
          )
        )
      )
    }
  }) 
  
  ############### plotting
  plotInput <- reactive({
    if (is.null(input$file1_EBP$datapath)) {
      return()
    }
    if (is.null(input$submit_EBP)) {
      return()
    }
    if (input$submit_EBP > 0) {
      x <- as.matrix(csvfile_EBP()[, input$xebp])
      y <- as.matrix(csvfile_EBP()[, input$yebp])
      z <- as.matrix(csvfile_EBP()[, input$lebp])
      w <- as.matrix(csvfile_EBP()[, input$cebp])
      data_ebp <- data.frame(
        xvar = x,
        letter = z,
        yvar = y,
        ci = w
      )
      
      data_ebp$xvar <- factor(data_ebp$xvar, levels = unique(data_ebp$xvar)) ### sorting factors bcz T1, T2 etc can cause problem
      #data_ebp$colour <- factor(data_ebp$colour, levels = unique(data_ebp$colour))
      nb.col <- nlevels(data_ebp$xvar) # number of colouring factors
      
      if (is.null(input$pal.col_EBP)){
        mycolors <- colorRampPalette(brewer.pal(8, "Blues"))(nb.col) 
      } else {
        # Changed here for a debugging
        mycolors <- colorRampPalette(brewer.pal(8, input$pal.col_EBP))(nb.col)
      }
      
      
      
      #mycolors <- colorRampPalette(brewer.pal(8, input$pal.col_EBP))(nb.col) # colorrampallete code
      p <- ggplot2::ggplot(data_ebp, aes(fill = xvar, y = yvar, x = xvar)) + # plot using aesthetics
        geom_bar(
          stat = "identity",
          position = position_dodge(width = 0.1),## change space between bars
          alpha = 1 , width = input $ Barwidth_ebp # alpha = input trans server crd # width = input width1 server crd
        )+
        geom_errorbar(aes(ymin = yvar - 1.96*ci, ymax = yvar+ 1.96*ci),
                      width = 0.2, colour = "#354543", alpha = 1,
                      position=position_dodge(.5)
        )+
        geom_text(aes(label = letter, y = yvar + ci), vjust = -2.4,position=position_dodge(.5),size=3)+
        scale_fill_manual(values = mycolors) + # apply the color scale to fill the aesthetic of a plot
        xlab(input$xlab_ebp) + # to give labels on x-axis
        ylab(input$ylab_ebp) + # to give labels on y axis
        ggtitle(input$title_ebp) + # to give title of plot
        labs(fill= input$legend_ebp)+
        theme(axis.text.x=element_text(angle=input$angle_ebp,color = input$colour2_ebp, vjust = 0.5),
              axis.text.y=element_text(color = input$colour2_ebp))+
        theme(
          axis.text = element_text(face = input$face_ebp,family = input$font_selector_ebp, size=input$size1_ebp)
        )+
        theme(legend.text = element_text(face = input$face_ebp,color = input$colour4_ebp,family = input$font_selector_ebp,size=input$size3_ebp,)
        )+
        theme(axis.title  = element_text(size=input$size2_ebp, family = input$font_selector_ebp, face = input$face_ebp, color = input$colour2_ebp,hjust=0.5)
        )+
        theme(legend.title = element_text(face = input$face_ebp,color = input$colour4_ebp,family = input$font_selector_ebp, size=input$size4_ebp,)
        )+
        theme(plot.title = element_text(size=input$size5_ebp,face = input$face_ebp, family = input$font_selector_ebp, hjust = 0.5,color = input$colour3_ebp),
        )+
        theme(legend.position=input$Legend_Position_EBP)
      
      
      #############################  
      if (input$submit_EBP>0) {
        if (is.null(input$theme_ebp)) {
          return(p)
        }
        if (input$theme_ebp == "normal") {
          q <- p + theme_bw()
        } else if (input$theme_ebp == "economist") {
          q <- p + ggthemes::theme_economist() 
        } else if (input$theme_ebp == "grey") {
          q <- p + theme_gray() 
        } 
        else if (input$theme_ebp == "minimal") {
          q <- p + theme_minimal()
        }else if (input$theme_ebp == "light") {
          q <- p + theme_light()
        }
        else if (input$theme_ebp == "void") {
          q <- p + theme_void()
        }
        else if (input$theme_ebp == "tufte") {
          q <- p + ggthemes::theme_tufte()
        }
        else if (input$theme_ebp == "stata") {
          q <- p + ggthemes::theme_stata()
        }
        else if (input$theme_ebp == "wsj") {
          q <- p + ggthemes::theme_wsj()
        }
        else if (input$theme_ebp == "calc") {
          q <- p + ggthemes::theme_calc()
        }
        else if (input$theme_ebp == "hc") {
          q <- p + ggthemes::theme_hc()
        }
        q <- q+theme(axis.text.x=element_text(angle=input$angle_ebp,color = input$colour2_ebp, vjust = 0.5),
                     axis.text.y=element_text(color = input$colour2_ebp))+
          theme(
            axis.text = element_text(face = input$face_ebp,family = input$font_selector_ebp, size=input$size1_ebp)
          )+
          theme(legend.text = element_text(face = input$face_ebp,color = input$colour4_ebp,family = input$font_selector_ebp,size=input$size3_ebp,)
          )+
          theme(axis.title  = element_text(size=input$size2_ebp, family = input$font_selector_ebp, face = input$face_ebp, color = input$colour2_ebp,hjust=0.5)
          )+
          theme(legend.title = element_text(face = input$face_ebp,color = input$colour4_ebp,family = input$font_selector_ebp, size=input$size4_ebp,)
          )+
          theme(plot.title = element_text(size=input$size5_ebp,face = input$face_ebp, family = input$font_selector_ebp, hjust = 0.5,color = input$colour3_ebp),
          )+
          theme(legend.position=input$Legend_Position_EBP)
        q
      }
      else{
        return(p) 
      }
    }
    
  })
  ################################plot output 
  output$ebp <- renderPlot( {
    plotInput()
  },
  bg = "transparent"
  ) 
  
  
  ###############color show
  output$colours_EBP <- renderPlot( {
    if (is.null(input$Show_col_switch_EBP)){return()}
    if (input$Show_col_switch_EBP > 0) {
      par(mar=c(3,4,2,2))
      display.brewer.all(n = NULL, type = "all", select = NULL,
                         colorblindFriendly = TRUE)
    }
  },
  bg = "transparent"
  ) 
  
  
  ################### Download button
  output$image_down_ebp <- renderUI({
    if (is.null(input$submit_EBP)) {
      return()
    }
    
    if (input$submit_EBP > 0 ) {
      list(downloadButton("downloadImage2",
                          label = "Download Plot", class = "butt2"
      ))
    }
  })
  ############# Download image
  
  output$downloadImage2 <- downloadHandler(
    filename = "Error bar.png",
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
  output$data_set_EBP = renderUI({
    
    list(
      selectInput(
        "filenames_ebp", "Choose a dataset:",
        list.files(
          pattern = c("Error bar 1.csv|Error bar 2.csv")
        )
      ),
      downloadButton("downloadData2", label = "Download csv file", class = "butt2",)
    )
    
    
    
  })
  
  datasetInput = reactive({
    switch(input$filenames_ebp,
           filenames_ebp
    )
  })
  
  output$downloadData2 = downloadHandler(
    filename = function() {
      input$filenames_ebp
    },
    content = function(file) {
      write.csv(read.csv
                (input$filenames_ebp, header = TRUE, sep = ","), file, row.names = FALSE)
    }
  )
  
  #########################
}