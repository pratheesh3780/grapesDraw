CBP_server <- function(input, output,session) {
  
  
  
  ########################## cBP 
  csvfile_CBP <- shiny::reactive({
    csvfile_CBP <- input$file1_CBP
    if (is.null(csvfile_CBP)){return(NULL)}
    dt_cbp  <- read.csv(csvfile_CBP $datapath, header=input$header_CBP, sep=",")
    dt_cbp 
  })
  
  output$var_CBP  <- shiny::renderUI({
    if(is.null(input$file1_CBP$datapath)){
      return()
    }
    else{
      list (shiny::radioButtons("xcbp", "Select the x axis variables", choices =    names(csvfile_CBP())),
            shiny::radioButtons("ycbp", "Select the observations (quantitative variable)", choices = names(csvfile_CBP())),
            
            shinyWidgets::actionBttn(
              inputId = "submit_CBP",
              label = "DRAW!",
              color = "danger",
              style = "jelly"
            )
      )
    }
  })
  
  ############## control panel for plots
  output$cp_CBP <- shiny::renderUI({
    if (is.null(input$file1_CBP$datapath)){return()}
    if (is.null(input$submit_CBP)){return()}
    if (input$submit_CBP > 0) {
      list(
        fluidRow(
          column(4,
                 shiny::textInput("xlab_cbp", "Enter required x-axis title", "X-axis")
          ),
          column(4,
                 shiny::textInput("ylab_cbp", "Enter required y-axis title", "Y-axis")
          ),
          column(4,
                 shiny::textInput("title_cbp", "Enter required title", "title")
          ),
          column(4,
                 shiny::textInput("legend_cbp", "Enter required legend title", "legend")
          ),
          
          
          column(4,
                 shiny::selectInput("pal.col_CBP", "Choose Colour pattern (plot):",
                             list(`Sequential` = list("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"),
                                  `Qualitative` = list("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3"),
                                  `Diverging` = list("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral"))
                 )
          ),
          column(4,
                 shiny::selectInput("Legend_Position_CBP", "Choose legend position:",
                             choices = c("none","right","left","top","bottom"),
                             selected = "right")
          ),
          
          column(5,
                 shiny::sliderInput("y_min", "y axis min:",
                             min = -15, max = 0, value = -5
                 )
          ),
          column(
            4,
            shiny::selectInput(
              "theme_cbp", "Choose your theme:",
              list("normal","economist","minimal","grey","light","void","tufte","stata","wsj","calc","hc")
            )
          ),
          column(3,
                 shinyWidgets::materialSwitch(inputId = "Show_col_switch_CBP", label = "Show available colours", status = "danger")
          ),
          
          column(
            4,
            shinyWidgets::materialSwitch(inputId = "manual_change_cbp", label = "Show me manual controls", status = "danger")
          )
        )
        
      )
      
    }
  })
  
  
  #################manual changes of the plot
  
  output$manual_change_CBP <- shiny::renderUI({
    if (is.null(input$file1_CBP$datapath)) {
      return()
    }
    if (is.null(input$submit_CBP)) {
      return()
    }
    if (is.null(input$manual_change_cbp)) {
      return()
    }
    if (input$submit_CBP > 0 && input$manual_change_cbp > 0) {
      list(
        fluidRow(
          
          column(4,
                 shiny::selectInput("face_cbp", "select font face",
                             choices = c("plain","italic","bold","bold.italic"),
                             selected = "plain")
          ),
          column(4,
                 shiny::selectInput("colour2_cbp","choose colour of axis title and label:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(4,
                 shiny::selectInput("colour4_cbp","choose colour of legend title and label:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(4,
                 shiny::selectInput("colour3_cbp","choose colour of plot title:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          
          column(4,
                 shiny::selectInput("font_selector_cbp", "select a font",
                             choices = c(
                               "Arial" ,"Calibri","Georgia","Helvetica" ,"Palatino","Garamond",
                               "Times New Roman" ,"Baskerville","Courier New","Verdana","Century Schoolbook",
                               "Liberation Serif"
                             ),
                             selected = "Arial")),
          
          column(5,
                 shiny::sliderInput("size1_cbp", "axis label size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 shiny::sliderInput("size2_cbp", "axis title size:",
                             min = 10, max = 20, value = 10
                 )
          ),
          column(5,
                 shiny::sliderInput("size3_cbp", "legend label size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 shiny::sliderInput("size4_cbp", "legend title size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 shiny::sliderInput("size5_cbp", "plot title size:",
                             min = 15, max = 30, value = 15)
          ),
          column(5,
                 shiny::sliderInput("angle_cbp","required angle of x-axis labels (degrees)",
                             min=0, max=135, value=0, step = 15)
          ),
          column(5,
                 shiny::sliderInput("Barwidth_cbp","select required bar width",
                             min=0.1, max=1, value=0.9 )
          )
          
          
        )
      )
    }
  }) 
  
  ############### plotting
  plotInput <- shiny::reactive({
    if (is.null(input$file1_CBP$datapath)) {
      return()
    }
    if (is.null(input$submit_CBP)) {
      return()
    }
    if (input$submit_CBP > 0) {
      x <- as.matrix(csvfile_CBP()[, input$xcbp])
      y <- as.matrix(csvfile_CBP()[, input$ycbp])
      
      
      data_cbp <- data.frame(
        xvar = x,
        yvar = y
      )
      
      data_cbp$xvar <- factor(data_cbp$xvar, levels = unique(data_cbp$xvar)) ### sorting factors bcz T1, T2 etc can cause problem
      #data_cbp$colour <- factor(data_cbp$colour, levels = unique(data_cbp$colour))
      nb.col <- nlevels(data_cbp$xvar) # number of colouring factors
      
      if (is.null(input$pal.col_CBP)){
        mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Blues"))(nb.col) 
      } else {
        # Changed here for a debugging
        mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, input$pal.col_CBP))(nb.col)
      }
      #########################
      y_limits<- shiny::reactive({
        min_val <- min(input$y_min)
        max_val <- max(data_cbp$yvar)
        c(min_val, max_val)
      })
      
      ########################
      #mycolors <- colorRampPalette(brewer.pal(8, input$pal.col_CBP))(nb.col) # colorrampallete code
      p <- ggplot2::ggplot(data_cbp, aes(fill = xvar, y = yvar, x = xvar)) + # plot using aesthetics
        geom_bar( stat = "identity",width=input$Barwidth_cbp) + # geometric bar operation is used to give the type of bar plot stacked at dodged.
        scale_fill_manual(values = mycolors) + # apply the color scale to fill the aesthetic of a plot
        xlab(input$xlab_cbp) + # to give labels on x-axis
        ylab(input$ylab_cbp) + # to give labels on y axis
        ggtitle(input$title_cbp) + # to give title of plot
        labs(fill= input$legend_cbp)+
        coord_polar(start = 0)+ #helps in plotting data in a polar coordinate system
        ylim(y_limits())+ #to have circle
        theme(axis.text.x=element_text(angle=input$angle_cbp,color = input$colour2_cbp, vjust = 0.5, hjust=1),
              axis.text.y=element_text(color = input$colour2_cbp))+
        theme(
          axis.text = element_text(face = input$face_cbp,family = input$font_selector_cbp, size=input$size1_cbp)
        )+
        theme(legend.text = element_text(face = input$face_cbp,color = input$colour4_cbp,family = input$font_selector_cbp,size=input$size3_cbp,)
        )+
        theme(axis.title  = element_text(size=input$size2_cbp, family = input$font_selector_cbp, face = input$face_cbp, color = input$colour2_cbp,hjust=0.5)
        )+
        theme(legend.title = element_text(face = input$face_cbp,color = input$colour4_cbp,family = input$font_selector_cbp, size=input$size4_cbp,)
        )+
        theme(plot.title = element_text(size=input$size5_cbp,face = input$face_cbp,hjust = 0.5,family = input$font_selector_cbp,color = input$colour3_cbp)
        )+
        theme(legend.position = input$Legend_Position_CBP)
      
      
      
      
      if (input$submit_CBP > 0) {
        if (is.null(input$theme_cbp)) {
        return(p)
      }
        
        if (input$theme_cbp == "normal") {
          q <- p + theme_bw()
        } else if (input$theme_cbp == "economist") {
          q <- p + ggthemes::theme_economist() 
        } else if (input$theme_cbp == "grey") {
          q <- p + theme_gray() 
        } 
        else if (input$theme_cbp == "minimal") {
          q <- p + theme_minimal()
        }else if (input$theme_cbp == "light") {
          q <- p + theme_light()
        }
        else if (input$theme_cbp == "void") {
          q <- p + theme_void()
        }
        else if (input$theme_cbp == "tufte") {
          q <- p + ggthemes::theme_tufte()
        }
        else if (input$theme_cbp == "stata") {
          q <- p + ggthemes::theme_stata()
        }
        else if (input$theme_cbp == "wsj") {
          q <- p + ggthemes::theme_wsj()
        }
        else if (input$theme_cbp == "calc") {
          q <- p + ggthemes::theme_calc()
        }
        else if (input$theme_cbp == "hc") {
          q <- p + ggthemes::theme_hc()
        }
        
        q<- q+theme(axis.text.x=element_text(angle=input$angle_cbp,color = input$colour2_cbp, vjust = 0.5, hjust=1),
                    axis.text.y=element_text(color = input$colour2_cbp))+
          theme(
            axis.text = element_text(face = input$face_cbp,family = input$font_selector_cbp, size=input$size1_cbp)
          )+
          theme(legend.text = element_text(face = input$face_cbp,color = input$colour4_cbp,family = input$font_selector_cbp,size=input$size3_cbp,)
          )+
          theme(axis.title  = element_text(size=input$size2_cbp, family = input$font_selector_cbp, face = input$face_cbp, color = input$colour2_cbp,hjust=0.5)
          )+
          theme(legend.title = element_text(face = input$face_cbp,color = input$colour4_cbp,family = input$font_selector_cbp, size=input$size4_cbp,)
          )+
          theme(plot.title = element_text(size=input$size5_cbp,face = input$face_cbp,hjust = 0.5,family = input$font_selector_cbp,color = input$colour3_cbp)
          )+
          theme(legend.position = input$Legend_Position_CBP)
        q
      }
      else{
        return(p) 
      }
    }
    
  })
  ################################plot output 
  output$cbp <- shiny::renderPlot( {
    plotInput()
  },
  bg = "transparent"
  ) 
  
  
  ###############color show
  output$colours_CBP <- shiny::renderPlot( {
    if (is.null(input$Show_col_switch_CBP)){return()}
    if (input$Show_col_switch_CBP > 0) {
      par(mar=c(3,4,2,2))
      RColorBrewer::display.brewer.all(n = NULL, type = "all", select = NULL,
                         colorblindFriendly = TRUE)
    }
  },
  bg = "transparent"
  ) 
  
  ################### Download button
  output$image_down_cbp <- shiny::renderUI({
    if (is.null(input$submit_CBP)) {
      return()
    }
    if (input$submit_CBP > 0) {
      list(shiny::downloadButton("downloadImage5",
                          label = "Download Plot", class = "butt1"
      ))
    }
  })
  ############# Download image
  
  output$downloadImage5 <- shiny::downloadHandler(
    filename = "Circularbarplot.png",
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
  output$data_set_CBP = shiny::renderUI({
    
    list(
      shiny::selectInput(
        "filenames_cbp", "Choose a dataset:",
        list.files(
          pattern = c("circular_bar_1.csv|circular_bar_2.csv")
        )
      ),
      shiny::downloadButton("downloadData5", label = "Download csv file", class = "butt5",)
    )
    
    
    
  })
  
  datasetInput = shiny::reactive({
    switch(input$filenames_cbp,
           filenames_cbp
    )
  })
  
  output$downloadData5 = shiny::downloadHandler(
    filename = function() {
      input$filenames_cbp
    },
    content = function(file) {
      write.csv(read.csv
                (input$filenames_cbp, header = TRUE, sep = ","), file, row.names = FALSE)
    }
  )
  
} 