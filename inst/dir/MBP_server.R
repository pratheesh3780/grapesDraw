MBP_server <- function(input, output,session) {
  
  
  
  ########################## MBP 
  csvfile_MBP <- shiny::reactive({
    csvfile_MBP <- input$file1_MBP
    if (is.null(csvfile_MBP)){return(NULL)}
    dt_MBP  <- read.csv(csvfile_MBP $datapath, header=input$header, sep=",")
    dt_MBP 
  })
  
  output$var_MBP  <- shiny::renderUI({
    if(is.null(input$file1_MBP$datapath)){
      return()
    }
    else{
      list (shiny::radioButtons("xmbp", "Select the x axis variables", choices =    names(csvfile_MBP())),
            shiny::radioButtons("cmbp", "Select the colouring variable", choices = names(csvfile_MBP())),
            shiny::radioButtons("vmbp", "Select the observations (quantitative variable)", choices = names(csvfile_MBP())),
            shinyWidgets::actionBttn(
              inputId = "submit_MBP",
              label = "DRAW!",
              color = "danger",
              style = "jelly"
            )
      )
    }
  })
  ############## control panel for plots
  output$cp_MBP <- shiny::renderUI({
    if (is.null(input$file1_MBP$datapath)){return()}
    if (is.null(input$submit_MBP)){return()}
    if (input$submit_MBP > 0) {
      list(
        fluidRow(
          column(4,
                 shiny::textInput("xlab_mbp", "Enter required x-axis title", "X-axis")
          ),
          column(4,
                 shiny::textInput("ylab_mbp", "Enter required y-axis title", "Y-axis")
          ),
          column(4,
                 shiny::textInput("title_mbp", "Enter required title", "title")
          ),
          column(4,
                 shiny::textInput("legend_mbp", "Enter required legend title", "legend")
          ),
          
          
          column(4,
                 shiny::selectInput("pal.col_MBP", "Choose Colour pattern (plot):",
                             list(`Sequential` = list("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"),
                                  `Qualitative` = list("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3"),
                                  `Diverging` = list("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral"))
                 )
          ), 
          column(4,
                 shiny::selectInput("Legend_Position_MBP", "Choose legend position:",
                             choices = c("none","right","left","top","bottom"),
                             selected = "right")
          ),
          column(
            4,
            shiny::selectInput(
              "theme_mbp", "Choose your theme:",
              list("normal","economist","minimal","grey","light","void","tufte","stata","wsj","calc","hc")
            )
          ),
          column(3,
                 shinyWidgets::materialSwitch(inputId = "Show_col_switch_MBP", label = "Show available colours", status = "danger")
          ),
          column(
            4,
            shinyWidgets::materialSwitch(inputId = "manual_change_mbp", label = "Show me manual controls", status = "danger")
          )
        )
        
      )
      
    }
  })
  
  #################manual changes of the plot
  
  output$manual_change_MBP <- shiny::renderUI({
    if (is.null(input$file1_MBP$datapath)) {
      return()
    }
    if (is.null(input$submit_MBP)) {
      return()
    }
    if (is.null(input$manual_change_mbp)) {
      return()
    }
    if (input$submit_MBP > 0 && input$manual_change_mbp > 0) {
      list(
        fluidRow(
          
          column(4,
                 shiny::selectInput("face_mbp", "select font face",
                             choices = c("plain","italic","bold","bold.italic"),
                             selected = "plain")
          ),
          column(4,
                 shiny::selectInput("font_selector_mbp", "select a font",
                             choices = c(
                               "Arial" ,"Calibri","Georgia","Helvetica" ,"Palatino","Garamond",
                               "Times New Roman" ,"Baskerville","Courier New","Verdana","Century Schoolbook",
                               "Liberation Serif"
                             ),
                             selected = "Arial")),
          column(4,
                 shiny::selectInput("colour2_mbp","choose colour of axis title and label:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(4,
                 shiny::selectInput("colour4_mbp","choose colour of legend title and label:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(4,
                 shiny::selectInput("colour3_mbp","choose colour of plot title:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(5,
                 shiny::sliderInput("size1_mbp", "axis label size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 shiny::sliderInput("size2_mbp", "axis title size:",
                             min = 10, max = 20, value = 10
                 )
          ),
          column(5,
                 shiny::sliderInput("size3_mbp", "legend label size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 shiny::sliderInput("size4_mbp", "legend title size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 shiny::sliderInput("size5_mbp", "plot title size:",
                             min = 15, max = 30, value = 15)
          ),
          column(5,
                 shiny::sliderInput("angle_mbp","required angle of x-axis labels (degrees)",min=0, max=135, value= 0, step = 15)
          ),
          column(5,
                 shiny::sliderInput("Barwidth_mbp","select required bar width",
                             min=0.1, max=1, value=1 )
          )
          
          
        )
      )
    }
  }) 
  
  ############### plotting
  plotInput <- shiny::reactive({
    if (is.null(input$file1_MBP$datapath)) {
      return()
    }
    if (is.null(input$submit_MBP)) {
      return()
    }
    if (input$submit_MBP > 0) {
      x <- as.matrix(csvfile_MBP()[, input$xmbp])
      y <- as.matrix(csvfile_MBP()[, input$vmbp])
      z <- as.matrix(csvfile_MBP()[, input$cmbp])
      
      data_bmp <- data.frame(
        xvar = x,
        colour = z,
        value = y
      )
      
      data_bmp$xvar <- factor(data_bmp$xvar, levels = unique(data_bmp$xvar)) ### sorting factors bcz T1, T2 etc can cause problem
      data_bmp$colour <- factor(data_bmp$colour, levels = unique(data_bmp$colour))
      nb.col <- nlevels(data_bmp$colour) # number of colouring factors
      
      if (is.null(input$pal.col_MBP)){
        mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Blues"))(nb.col) 
      } else {
        # Changed here for a debugging
        mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, input$pal.col_MBP))(nb.col)
      }
      
      
      
      #mycolors <- colorRampPalette(brewer.pal(8, input$pal.col_MBP))(nb.col) # colorrampallete code
      p <- ggplot2::ggplot(data_bmp, aes(fill = colour, y = value, x = xvar)) + # plot using aesthetics
        geom_bar(position = "dodge", stat = "identity",width=input$Barwidth_mbp) + # geometric bar operation is used to give the type of bar plot stacked at dodged.
        scale_fill_manual(values = mycolors) + # apply the color scale to fill the aesthetic of a plot
        xlab(input$xlab_mbp) + # to give labels on x-axis
        ylab(input$ylab_mbp) + # to give labels on y axis
        ggtitle(input$title_mbp) + # to give title of plot
        labs(fill= input$legend_mbp)+
        theme(axis.text.x=element_text(angle=input$angle_mbp,color = input$colour2_mbp, vjust = 0.5),
              axis.text.y=element_text(color = input$colour2_mbp))+
        theme(
          axis.text = element_text(face = input$face_mbp,family = input$font_selector_mbp, size=input$size1_mbp)
        )+
        theme(legend.text = element_text(face = input$face_mbp,color = input$colour4_mbp,family = input$font_selector_mbp,size=input$size3_mbp,)
        )+
        theme(axis.title  = element_text(size=input$size2_mbp, family = input$font_selector_mbp, face = input$face_mbp, color = input$colour2_mbp,hjust=0.5)
        )+
        theme(legend.title = element_text(face = input$face_mbp,color = input$colour4_mbp,family = input$font_selector_mbp, size=input$size4_mbp,)
        )+
        theme(plot.title = element_text(size=input$size5_mbp, family = input$font_selector_mbp, hjust = 0.5,color = input$colour3_mbp,face = input$face_mbp)
        )+
        theme(legend.position=input$Legend_Position_MBP)
      
      
      #############################  
      
      if (input$submit_MBP>0) {
        if (is.null(input$theme_mbp)) {
          return(p)
        }
        if (input$theme_mbp == "normal") {
          q <- p + theme_bw()
        } else if (input$theme_mbp == "economist") {
          q <- p + ggthemes::theme_economist() 
        } else if (input$theme_mbp == "grey") {
          q <- p + theme_gray() 
        } 
        else if (input$theme_mbp == "minimal") {
          q <- p + theme_minimal()
        }else if (input$theme_mbp == "light") {
          q <- p + theme_light()
        }
        else if (input$theme_mbp == "void") {
          q <- p + theme_void()
        }
        else if (input$theme_mbp == "tufte") {
          q <- p + ggthemes::theme_tufte()
        }
        else if (input$theme_mbp == "stata") {
          q <- p + ggthemes::theme_stata()
        }
        else if (input$theme_mbp == "wsj") {
          q <- p + ggthemes::theme_wsj()
        }
        else if (input$theme_mbp == "calc") {
          q <- p + ggthemes::theme_calc()
        }
        else if (input$theme_mbp == "hc") {
          q <- p + ggthemes::theme_hc()
        }
        q<- q+theme(axis.text.x=element_text(angle=input$angle_mbp,color = input$colour2_mbp, vjust = 0.5),
                    axis.text.y=element_text(color = input$colour2_mbp))+
          theme(
            axis.text = element_text(face = input$face_mbp,family = input$font_selector_mbp, size=input$size1_mbp)
          )+
          theme(legend.text = element_text(face = input$face_mbp,color = input$colour4_mbp,family = input$font_selector_mbp,size=input$size3_mbp,)
          )+
          theme(axis.title  = element_text(size=input$size2_mbp, family = input$font_selector_mbp, face = input$face_mbp, color = input$colour2_mbp,hjust=0.5)
          )+
          theme(legend.title = element_text(face = input$face_mbp,color = input$colour4_mbp,family = input$font_selector_mbp, size=input$size4_mbp,)
          )+
          theme(plot.title = element_text(size=input$size5_mbp, family = input$font_selector_mbp, hjust = 0.5,color = input$colour3_mbp,face = input$face_mbp)
          )+
          theme(legend.position=input$Legend_Position_MBP)
        
        q
      }
      else{
        return(p) 
      }
    }
    
  })
  ################################plot output 
  output$mbp <- shiny::renderPlot( {
    plotInput()
  },
  bg = "transparent"
  ) 
  
  
  ###############color show
  output$colours_MBP <- shiny::renderPlot( {
    if (is.null(input$Show_col_switch_MBP)){return()}
    if (input$Show_col_switch_MBP > 0) {
      par(mar=c(3,4,2,2))
      RColorBrewer::display.brewer.all(n = NULL, type = "all", select = NULL,
                         colorblindFriendly = TRUE)
    }
  },
  bg = "transparent"
  ) 
  
  
  
  ################### Download button
  output$image_down_mbp <- shiny::renderUI({
    if (is.null(input$submit_MBP)) {
      return()
    }
    
    if (input$submit_MBP > 0 ) {
      list(shiny::downloadButton("downloadImage3",
                          label = "Download Plot", class = "butt1"
      ))
    }
  })
  ############# Download image
  
  output$downloadImage3 <- shiny::downloadHandler(
    filename = "Multiple bar plot.png",
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
  #########################
  
  ############################# download data set
  output$data_set_MBP = shiny::renderUI({
    
    
    
    list(
      shiny::selectInput(
        "filenames_mbp", "Choose a dataset:",
        list.files(
          pattern = c("multiple_bar_1.csv|multiple_bar_2.csv")
        )
      ),
      shiny::downloadButton("downloadData3", label = "Download csv file", class = "butt3",)
    )
    
    
    
  })
  
  datasetInput = shiny::reactive({
    switch(input$filenames_mbp,
           filenames_mbp
    )
  })
  
  output$downloadData3 = shiny::downloadHandler(
    filename = function() {
      input$filenames_mbp
    },
    content = function(file) {
      write.csv(read.csv
                (input$filenames_mbp, header = TRUE, sep = ","), file, row.names = FALSE)
    }
  )
  ######################### end data set download
}
