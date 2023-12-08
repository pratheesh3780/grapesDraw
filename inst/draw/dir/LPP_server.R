LPP_server <- function(input, output,session) {
  
  
  
  ########################## MBP 
  csvfile_LPP <- shiny::reactive({
    csvfile_LPP <- input$file1_LPP
    if (is.null(csvfile_LPP)){return(NULL)}
    dt_MBP  <- read.csv(csvfile_LPP $datapath, header=input$header_LPP, sep=",")
    dt_MBP 
  })
  
  output$var_LPP  <- shiny::renderUI({
    if(is.null(input$file1_LPP$datapath)){
      return()
    }
    list (shiny::radioButtons("xlpp", "Select the x axis variables", choices =    names(csvfile_LPP())),
          shiny::radioButtons("ylpp", "Select the y axis variables (quantitative variable)", choices = names(csvfile_LPP())),
          shinyWidgets::actionBttn(
            inputId = "submit_LPP",
            label = "DRAW!",
            color = "danger",
            style = "jelly"
          )
    )
    
  })
  ############## control panel for plots
  output$cp_LPP <- shiny::renderUI({
    if (is.null(input$file1_LPP$datapath)){return()}
    if (is.null(input$submit_LPP)){return()}
    if (input$submit_LPP > 0) {
      list(
        fluidRow(
          column(4,
                 shiny::textInput("xlab_lpp", "Enter required x-axis title", "X-axis")
          ),
          column(4,
                 shiny::textInput("ylab_lpp", "Enter required y-axis title", "Y-axis")
          ),
          column(4,
                 shiny::textInput("title_lpp", "Enter required title", "title")
          ),
          column(4,
                 shiny::textInput("legend_lpp", "Enter required legend title", "legend")
          ),
          
          
          column(4,
                 shiny::selectInput("pal.col_LPP", "Choose gradient Colour pattern (plot):",
                             list(`Sequential` = list("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"),
                                  `Qualitative` = list("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3"),
                                  `Diverging` = list("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral"))
                 )
          ),
          column(4,
                 shiny::selectInput("Legend_Position_LPP", "Choose legend position:",
                             choices = c("none","right","left","top","bottom"),
                             selected = "right")
          ),
          column(
            4,
            shiny::selectInput(
              "theme_lpp", "Choose your theme:",
              list("normal","economist","minimal","grey","light","void","tufte","stata","wsj","calc","hc")
            )
          ),
          column(3,
                 shinyWidgets::materialSwitch(inputId = "Show_col_switch_LPP", label = "Show available colours", status = "danger")
          ),
          column(
            4,
            shinyWidgets::materialSwitch(inputId = "manual_change_lpp", label = "Show me manual controls", status = "danger")
          )
        )
        
      )
      
    }
  })
  
  
  
  #################manual changes of the plot
  
  output$manual_LPP <- shiny::renderUI({
    if (is.null(input$file1_LPP$datapath)) {
      return()
    }
    if (is.null(input$submit_LPP)) {
      return()
    }
    if (is.null(input$manual_change_lpp)) {
      return()
    }
    if (input$submit_LPP > 0 && input$manual_change_lpp > 0) {
      list(
        fluidRow(
          
          column(4,
                 shiny::selectInput("face_lpp", "select font face",
                             choices = c("plain","italic","bold","bold.italic"),
                             selected = "plain")
          ),
          column(4,
                 shiny::selectInput("font_selector_lpp", "select a font",
                             choices = c(
                               "Arial" ,"Calibri","Georgia","Helvetica" ,"Palatino","Garamond",
                               "Times New Roman" ,"Baskerville","Courier New","Verdana","Century Schoolbook",
                               "Liberation Serif"
                             ),
                             selected = "Arial")),
          
          column(4,
                 shiny::selectInput("colour2_lpp","choose colour of axis title and label:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(4,
                 shiny::selectInput("colour4_lpp","choose colour of legend title and label:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(4,
                 shiny::selectInput("colour3_lpp","choose colour of plot title:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          
          
          column(5,
                 shiny::sliderInput("size1_lpp", "axis label size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 shiny::sliderInput("size2_lpp", "axis title size:",
                             min = 10, max = 20, value = 10
                 )
          ),
          column(5,
                 shiny::sliderInput("size3_lpp", "legend label size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 shiny::sliderInput("size4_lpp", "legend title size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 shiny::sliderInput("size5_lpp", "plot title size:",
                             min = 15, max = 30, value = 15)
          ),
          
          column(5,
                 shiny::sliderInput("angle_lpp","required angle of x-axis labels (degrees)",min=0, max=135, value= 0, step = 15)
          )
        )
      )
    }
  }) 
  
  ############### plotting
  plotInput <- shiny::reactive({
    if (is.null(input$file1_LPP$datapath)) {
      return()
    }
    if (is.null(input$submit_LPP)) {
      return()
    }
    if (input$submit_LPP > 0) {
      x <- as.matrix(csvfile_LPP()[, input$xlpp])
      y <- as.matrix(csvfile_LPP()[, input$ylpp])
      
      data_lpp<- data.frame(
        xvar = x,
        yvar = y
      )
      
      data_lpp$xvar <- factor(data_lpp$xvar, levels = unique(data_lpp$xvar)) ### sorting factors bcz T1, T2 etc can cause problem
      #data_lpp$colour <- factor(data_lpp$colour, levels = unique(data_lpp$colour))
      nb.col <- nlevels(data_lpp$xvar) # number of colouring factors
      
      if (is.null(input$pal.col_LPP)){
        mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Blues"))(nb.col) 
      } else {
        # Changed here for a debugging
        mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, input$pal.col_LPP))(nb.col)
      }
      #mycolors <- colorRampPalette(brewer.pal(8, input$pal.col_LPP))(nb.col) # colorrampallete code
      p <- ggplot2::ggplot(data_lpp, aes(fill = xvar, y = yvar, x = xvar)) + # plot using aesthetics
        geom_segment( aes(x=xvar,xend=xvar,  y=0, yend=yvar)) + # creating line segments in a plot
        geom_point( size = 5, alpha=0.7, shape=21, stroke=1)+ # used for creating scatter plots or adding individual data points to an existing plot 
        scale_fill_manual(values = mycolors)+
        xlab(input$xlab_lpp)+# to give labels on x-axis
        ylab(input$ylab_lpp)+# to give labels on y axis
        ggtitle(input$title_lpp)+  #to give title of plot
        labs(fill= input$legend_lpp)+
        theme(axis.text.x=element_text(angle = input$angle_lpp,color = input$colour2_lpp, vjust = 0.5, hjust = 0.5),
              axis.text.y=element_text(color = input$colour2_lpp))+
        theme(
          axis.text = element_text(face = input$face_lpp, family = input$font_selector_lpp, size=input$size1_lpp)
        )+
        theme(legend.text = element_text(face = input$face_lpp,color = input$colour4_lpp,family = input$font_selector_lpp,size=input$size3_lpp,)
        )+
        theme(axis.title  = element_text(size = input$size2_lpp, family = input$font_selector_lpp, face = input$face_lpp, color = input$colour2_lpp,hjust=0.5)
        )+
        theme(legend.title = element_text(face = input$face_lpp,color = input$colour4_lpp,family = input$font_selector_lpp, size=input$size4_lpp,)
        )+
        theme(legend.position = input $ Legend_Position_LPP)+
        theme(plot.title = element_text(size = input$size5_lpp,face = input$face_lpp,family = input$font_selector_lpp,hjust = 0.5,color = input$colour3_lpp)
        )
      
      
      
      #############################  
      
      
      if (input$submit_LPP>0) {
        if (is.null(input$theme_lpp)) {
          return(p)
        }
        if (input$theme_lpp == "normal") {
          q <- p + theme_bw()
        } else if (input$theme_lpp == "economist") {
          q <- p + ggthemes::theme_economist() 
        } else if (input$theme_lpp == "grey") {
          q <- p + theme_gray() 
        } 
        else if (input$theme_lpp == "minimal") {
          q <- p + theme_minimal()
        }else if (input$theme_lpp == "light") {
          q <- p + theme_light()
        }
        else if (input$theme_lpp == "void") {
          q <- p + theme_void()
        }
        else if (input$theme_lpp == "tufte") {
          q <- p + ggthemes::theme_tufte()
        }
        else if (input$theme_lpp == "stata") {
          q <- p + ggthemes::theme_stata()
        }
        else if (input$theme_lpp == "wsj") {
          q <- p + ggthemes::theme_wsj()
        }
        else if (input$theme_lpp == "calc") {
          q <- p + ggthemes::theme_calc()
        }
        else if (input$theme_lpp == "hc") {
          q <- p + ggthemes::theme_hc()
        }
        q<- q+theme(axis.text.x=element_text(angle = input$angle_lpp,color = input$colour2_lpp, vjust = 0.5, hjust = 0.5),
                    axis.text.y=element_text(color = input$colour2_lpp))+
          theme(
            axis.text = element_text(face = input$face_lpp, family = input$font_selector_lpp, size=input$size1_lpp)
          )+
          theme(legend.text = element_text(face = input$face_lpp,color = input$colour4_lpp,family = input$font_selector_lpp,size=input$size3_lpp,)
          )+
          theme(axis.title  = element_text(size = input$size2_lpp, family = input$font_selector_lpp, face = input$face_lpp, color = input$colour2_lpp,hjust=0.5)
          )+
          theme(legend.title = element_text(face = input$face_lpp,color = input$colour4_lpp,family = input$font_selector_lpp, size=input$size4_lpp,)
          )+
          theme(legend.position = input $ Legend_Position_LPP)+
          theme(plot.title = element_text(size = input$size5_lpp,face = input$face_lpp,family = input$font_selector_lpp,hjust = 0.5,color = input$colour3_lpp)
          )
        q
      }
      else{
        return(p) 
      }
    }
    
  })
  ################################plot output 
  output$lpp <- shiny::renderPlot( {
    plotInput()
  },
  bg = "transparent"
  ) 
  
  
  ###############color show
  output$colours_LPP <- shiny::renderPlot( {
    if (is.null(input$Show_col_switch_LPP)){return()}
    if (input$Show_col_switch_LPP > 0) {
      par(mar=c(3,4,2,2))
      RColorBrewer::display.brewer.all(n = NULL, type = "all", select = NULL,
                         colorblindFriendly = TRUE)
    }
  },
  bg = "transparent"
  ) 
  
   
  ################### Download button
  output$image_down_lpp <- shiny::renderUI({
    if (is.null(input$submit_LPP)) {
      return()
    }
    if (input$submit_LPP > 0) {
      list(shiny::downloadButton("downloadImage6",
                          label = "Download Plot", class = "butt1"
      ))
    }
  })
  ############# Download image
  
  output$downloadImage6 <- shiny::downloadHandler(
    filename = "Lollipop chart.png",
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
  output$data_set_LPP = shiny::renderUI({
    
    
    
    list(
      shiny::selectInput(
        "filenames_lpp", "Choose a dataset:",
        list.files(
          pattern = c("lollipop_chart_1.csv|lollipop_chart_2.csv")
        )
      ),
      shiny::downloadButton("downloadData6", label = "Download csv file", class = "butt6",)
    )
    
    
    
  })
  
  datasetInput = shiny::reactive({
    switch(input$filenames_lpp,
           filenames_lpp
    )
  })
  
  output$downloadData6 = shiny::downloadHandler(
    filename = function() {
      input$filenames_lpp
    },
    content = function(file) {
      write.csv(read.csv
                (input$filenames_lpp, header = TRUE, sep = ","), file, row.names = FALSE)
    }
  )
  ######################### end data set download
}