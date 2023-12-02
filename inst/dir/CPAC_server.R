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
library(reshape2)
library(packcircles)

CPAC_server <- function(input, output,session) {
  
  
  
  ########################## CPAC 
  csvfile_CPAC <- reactive({
    csvfile_CPAC <- input$file1_CPAC
    if (is.null(csvfile_CPAC)){return(NULL)}
    dt_cpac <- read.csv(csvfile_CPAC $datapath, header=input$header, sep=",")
    dt_cpac
  })
  
  output$var_CPAC  <- renderUI({
    if(is.null(input$file1_CPAC$datapath)){
      return()
    }
    else{
      list (radioButtons("xcpac", "Select the x axis variables", choices =    names(csvfile_CPAC())),
            radioButtons("ycpac", "Select the y axis variables)", choices = names(csvfile_CPAC())),
            actionBttn(
              inputId = "submit_CPAC",
              label = "DRAW!",
              color = "danger",
              style = "jelly"
            )
      )
    }
  })
  
  ############## control panel for plots
  output$cp_CPAC <- renderUI({
    if (is.null(input$file1_CPAC$datapath)){return()}
    if (is.null(input$submit_CPAC)){return()}
    if (input$submit_CPAC > 0) {
      list(
        fluidRow(
          column(4,
                 textInput("xlab_cpac", "Enter required x-axis title", "X-axis")
          ),
          column(4,
                 textInput("ylab_cpac", "Enter required y-axis title", "Y-axis")
          ),
          column(4,
                 textInput("title_cpac", "Enter required title", "title")
          ),
          column(4,
                 textInput("colour1_cpac", "Enter required legend title", "legend")
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
                 selectInput("Legend_Position_CPAC", "Choose legend position:",
                             choices = c("none","right","left","top","bottom"),
                             selected = "right")
          ),
          column(
            4,
            selectInput(
              "theme_cpac", "Choose your theme:",
              list("normal","economist","minimal","grey","light","void","tufte","stata","wsj","calc","hc")
            )
          ),
          column(3,
                 materialSwitch(inputId = "Show_col_switch_CPAC", label = "Show available colours", status = "danger")
          ),
          column(
            4,
            materialSwitch(inputId = "manual_change_CPAC", label = "Show me manual controls", status = "danger")
          )
        )
        
      )
      
    }
  })
  
  
  #################manual changes of the plot
  
  output$manual_CPAC <- renderUI({
    if (is.null(input$file1_CPAC$datapath)) {
      return()
    }
    if (is.null(input$submit_CPAC)) {
      return()
    }
    if (is.null(input$manual_change_CPAC)) {
      return()
    }
    if (input$submit_CPAC > 0 && input$manual_change_CPAC > 0) {
      list(
        fluidRow(
          
          column(4,
                 selectInput("face_cpac", "select font face",
                             choices = c("plain","italic","bold","bold.italic"),
                             selected = "plain")
          ),
          column(4,
                 selectInput("colour2_cpac","choose colour of axis title and label:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(4,
                 selectInput("colour4_cpac","choose colour of legend title and label:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(4,
                 selectInput("colour3_cpac","choose colour of plot title:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          
          column(4,
                 selectInput("font_selector_cpac", "select a font",
                             choices = c(
                               "Arial" ,"Calibri","Georgia","Helvetica" ,"Palatino","Garamond",
                               "Times New Roman" ,"Baskerville","Courier New","Verdana","Century Schoolbook",
                               "Liberation Serif"
                             ),
                             selected = "Arial")),
          
          column(5,
                 sliderInput("size1_cpac", "axis label size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 sliderInput("size2_cpac", "axis title size:",
                             min = 10, max = 20, value = 10
                 )
          ),
          column(5,
                 sliderInput("size3_cpac", "legend label size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 sliderInput("size4_cpac", "legend title size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 sliderInput("size5_cpac", "plot title size:",
                             min = 15, max = 30, value = 15)
          ),
          column(5,
                 sliderInput("angle_cpac","required angle of x-axis labels (degrees)",min=0, max=135, value=0, step = 15)
          )
          
        )
      )
    }
  }) 
  
  ############### plotting
  plotInput <- reactive({
    if (is.null(input$file1_CPAC$datapath)) {
      return()
    }
    if (is.null(input$submit_CPAC)) {
      return()
    }
    if (input$submit_CPAC > 0) {
      x <- as.matrix(csvfile_CPAC()[, input$xcpac])
      y <- as.matrix(csvfile_CPAC()[, input$ycpac])
      
      
      data_cpac <- data.frame( 
        xvar = x,
        yvar = y)
      nb.col <- nlevels(data_cpac$xvar) # number of colouring factors
      
      if (is.null(input$pal.col_CPAC)){
        mycolors <- colorRampPalette(brewer.pal(8, "Blues"))(nb.col) 
      } else {
        # Changed here for a debugging
        mycolors <- colorRampPalette(brewer.pal(8, input$pal.col_CPAC))(nb.col)
      }
      
      packing <- circleProgressiveLayout(data_cpac$y, sizetype='area')
      packing$radius <- 0.95*packing$radius
      data <- cbind(data_cpac, packing)
      dat.gg <- circleLayoutVertices(packing, npoints=50)
      #########################
      
      
      
      ########################
      
      p <- ggplot2::ggplot() + # plot using aesthetics
        geom_polygon(data=dat.gg, aes(x,y,group=id, fill=as.factor(id)))+    
        geom_text(data = data, aes(x,y,size=yvar, label=paste0(xvar)))+
        scale_size_continuous(range = c(2,4))+
        coord_equal()+
        scale_color_manual(values = mycolors ) + # apply the color scale to fill the aesthetic of a plot
        xlab(input$xlab_cpac) + # to give labels on x-axis
        ylab(input$ylab_cpac) + # to give labels on y axis
        ggtitle(input$title_cpac) + # to give title of plot
        labs(fill= input$colour1_cpac)+
        #coord_polar(start = 0)+ #helps in plotting data in a polar coordinate system
        #ylim(y_limits())+ #to have circle
        theme(axis.text.x=element_text(angle=input$angle_cpac,color = input$colour2_cpac, vjust = 0.5),
              axis.text.y=element_text(color = input$colour2_cpac))+
        theme(
          axis.text = element_text(face = input$face_cpac,family = input$font_selector_cpac, size=input$size1_cpac)
        )+
        theme(legend.text = element_text(face = input$face_cpac,color = input$colour4_cpac,family = input$font_selector_cpac,size=input$size3_cpac)
        )+
        theme(axis.title  = element_text(size=input$size2_cpac, family = input$font_selector_cpac, face = input$face_cpac, color = input$colour2_cpac,hjust=0.5)
        )+
        theme(legend.title = element_text(face = input$face_cpac,color = input$colour4_cpac,family = input$font_selector_cpac, size=input$size4_cpac)
        )+
        theme(plot.title = element_text(face = input$face_cpac,size=input$size5_cpac,hjust = 0.5,family = input$font_selector_cpac,color = input$colour3_cpac)
        )+
        theme(legend.position = input$Legend_Position_CPAC)
      
      
      
      
      if (input$submit_CPAC>0) {
        if (is.null(input$theme_cpac)) {
          return(p)
        }
        
        if (input$theme_cpac == "normal") {
          q <- p + theme_bw()
        } else if (input$theme_cpac == "economist") {
          q <- p + ggthemes::theme_economist() 
        } else if (input$theme_cpac == "grey") {
          q <- p + theme_gray() 
        } 
        else if (input$theme_cpac == "minimal") {
          q <- p + theme_minimal()
        }else if (input$theme_cpac == "light") {
          q <- p + theme_light()
        }
        else if (input$theme_cpac == "void") {
          q <- p + theme_void()
        }
        else if (input$theme_cpac == "tufte") {
          q <- p + ggthemes::theme_tufte()
        }
        else if (input$theme_cpac == "stata") {
          q <- p + ggthemes::theme_stata()
        }
        else if (input$theme_cpac == "wsj") {
          q <- p + ggthemes::theme_wsj()
        }
        else if (input$theme_cpac == "calc") {
          q <- p + ggthemes::theme_calc()
        }
        else if (input$theme_cpac == "hc") {
          q <- p + ggthemes::theme_hc()
        }
        
        q<- q+theme(axis.text.x=element_text(angle=input$angle_cpac,color = input$colour2_cpac, vjust = 0.5),
                    axis.text.y=element_text(color = input$colour2_cpac))+
          theme(
            axis.text = element_text(face = input$face_cpac,family = input$font_selector_cpac, size=input$size1_cpac)
          )+
          theme(legend.text = element_text(face = input$face_cpac,color = input$colour4_cpac,family = input$font_selector_cpac,size=input$size3_cpac)
          )+
          theme(axis.title  = element_text(size=input$size2_cpac, family = input$font_selector_cpac, face = input$face_cpac, color = input$colour2_cpac,hjust=0.5)
          )+
          theme(legend.title = element_text(face = input$face_cpac,color = input$colour4_cpac,family = input$font_selector_cpac, size=input$size4_cpac)
          )+
          theme(plot.title = element_text(size=input$size5_cpac,face = input$face_cpac,hjust = 0.5,family = input$font_selector_cpac,color = input$colour3_cpac)
          )+
          theme(legend.position = input$Legend_Position_CPAC)
        
        
        q
      }
      else{
        return(p) 
      }
    }
    
  })
  ################################plot output 
  output$cpac <- renderPlot( {
    plotInput()
  },
  bg = "transparent"
  ) 
  
  
  ###############color show
  output$colours_CPAC <- renderPlot( {
    if (is.null(input$Show_col_switch_CPAC)){return()}
    if (input$Show_col_switch_CPAC > 0) {
      par(mar=c(3,4,2,2))
      display.brewer.all(n = NULL, type = "all", select = NULL,
                         colorblindFriendly = TRUE)
    }
  },
  bg = "transparent"
  ) 
  
  
  
  ################### Download button
  output$image_down_cpac<- renderUI({
    if (is.null(input$submit_CPAC)) {
      return()
    }
    if (input$submit_CPAC > 0) {
      list(downloadButton("downloadImage13",
                          label = "Download Plot", class = "butt1"
      ))
    }
  })
  ############# Download image
  
  output$downloadImage13 <- downloadHandler(
    filename = "Circular packing.png",
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
  output$data_set_CPAC = renderUI({
    
    list(
      selectInput(
        "filenames_cpac", "Choose a dataset:",
        list.files(
          pattern = c("Circular packing 1.csv|Circular packing 2.csv")
        )
      ),
      downloadButton("downloadData13", label = "Download csv file", class = "butt13")
    )
    
    
    
  })
  
  datasetInput = reactive({
    switch(input$filenames_cpac,
           filenames_cpac
    )
  })
  
  output$downloadData13 = downloadHandler(
    filename = function() {
      input$filenames_cpac
    },
    content = function(file) {
      write.csv(read.csv
                (input$filenames_cpac, header = TRUE, sep = ","), file, row.names = FALSE)
    }
  ) 
}