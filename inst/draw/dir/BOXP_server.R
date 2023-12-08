BOXP_server <- function(input, output,session) {
  
  
  
  ########################## boxp 
  csvfile_BOXP <- shiny::reactive({
    csvfile_BOXP <- input$file1_BOXP
    if (is.null(csvfile_BOXP)){return(NULL)}
    dt_boxp  <- read.csv(csvfile_BOXP $datapath, header=input$header_BOXP, sep=",")
    dt_boxp 
  })
  
  output$var_BOXP  <- shiny::renderUI({
    if(is.null(input$file1_BOXP$datapath)){
      return()
    }
    else{
      list (shiny::radioButtons("xboxp", "Select the x axis variables", choices =    names(csvfile_BOXP())),
            shiny::radioButtons("yboxp", "Select the y axis variable", choices = names(csvfile_BOXP())),
            shinyWidgets::actionBttn(
              inputId = "submit_BOXP",
              label = "DRAW!",
              color = "danger",
              style = "jelly"
            )
      )
    }
  })
  ############## control panel for plots
  output$cp_BOXP <- shiny::renderUI({
    if (is.null(input$file1_BOXP$datapath)){return()}
    if (is.null(input$submit_BOXP)){return()}
    if (input$submit_BOXP > 0) {
      list(
        fluidRow(
          column(4,
                 shiny::textInput("xlab_boxp", "Enter required x-axis title", "X-axis")
          ),
          column(4,
                 shiny::textInput("ylab_boxp", "Enter required y-axis title", "Y-axis")
          ),
          column(4,
                 shiny::textInput("title_boxp", "Enter required title", "title")
          ),
          column(4,
                 shiny::textInput("colour1_boxp", "Enter required legend title", "legend")
          ),
          
          
          column(4,
                 shiny::selectInput("pal.col_BOXP", "Choose Colour pattern (plot):",
                             list(`Sequential` = list("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"),
                                  `Qualitative` = list("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3"),
                                  `Diverging` = list("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral"))
                 )
          ),
          
          column(4,
                 shiny::selectInput("Legend_Position_BOXP", "Choose legend position:",
                             choices = c("none","right","left","top","bottom"),
                             selected = "right")
          ),
          column(
            4,
            shiny::selectInput(
              "theme_boxp", "Choose your theme:",
              list("normal","economist","minimal","grey","light","void","tufte","stata","wsj","calc","hc")
            )
          ),
          column(3,
                 shinyWidgets::materialSwitch(inputId = "Show_col_switch_BOXP", label = "Show available colours", status = "danger")
          ),
          
          column(
            4,
            shinyWidgets::materialSwitch(inputId = "manual_change_boxp", label = "Show me manual controls", status = "danger")
          )
        )
        
      )
      
    }
  })
  
  
  
  #################manual changes of the plot
  
  output$manual_BOXP <- shiny::renderUI({
    if (is.null(input$file1_BOXP$datapath)) {
      return()
    }
    if (is.null(input$submit_BOXP)) {
      return()
    }
    if (is.null(input$manual_change_boxp)) {
      return()
    }
    if (input$submit_BOXP > 0 && input$manual_change_boxp > 0) {
      list(
        fluidRow(
          
          column(4,
                 shiny::selectInput("face_boxp", "select font face",
                             choices = c("plain","italic","bold","bold.italic"),
                             selected = "plain")
          ),
          column(4,
                 shiny::selectInput("font_selector_boxp", "select a font",
                             choices = c(
                               "Arial" ,"Calibri","Georgia","Helvetica" ,"Palatino","Garamond",
                               "Times New Roman" ,"Baskerville","Courier New","Verdana","Century Schoolbook",
                               "Liberation Serif"
                             ),
                             selected = "Arial")),
          column(4,
                 shiny::selectInput("colour2_boxp","choose colour of axis title and label:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(4,
                 shiny::selectInput("colour4_boxp","choose colour of legend title and label:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(4,
                 shiny::selectInput("colour3_boxp","choose colour of plot title:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(5,
                 shiny::sliderInput("size1_boxp", "axis label size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 shiny::sliderInput("size2_boxp", "axis title size:",
                             min = 10, max = 20, value = 10
                 )
          ),
          column(5,
                 shiny::sliderInput("size3_boxp", "legend label size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 shiny::sliderInput("size4_boxp", "legend title size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 shiny::sliderInput("size5_boxp", "plot title size:",
                             min = 15, max = 30, value = 15)
          ),
          column(5,
                 shiny::sliderInput("angle_boxp","required angle of x-axis labels (degrees)",min=0, max=135, value= 0, step = 15)
          )
          
          
        )
      )
    }
  }) 
  
  ############### plotting
  plotInput <- shiny::reactive({
    if (is.null(input$file1_BOXP$datapath)) {
      return()
    }
    if (is.null(input$submit_BOXP)) {
      return()
    }
    if (input$submit_BOXP > 0) {
      x <- as.matrix(csvfile_BOXP()[, input$xboxp])
      y <- as.matrix(csvfile_BOXP()[, input$yboxp])
      
      data_boxp <- data.frame(
        xvar = x,
        yvar = y
      )
      
      data_boxp$xvar<-factor(data_boxp$xvar, levels = unique(data_boxp$xvar)) ### sorting factors bcz T1, T2 etc can cause problem
      #data_boxp$yvar<-factor(data_boxp$yvar, levels = unique(data_boxp$yvar))
      nb.col<-nlevels(data_boxp$xvar)# number of colouring factors
      
      if (is.null(input$pal.col_BOXP)){
        mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Blues"))(nb.col) 
      } else {
        # Changed here for a debugging
        mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, input$pal.col_BOXP))(nb.col)
      }
      
      
      
      #mycolors <- colorRampPalette(brewer.pal(8, input$pal.col_BOXP))(nb.col) # colorrampallete code
      p <-ggplot2::ggplot(data_boxp, aes(fill=xvar, y=yvar, x=xvar)) + #plot using aesthetics
        geom_boxplot() +
        geom_jitter(size = 0.4, alpha=0.9)+
        scale_fill_manual(values = mycolors) +#apply the color scale to fill the aesthetic of a plot
        xlab(input$xlab_boxp)+# to give labels on x-axis
        ylab(input$ylab_boxp)+# to give labels on y axis
        ggtitle(input$title_boxp)+  #to give title of plot
        #theme_ipsum() + ##to provide  set of pre-defined theme
        labs(fill= input$colour1_boxp)+
        theme(axis.text.x=element_text(angle=input$angle_boxp,color = input$colour2_boxp, vjust = 0.5),
              axis.text.y=element_text(color = input$colour2_boxp),
        )+
        theme(
          axis.text = element_text(face = input$face_boxp,family = input$font_selector_boxp, size=input$size1_boxp)
        )+
        theme(legend.text = element_text(face = input$face_boxp,color = input$colour4_boxp,family = input$font_selector_boxp,size=input$size3_boxp,)
        )+
        theme(axis.title  = element_text(size=input$size2_boxp, family = input$font_selector_boxp, face = input$face_boxp, color = input$colour2_boxp,hjust=0.5)
        )+
        theme(legend.title = element_text(face = input$face_boxp,color = input$colour4_boxp,family = input$font_selector_boxp, size=input$size4_boxp,)
        )+
        theme(plot.title = element_text(size=input$size5_boxp,face = input$face_boxp, family = input$font_selector_boxp, hjust = 0.5,color = input$colour3_boxp)
        )+
        theme(legend.position = input$Legend_Position_BOXP)
      
      
      
      #############################  
       
      if (input$submit_BOXP > 0) {
        if (is.null(input$theme_boxp)) {
          return(p)
        }
        if (input$theme_boxp == "normal") {
          q <- p + theme_bw()
        } else if (input$theme_boxp == "economist") {
          q <- p + ggthemes::theme_economist() 
        } else if (input$theme_boxp == "grey") {
          q <- p + theme_gray() 
        } 
        else if (input$theme_boxp == "minimal") {
          q <- p + theme_minimal()
        }else if (input$theme_boxp == "light") {
          q <- p + theme_light()
        }
        else if (input$theme_boxp == "void") {
          q <- p + theme_void()
        }
        else if (input$theme_boxp == "tufte") {
          q <- p + ggthemes::theme_tufte()
        }
        else if (input$theme_boxp == "stata") {
          q <- p + ggthemes::theme_stata()
        }
        else if (input$theme_boxp == "wsj") {
          q <- p + ggthemes::theme_wsj()
        }
        else if (input$theme_boxp == "calc") {
          q <- p + ggthemes::theme_calc()
        }
        else if (input$theme_boxp == "hc") {
          q <- p + ggthemes::theme_hc()
        }
        q<- q+theme(legend.position = input$Legend_Position_BOXP)+
          theme(axis.text.x=element_text(angle=input$angle_boxp,color = input$colour2_boxp, vjust = 0.5),
                axis.text.y=element_text(color = input$colour2_boxp),
          )+
          theme(
            axis.text = element_text(face = input$face_boxp,family = input$font_selector_boxp, size=input$size1_boxp)
          )+
          theme(legend.text = element_text(face = input$face_boxp,color = input$colour4_boxp,family = input$font_selector_boxp,size=input$size3_boxp,)
          )+
          theme(axis.title  = element_text(size=input$size2_boxp, family = input$font_selector_boxp, face = input$face_boxp, color = input$colour2_boxp,hjust=0.5)
          )+
          theme(legend.title = element_text(face = input$face_boxp,color = input$colour4_boxp,family = input$font_selector_boxp, size=input$size4_boxp,)
          )+
          theme(plot.title = element_text(size=input$size5_boxp,face = input$face_boxp, family = input$font_selector_boxp, hjust = 0.5,color = input$colour3_boxp)
          )
        q
      }
      else{
        return(p) 
      }
    }
    
  })
  ################################plot output 
  output$boxp <- shiny::renderPlot( {
    plotInput()
  },
  bg = "transparent"
  ) 
  
  
  ###############color show
  output$colours_BOXP <- shiny::renderPlot( {
    if (is.null(input$Show_col_switch_BOXP)){return()}
    if (input$Show_col_switch_BOXP > 0) {
      par(mar=c(3,4,2,2))
      RColorBrewer::display.brewer.all(n = NULL, type = "all", select = NULL,
                         colorblindFriendly = TRUE)
    }
  },
  bg = "transparent"
  ) 
  
  
  ################### Download button
  output$image_down_boxp <- shiny::renderUI({
    if (is.null(input$submit_BOXP)) {
      return()
    }
    if (input$submit_BOXP > 0) {
      list(shiny::downloadButton("downloadImage7",
                          label = "Download Plot", class = "butt1"
      ))
    }
  })
  ############# Download image
  
  output$downloadImage7 <- shiny::downloadHandler(
    filename = "Box plot.png",
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
  output$data_set_BOXP = shiny::renderUI({
    
    list(
      shiny::selectInput(
        "filenames_boxp", "Choose a dataset:",
        list.files(
          pattern = c("boxplot_data_1.csv|boxplot_data_2.csv")
        )
      ),
      shiny::downloadButton("downloadData7", label = "Download csv file", class = "butt7",)
    )
    
    
    
  })
  
  datasetInput = shiny::reactive({
    switch(input$filenames_boxp,
           filenames_boxp
    )
  })
  
  output$downloadData7 = shiny::downloadHandler(
    filename = function() {
      input$filenames_boxp
    },
    content = function(file) {
      write.csv(read.csv
                (input$filenames_boxp, header = TRUE, sep = ","), file, row.names = FALSE)
    }
  )
}