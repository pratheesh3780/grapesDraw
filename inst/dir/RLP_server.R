RLP_server <- function(input, output,session) {
  
  
  
  ##########################rlp
  csvfile_RLP <- shiny::reactive({
    csvfile_RLP <- input$file1_RLP
    if (is.null(csvfile_RLP)){return(NULL)}
    dt_cbp  <- read.csv(csvfile_RLP $datapath, header=input$header, sep=",")
    dt_cbp 
  })
  
  output$var_RLP  <- shiny::renderUI({
    if(is.null(input$file1_RLP$datapath)){
      return()
    }
    else{
      list (shiny::radioButtons("yrlp", "Select the x axis variables (numerical value)", choices =    names(csvfile_RLP())),
            shiny::radioButtons("xrlp", "Select the y axis variables (categorical variable)", choices = names(csvfile_RLP())),
            
            shinyWidgets::actionBttn(
              inputId = "submit_RLP",
              label = "DRAW!",
              color = "danger",
              style = "jelly"
            )
      )
    }
  })
  
  ############## control panel for plots
  output$cp_RLP <- shiny::renderUI({
    if (is.null(input$file1_RLP$datapath)){return()}
    if (is.null(input$submit_RLP)){return()}
    if (input$submit_RLP > 0) {
      list(
        fluidRow(
          column(4,
                 shiny::textInput("xlab_rlp", "Enter required x-axis title", "X-axis")
          ),
          column(4,
                 shiny::textInput("ylab_rlp", "Enter required y-axis title", "Y-axis")
          ),
          column(4,
                 shiny::textInput("title_rlp", "Enter required title", "title")
          ),
          column(4,
                 shiny::textInput("colour1_rlp", "Enter required legend title", "legend")
          ),
          
          
          column(4,
                 shiny::selectInput("pal.col_rlp", "Choose Colour pattern (plot):",
                             list(`Sequential` = list("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"),
                                  `Qualitative` = list("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3"),
                                  `Diverging` = list("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral"))
                 )
          ),
          column(4,
                 shiny::selectInput("Legend_Position_RLP", "Choose legend position:",
                             choices = c("none","right","left","top","bottom"),
                             selected = "right")
          ),
          column(
            4,
            shiny::selectInput(
              "theme_rlp", "Choose your theme:",
              list("normal","economist","minimal","grey","light","void","tufte","stata","wsj","calc","hc")
            )
          ),
          column(3,
                 shinyWidgets::materialSwitch(inputId = "Show_col_switch_RLP", label = "Show available colours", status = "danger")
          ),
          column(
            4,
            shinyWidgets::materialSwitch(inputId = "manual_change_rlp", label = "Show me manual controls", status = "danger")
          )
        )
        
      )
      
    }
  })
  
  
  
  
  #################manual changes of the plot
  
  output$manual_RLP <- shiny::renderUI({
    if (is.null(input$file1_RLP$datapath)) {
      return()
    }
    if (is.null(input$submit_RLP)) {
      return()
    }
    if (is.null(input$manual_change_rlp)) {
      return()
    }
    if (input$submit_RLP > 0 && input$manual_change_rlp > 0) {
      list(
        fluidRow(
          
          column(4,
                 shiny::selectInput("face_rlp", "select font face",
                             choices = c("plain","italic","bold","bold.italic"),
                             selected = "plain")
          ),
          column(4,
                 shiny::selectInput("font_selector_rlp", "select a font",
                             choices = c(
                               "Arial" ,"Calibri","Georgia","Helvetica" ,"Palatino","Garamond",
                               "Times New Roman" ,"Baskerville","Courier New","Verdana","Century Schoolbook",
                               "Liberation Serif"
                             ),
                             selected = "Arial")
          ),
          column(4,
                 shiny::selectInput("colour2_rlp","choose colour of axis title and label:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(4,
                 shiny::selectInput("colour4_rlp","choose colour of legend title and label:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(4,
                 shiny::selectInput("colour3_rlp","choose colour of plot title:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(5,
                 shiny::sliderInput("size1_rlp", "axis label size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 shiny::sliderInput("size2_rlp", "axis title size:",
                             min = 10, max = 20, value = 10
                 )
          ),
          column(5,
                 shiny::sliderInput("size3_rlp", "legend label size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 shiny::sliderInput("size4_rlp", "legend title size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 shiny::sliderInput("size5_rlp", "plot title size:",
                             min = 15, max = 30, value = 15)
          ),
          column(5,
                 shiny::sliderInput("angle_rlp","required angle of x-axis labels (degrees)",min=0, max=135, value=0, step = 15)
          )
          
          
        )
      )
    }
  }) 
  
  ############### plotting
  plotInput <- shiny::reactive({
    if (is.null(input$file1_RLP$datapath)) {
      return()
    }
    if (is.null(input$submit_RLP)) {
      return()
    }
    if (input$submit_RLP > 0) {
      y <- as.matrix(csvfile_RLP()[, input$xrlp])
      x <- as.matrix(csvfile_RLP()[, input$yrlp])
      
      
      data_rlp <- data.frame(
        value = x,
        text = y
      )
      
      data_rlp$text<- factor(data_rlp$text, levels = unique(data_rlp$text)) ### sorting factors bcz T1, T2 etc can cause problem
      #data_rlp$colour <- factor(data_rlp$colour, levels = unique(data_rlp$colour))
      nb.col <- nlevels(data_rlp$text) # number of colouring factors
      
      if (is.null(input$pal.col_rlp)){
        mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Blues"))(nb.col) 
      } else {
        # Changed here for a debugging
        mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, input$pal.col_rlp))(nb.col)
      }
      
      ########################
      #mycolors <- colorRampPalette(brewer.pal(8, input$pal.col_rlp))(nb.col) # colorrampallete code
      p <- ggplot2::ggplot(data_rlp, aes(fill = text, y = text, x = value)) + # plot using aesthetics
        #geom_bar( stat = "identity",width=input$Barwidth_cbp) + # geometric bar operation is used to give the type of bar plot stacked at dodged.
        geom_density_ridges(alpha=1)+
        scale_fill_manual(values = mycolors) + # apply the color scale to fill the aesthetic of a plot
        xlab(input$xlab_rlp) + # to give labels on x-axis
        ylab(input$ylab_rlp) + # to give labels on y axis
        ggtitle(input$title_rlp) + # to give title of plot
        labs(fill= input$colour1_rlp)+
        #coord_polar(start = 0)+ #helps in plotting data in a polar coordinate system
        #ylim(y_limits())+ #to have circle
        theme(axis.text.x=element_text(angle=input$angle_rlp,color = input$colour2_rlp, vjust = 0.5),
              axis.text.y=element_text(color = input$colour2_rlp))+
        theme(
          axis.text = element_text(face = input$face_rlp,family = input$font_selector_rlp, size=input$size1_rlp)
        )+
        theme(legend.text = element_text(face = input$face_rlp,color = input$colour4_rlp,family = input$font_selector_rlp,size=input$size3_rlp,)
        )+
        theme(axis.title  = element_text(size=input$size2_rlp, family = input$font_selector_rlp, face = input$face_rlp, color = input$colour2_rlp,hjust=0.5)
        )+
        theme(legend.title = element_text(face = input$face_rlp,color = input$colour4_rlp,family = input$font_selector_rlp, size=input$size4_rlp,)
        )+
        theme(plot.title = element_text(size=input$size5_rlp,face = input$face_rlp,hjust = 0.5,family = input$font_selector_rlp,color = input$colour3_rlp)
        )+
        theme(legend.position = input$Legend_Position_RLP)+
        theme(panel.grid = element_blank(),
              panel.background = element_rect(fill = "white"))
      
      
      
      
      if (input$submit_RLP>0) {
        if (is.null(input$theme_rlp)) {
          return(p)
        }
        if (input$theme_rlp == "normal") {
          q <- p + theme_bw()
        } else if (input$theme_rlp == "economist") {
          q <- p + ggthemes::theme_economist() 
        } else if (input$theme_rlp == "grey") {
          q <- p + theme_gray() 
        } 
        else if (input$theme_rlp == "minimal") {
          q <- p + theme_minimal()
        }else if (input$theme_rlp == "light") {
          q <- p + theme_light()
        }
        else if (input$theme_rlp == "void") {
          q <- p + theme_void()
        }
        else if (input$theme_rlp == "tufte") {
          q <- p + ggthemes::theme_tufte()
        }
        else if (input$theme_rlp == "stata") {
          q <- p + ggthemes::theme_stata()
        }
        else if (input$theme_rlp == "wsj") {
          q <- p + ggthemes::theme_wsj()
        }
        else if (input$theme_rlp == "calc") {
          q <- p + ggthemes::theme_calc()
        }
        else if (input$theme_rlp == "hc") {
          q <- p + ggthemes::theme_hc()
        }
        
        q<- q+theme(legend.position = input$Legend_Position_RLP)+
          theme(axis.text.x=element_text(angle=input$angle_rlp,color = input$colour2_rlp, vjust = 0.5),
                axis.text.y=element_text(color = input$colour2_rlp))+
          theme(
            axis.text = element_text(face = input$face_rlp,family = input$font_selector_rlp, size=input$size1_rlp)
          )+
          theme(legend.text = element_text(face = input$face_rlp,color = input$colour4_rlp,family = input$font_selector_rlp,size=input$size3_rlp,)
          )+
          theme(axis.title  = element_text(size=input$size2_rlp, family = input$font_selector_rlp, face = input$face_rlp, color = input$colour2_rlp,hjust=0.5)
          )+
          theme(legend.title = element_text(face = input$face_rlp,color = input$colour4_rlp,family = input$font_selector_rlp, size=input$size4_rlp,)
          )+
          theme(plot.title = element_text(size=input$size5_rlp,face = input$face_rlp,hjust = 0.5,family = input$font_selector_rlp,color = input$colour3_rlp)
          )
        q
      }
      else{
        return(p) 
      }
    }
    
  })
  ################################plot output 
  output$rlp <- shiny::renderPlot( {
    plotInput()
  },
  bg = "transparent"
  ) 
  
  
  ###############color show
  output$colours_RLP <- shiny::renderPlot( {
    if (is.null(input$Show_col_switch_RLP)){return()}
    if (input$Show_col_switch_RLP > 0) {
      par(mar=c(3,4,2,2))
      RColorBrewer::display.brewer.all(n = NULL, type = "all", select = NULL,
                         colorblindFriendly = TRUE)
    }
  },
  bg = "transparent"
  ) 
  
  ################### Download button
  output$image_down_rlp <- shiny::renderUI({
    if (is.null(input$submit_RLP)) {
      return()
    }
    if (input$submit_RLP > 0) {
      list(shiny::downloadButton("downloadImage8",
                          label = "Download Plot", class = "butt1"
      ))
    }
  })
  ############# Download image
  
  output$downloadImage8 <- shiny::downloadHandler(
    filename = "Ridgeline plot.png",
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
  output$data_set_RLP = shiny::renderUI({
    
    
    
    list(
      shiny::selectInput(
        "filenames_rlp", "Choose a dataset:",
        list.files(
          pattern = c("ridgeline_plot_1.csv|ridgeline_plot_2.csv")
        )
      ),
      shiny::downloadButton("downloadData8", label = "Download csv file", class = "butt8",)
    )
    
    
    
  })
  
  datasetInput = shiny::reactive({
    switch(input$filenames_rlp,
           filenames_rlp
    )
  })
  
  output$downloadData8 = shiny::downloadHandler(
    filename = function() {
      input$filenames_rlp
    },
    content = function(file) {
      write.csv(read.csv
                (input$filenames_rlp, header = TRUE, sep = ","), file, row.names = FALSE)
    }
  )
  ######################### end data set download 
  
}
