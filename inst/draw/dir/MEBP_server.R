MEBP_server <- function(input, output,session) {
  
  
  
  ########################## MEBP 
  csvfile_MEBP <- shiny::reactive({
    csvfile_MEBP <- input$file1_MEBP
    if (is.null(csvfile_MEBP)){return(NULL)}
    dt_EBP  <- read.csv(csvfile_MEBP $datapath, header=input$header_MEBP, sep=",")
    dt_EBP 
  })
  
  output$var_MEBP  <- shiny::renderUI({
    if(is.null(input$file1_MEBP$datapath)){
      return()
    }
    else{
      list (shiny::radioButtons("xmebp", "Select the x axis variables", choices =    names(csvfile_MEBP())),
            shiny::radioButtons("comebp", "Select the colouring variables", choices =    names(csvfile_MEBP())),
            shiny::radioButtons("ymebp", "Select the y axis variable", choices = names(csvfile_MEBP())),
            shiny::radioButtons("lmebp", "Select the significant letter", choices = names(csvfile_MEBP())),
            shiny::radioButtons("cmebp", "Select the error value (sd/se/ci)", choices = names(csvfile_MEBP())),
            shinyWidgets::actionBttn(
              inputId = "submit_MEBP",
              label = "DRAW!",
              color = "danger",
              style = "jelly"
            )
      )
    }
  })
  ############## control panel for plots
  output$cp_MEBP <- shiny::renderUI({
    if (is.null(input$file1_MEBP$datapath)){return()}
    if (is.null(input$submit_MEBP)){return()}
    if (input$submit_MEBP > 0) {
      list(
        fluidRow(
          column(4,
                 shiny::textInput("xlab_mebp", "Enter required x-axis title", "X-axis")
          ),
          column(4,
                 shiny::textInput("ylab_mebp", "Enter required y-axis title", "Y-axis")
          ),
          column(4,
                 shiny::textInput("title_mebp", "Enter required title", "title")
          ),
          column(4,
                 shiny::textInput("legend_mebp", "Enter required legend title", "legend")
          ),
          
          
          column(4,
                 shiny::selectInput("pal.col_MEBP", "Choose Colour pattern (plot):",
                             list(`Sequential` = list("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"),
                                  `Qualitative` = list("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3"),
                                  `Diverging` = list("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral"))
                 )
          ), 
          column(4,
                 shiny::selectInput("Legend_Position_MEBP", "Choose legend position:",
                             choices = c("none","right","left","top","bottom"),
                             selected = "right")
          ),
          column(
            4,
            shiny::selectInput(
              "theme_mebp", "Choose your theme:",
              list("normal","economist","minimal","grey","light","void","tufte","stata","wsj","calc","hc")
            )
          ),
          column(3,
                 shinyWidgets::materialSwitch(inputId = "Show_col_switch_MEBP", label = "Show available colours", status = "danger")
          ),
          
          column(
            4,
            shinyWidgets::materialSwitch(inputId = "manual_change_MEBP", label = "Show me manual controls", status = "danger")
          )
        )
        
      )
      
    }
  })
  
  #################manual changes of the plot
  
  output$manual_MEBP <- shiny::renderUI({
    if (is.null(input$file1_MEBP$datapath)) {
      return()
    }
    if (is.null(input$submit_MEBP)) {
      return()
    }
    if (is.null(input$manual_change_MEBP)) {
      return()
    }
    if (input$submit_MEBP > 0 && input$manual_change_MEBP > 0) {
      list(
        fluidRow(
          
          column(4,
                 shiny::selectInput("face_mebp", "select font face",
                             choices = c("plain","italic","bold","bold.italic"),
                             selected = "plain")
          ),
          column(4,
                 shiny::selectInput("font_selector_mebp", "select a font",
                             choices = c(
                               "Arial" ,"Calibri","Georgia","Helvetica" ,"Palatino","Garamond",
                               "Times New Roman" ,"Baskerville","Courier New","Verdana","Century Schoolbook",
                               "Liberation Serif"
                             ),
                             selected = "Arial")),
          column(4,
                 shiny::selectInput("colour2_mebp","choose colour of axis title and label:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(4,
                 shiny::selectInput("colour4_mebp","choose colour of legend title and label:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(4,
                 shiny::selectInput("colour3_mebp","choose colour of plot title:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(5,
                 shiny::sliderInput("size1_mebp", "axis label size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 shiny::sliderInput("size2_mebp", "axis title size:",
                             min = 10, max = 20, value = 10
                 )
          ),
          column(5,
                 shiny::sliderInput("size3_mebp", "legend label size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 shiny::sliderInput("size4_mebp", "legend title size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 shiny::sliderInput("size5_mebp", "plot title size:",
                             min = 15, max = 30, value = 15)
          ),
          column(5,
                 shiny::sliderInput("angle_mebp","required angle of x-axis labels (degrees)",min=0, max=135, value= 0, step = 15)
          ) 
        )
      )
    }
  }) 
  
  ############### plotting
  plotInput <- shiny::reactive({
    if (is.null(input$file1_MEBP$datapath)) {
      return()
    }
    if (is.null(input$submit_MEBP)) {
      return()
    }
    if (input$submit_MEBP > 0) {
      x <- as.matrix(csvfile_MEBP()[, input$xmebp])
      u <- as.matrix(csvfile_MEBP()[, input$comebp])
      y <- as.matrix(csvfile_MEBP()[, input$ymebp])
      z <- as.matrix(csvfile_MEBP()[, input$lmebp])
      w <- as.matrix(csvfile_MEBP()[, input$cmebp])
      data_mebp <- data.frame(
        xvar = x,
        letter = z,
        yvar = y,
        ci = w,
        colour = u
      )
      
      data_mebp$xvar <- factor(data_mebp$xvar, levels = unique(data_mebp$xvar)) ### sorting factors bcz T1, T2 etc can cause problem
      data_mebp$colour <- factor(data_mebp$colour, levels = unique(data_mebp$colour))
      nb.col <- nlevels(data_mebp$xvar) # number of colouring factors
      
      if (is.null(input$pal.col_MEBP)){
        mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Blues"))(nb.col) 
      } else {
        # Changed here for a debugging
        mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, input$pal.col_MEBP))(nb.col)
      }
      
      
      
      #mycolors <- colorRampPalette(brewer.pal(8, input$pal.col_MEBP))(nb.col) # colorrampallete code
      p <- ggplot2::ggplot(data_mebp, aes(fill = colour, y = yvar, x = xvar)) + # plot using aesthetics
        geom_bar(
          stat = "identity",
          position = "dodge",## change space between bars
           width = 0.8 # alpha = input trans server crd # width = input width1 server crd
        )+
        geom_errorbar(aes(ymin = yvar - 1.96*ci, ymax = yvar+ 1.96*ci),
                      width = 0.2, colour = "#354543", alpha = 1,
                      position=position_dodge(.8)
        )+
        geom_text(aes(label = letter, y = yvar + ci), vjust = -2.4,position=position_dodge(.9),size=3)+
        scale_fill_manual(values = mycolors) + # apply the color scale to fill the aesthetic of a plot
        xlab(input$xlab_mebp) + # to give labels on x-axis
        ylab(input$ylab_mebp) + # to give labels on y axis
        ggtitle(input$title_mebp) + # to give title of plot
        labs(fill= input$legend_mebp)+
        theme(axis.text.x=element_text(angle=input$angle_mebp,color = input$colour2_mebp, vjust = 0.5),
              axis.text.y=element_text(color = input$colour2_mebp))+
        theme(
          axis.text = element_text(face = input$face_mebp,family = input$font_selector_mebp, size=input$size1_mebp)
        )+
        theme(legend.text = element_text(face = input$face_mebp,color = input$colour4_mebp,family = input$font_selector_mebp,size=input$size3_mebp,)
        )+
        theme(axis.title  = element_text(size=input$size2_mebp, family = input$font_selector_mebp, face = input$face_mebp, color = input$colour2_mebp,hjust=0.5)
        )+
        theme(legend.title = element_text(face = input$face_mebp,color = input$colour4_mebp,family = input$font_selector_mebp, size=input$size4_mebp,)
        )+
        theme(plot.title = element_text(size=input$size5_mebp,face = input$face_mebp, family = input$font_selector_mebp, hjust = 0.5,color = input$colour3_mebp),
        )+
        theme(legend.position=input$Legend_Position_MEBP)
      
      
      #############################  
      if (input$submit_MEBP>0) {
        if (is.null(input$theme_mebp)) {
          return(p)
        }
        if (input$theme_mebp == "normal") {
          q <- p + theme_bw()
        } else if (input$theme_mebp == "economist") {
          q <- p + ggthemes::theme_economist() 
        } else if (input$theme_mebp == "grey") {
          q <- p + theme_gray() 
        } 
        else if (input$theme_mebp == "minimal") {
          q <- p + theme_minimal()
        }else if (input$theme_mebp == "light") {
          q <- p + theme_light()
        }
        else if (input$theme_mebp == "void") {
          q <- p + theme_void()
        }
        else if (input$theme_mebp == "tufte") {
          q <- p + ggthemes::theme_tufte()
        }
        else if (input$theme_mebp == "stata") {
          q <- p + ggthemes::theme_stata()
        }
        else if (input$theme_mebp == "wsj") {
          q <- p + ggthemes::theme_wsj()
        }
        else if (input$theme_mebp == "calc") {
          q <- p + ggthemes::theme_calc()
        }
        else if (input$theme_mebp == "hc") {
          q <- p + ggthemes::theme_hc()
        }
        q <- q+theme(axis.text.x=element_text(angle=input$angle_mebp,color = input$colour2_mebp, vjust = 0.5),
                     axis.text.y=element_text(color = input$colour2_mebp))+
          theme(
            axis.text = element_text(face = input$face_mebp,family = input$font_selector_mebp, size=input$size1_mebp)
          )+
          theme(legend.text = element_text(face = input$face_mebp,color = input$colour4_mebp,family = input$font_selector_mebp,size=input$size3_mebp,)
          )+
          theme(axis.title  = element_text(size=input$size2_mebp, family = input$font_selector_mebp, face = input$face_mebp, color = input$colour2_mebp,hjust=0.5)
          )+
          theme(legend.title = element_text(face = input$face_mebp,color = input$colour4_mebp,family = input$font_selector_mebp, size=input$size4_mebp,)
          )+
          theme(plot.title = element_text(size=input$size5_mebp,face = input$face_mebp, family = input$font_selector_mebp, hjust = 0.5,color = input$colour3_mebp),
          )+
          theme(legend.position=input$Legend_Position_MEBP)
        q
      }
      else{
        return(p) 
      }
    }
    
  })
  ################################plot output 
  output$mebp <- shiny::renderPlot( {
    plotInput()
  },
  bg = "transparent"
  ) 
  
  
  ###############color show
  output$colours_MEBP <- shiny::renderPlot( {
    if (is.null(input$Show_col_switch_MEBP)){return()}
    if (input$Show_col_switch_MEBP > 0) {
      par(mar=c(3,4,2,2))
      RColorBrewer::display.brewer.all(n = NULL, type = "all", select = NULL,
                         colorblindFriendly = TRUE)
    }
  },
  bg = "transparent"
  ) 
  
  
  ################### Download button
  output$image_down_mebp <- shiny::renderUI({
    if (is.null(input$submit_MEBP)) {
      return()
    }
    
    if (input$submit_MEBP > 0 ) {
      list(shiny::downloadButton("downloadImage4",
                          label = "Download Plot", class = "butt1"
      ))
    }
  })
  ############# Download image
  
  output$downloadImage4 <- shiny::downloadHandler(
    filename = "Multiple Error bar.png",
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
  output$data_set_MEBP = shiny::renderUI({
    
    
    
    list(
      shiny::selectInput(
        "filenames_mebp", "Choose a dataset:",
        list.files(
          pattern = c("multiple_error_1.csv|multiple_error_2.csv")
        )
      ),
      shiny::downloadButton("downloadData4", label = "Download csv file", class = "butt4",)
    )
    
    
    
  })
  
  datasetInput = shiny::reactive({
    switch(input$filenames_mebp,
           filenames_mebp
    )
  })
  
  output$downloadData4 = shiny::downloadHandler(
    filename = function() {
      input$filenames_mebp
    },
    content = function(file) {
      write.csv(read.csv
                (input$filenames_mebp, header = TRUE, sep = ","), file, row.names = FALSE)
    }
  )
  
  #########################
}
