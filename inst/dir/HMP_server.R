HMP_server <- function(input, output,session) {
  
  
  
  ########################## HMP 
  csvfile_HMP <- shiny::reactive({
    csvfile_HMP <- input$file1_HMP
    if (is.null(csvfile_HMP)){return(NULL)}
    dt_HMP  <- read.csv(csvfile_HMP $datapath, header=input$header, sep=",")
    dt_HMP 
  })
  
  output$var_HMP  <- shiny::renderUI({
    if(is.null(input$file1_HMP$datapath)){
      return()
    }
    else{
      list (shiny::radioButtons("xhmp", "Select the x axis variables", choices =    names(csvfile_HMP())),
            shiny::radioButtons("yhmp", "Select the y axis variable", choices = names(csvfile_HMP())),
            shiny::radioButtons("fhmp", "Select the filling variable", choices = names(csvfile_HMP())),
            shinyWidgets::actionBttn(
              inputId = "submit_HMP",
              label = "DRAW!",
              color = "danger",
              style = "jelly"
            )
      )
    }
  })
  ############## control panel for plots
  output$cp_HMP <- shiny::renderUI({
    if (is.null(input$file1_HMP$datapath)){return()}
    if (is.null(input$submit_HMP)){return()}
    if (input$submit_HMP > 0) {
      list(
        fluidRow(
          column(4,
                 shiny::textInput("xlab_hmp", "Enter required x-axis title", "X-axis")
          ),
          column(4,
                 shiny::textInput("ylab_hmp", "Enter required y-axis title", "Y-axis")
          ),
          column(4,
                 shiny::textInput("title_hmp", "Enter required title", "title")
          ),
          column(4,
                 shiny::textInput("legend_hmp", "Enter required legend title", "legend")
          ),
          
          
          column(4,
                 shiny::selectInput("pal.col_HMP", "Choose Colour pattern (plot):",
                             list(`Sequential` = list("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"),
                                  `Qualitative` = list("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3"),
                                  `Diverging` = list("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral"))
                 )
          ), 
          column(4,
                 shiny::selectInput("Legend_Position_HMP", "Choose legend position:",
                             choices = c("none","right","left","top","bottom"),
                             selected = "right")
          ),
          column(
            4,
            shiny::selectInput(
              "theme_hmp", "Choose your theme:",
              list("normal","economist","minimal","grey","light","void","tufte","stata","wsj","calc","hc")
            )
          ),
          column(3,
                 shinyWidgets::materialSwitch(inputId = "Show_col_switch_HMP", label = "Show available colours", status = "danger")
          ),
          column(
            4,
            shinyWidgets::materialSwitch(inputId = "manual_change_hmp", label = "Show me manual controls", status = "danger")
          )
        )
        
      )
      
    }
  })
  
  
  #################manual changes of the plot
  
  output$manual_change_HMP <- shiny::renderUI({
    if (is.null(input$file1_HMP$datapath)) {
      return()
    }
    if (is.null(input$submit_HMP)) {
      return()
    }
    if (is.null(input$manual_change_hmp)) {
      return()
    }
    if (input$submit_HMP > 0 && input$manual_change_hmp > 0) {
      list(
        fluidRow(
          
          column(4,
                 shiny::selectInput("face_hmp", "select font face",
                             choices = c("plain","italic","bold","bold.italic"),
                             selected = "plain")
          ),
          column(4,
                 shiny::selectInput("font_selector_hmp", "select a font",
                             choices = c(
                               "Arial" ,"Calibri","Georgia","Helvetica" ,"Palatino","Garamond",
                               "Times New Roman" ,"Baskerville","Courier New","Verdana","Century Schoolbook",
                               "Liberation Serif"
                             ),
                             selected = "Arial")),
          column(4,
                 shiny::selectInput("colour1_hmp","choose colour of axis title and label:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(4,
                 shiny::selectInput("colour2_hmp","choose colour of legend title and label:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(4,
                 shiny::selectInput("colour3_hmp","choose colour of plot title:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(5,
                 shiny::sliderInput("size1_hmp", "axis label size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 shiny::sliderInput("size2_hmp", "axis title size:",
                             min = 10, max = 20, value = 10
                 )
          ),
          column(5,
                 shiny::sliderInput("size3_hmp", "legend label size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 shiny::sliderInput("size4_hmp", "legend title size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 shiny::sliderInput("size5_hmp", "plot title size:",
                             min = 15, max = 30, value = 15)
          ),
          column(5,
                 shiny::sliderInput("angle_hmp","required angle of x-axis labels (degrees)",min=0, max=135, value= 0, step = 15)
          )
        )
      )
    }
  }) 
  
  ############### plotting
  plotInput <- shiny::reactive({
    if (is.null(input$file1_HMP$datapath)) {
      return()
    }
    if (is.null(input$submit_HMP)) {
      return()
    }
    if (input$submit_HMP > 0) {
      x <- as.matrix(csvfile_HMP()[, input$xhmp])
      y <- as.matrix(csvfile_HMP()[, input$yhmp])
      z <- as.matrix(csvfile_HMP()[, input$fhmp])
      
      data_hmp <- data.frame(
        xvar = x,
        value = z,
        yvar = y
      )
      data_hmp$xvar <- factor(data_hmp$xvar, levels = unique(data_hmp$xvar)) ### sorting factors bcz T1, T2 etc can cause problem
      data_hmp$yvar <- factor(data_hmp$yvar, levels = unique(data_hmp$yvar)) ### sorting factors bcz T1, T2 etc can cause problem
      data_hmp$value <- factor(data_hmp$value, levels = unique(data_hmp$value))
      nb.col <- nlevels(data_hmp$value) # number of colouring factors
      
      if (is.null(input$pal.col_HMP)){
        mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, "Blues"))(nb.col) 
      } else {
        # Changed here for a debugging
        mycolors <- colorRampPalette(RColorBrewer::brewer.pal(8, input$pal.col_HMP))(nb.col)
      }
      
      
      
      #mycolors <- colorRampPalette(brewer.pal(8, input$pal.col_HMP))(nb.col) # colorrampallete code
      p <- ggplot2::ggplot(data_hmp, aes( x = xvar, y = yvar, fill = value )) + # plot using aesthetics
        #geom_bar(position = "dodge", stat = "identity",width=input$Barwidth_mbp) + # geometric bar operation is used to give the type of bar plot stacked at dodged.
        geom_tile()+
        scale_fill_manual(values = mycolors) + # apply the color scale to fill the aesthetic of a plot
        xlab(input$xlab_hmp) + # to give labels on x-axis
        ylab(input$ylab_hmp) + # to give labels on y axis
        ggtitle(input$title_hmp) + # to give title of plot
        labs(fill= input$legend_hmp)+
        theme(axis.text.x=element_text(angle=input$angle_hmp,color = input$colour1_hmp, vjust = 0.5),
              axis.text.y=element_text(color = input$colour1_hmp))+
        theme(
          axis.text = element_text(face = input$face_hmp,family = input$font_selector_hmp, size=input$size1_hmp)
        )+
        theme(legend.text = element_text(face = input$face_hmp,color = input$colour2_hmp,family = input$font_selector_hmp,size=input$size3_hmp,)
        )+
        theme(axis.title  = element_text(size=input$size2_hmp, family = input$font_selector_hmp, face = input$face_hmp, color = input$colour1_hmp,hjust=0.5)
        )+
        theme(legend.title = element_text(face = input$face_hmp,color = input$colour2_hmp,family = input$font_selector_hmp, size=input$size4_hmp,)
        )+
        theme(plot.title = element_text(size=input$size5_hmp,face = input$face_hmp, family = input$font_selector_hmp, hjust = 0.5,color = input$colour3_hmp)
        )+
        theme(legend.position=input$Legend_Position_HMP)
      
      
      #############################  
      
      if (input$submit_HMP>0) {
        if (is.null(input$theme_hmp)) {
          return(p)
        }
        if (input$theme_hmp == "normal") {
          q <- p + theme_bw()
        } else if (input$theme_hmp == "economist") {
          q <- p + ggthemes::theme_economist() 
        } else if (input$theme_hmp == "grey") {
          q <- p + theme_gray() 
        } 
        else if (input$theme_hmp == "minimal") {
          q <- p + theme_minimal()
        }else if (input$theme_hmp == "light") {
          q <- p + theme_light()
        }
        else if (input$theme_hmp == "void") {
          q <- p + theme_void()
        }
        else if (input$theme_hmp == "tufte") {
          q <- p + ggthemes::theme_tufte()
        }
        else if (input$theme_hmp == "stata") {
          q <- p + ggthemes::theme_stata()
        }
        else if (input$theme_hmp == "wsj") {
          q <- p + ggthemes::theme_wsj()
        }
        else if (input$theme_hmp == "calc") {
          q <- p + ggthemes::theme_calc()
        }
        else if (input$theme_hmp == "hc") {
          q <- p + ggthemes::theme_hc()
        }
        q<- q+theme(axis.text.x=element_text(angle=input$angle_hmp,color = input$colour1_hmp, vjust = 0.5),
                    axis.text.y=element_text(color = input$colour1_hmp))+
          theme(
            axis.text = element_text(face = input$face_hmp,family = input$font_selector_hmp, size=input$size1_hmp)
          )+
          theme(legend.text = element_text(face = input$face_hmp,color = input$colour2_hmp,family = input$font_selector_hmp,size=input$size3_hmp,)
          )+
          theme(axis.title  = element_text(size=input$size2_hmp, family = input$font_selector_hmp, face = input$face_hmp, color = input$colour1_hmp,hjust=0.5)
          )+
          theme(legend.title = element_text(face = input$face_hmp,color = input$colour2_hmp,family = input$font_selector_hmp, size=input$size4_hmp,)
          )+
          theme(plot.title = element_text(size=input$size5_hmp, face = input$face_hmp,family = input$font_selector_hmp, hjust = 0.5,color = input$colour3_hmp)
          )+
          theme(legend.position=input$Legend_Position_HMP)
        q
      }
      else{
        return(p) 
      }
    }
    
  })
  ################################plot output 
  output$hmp <- shiny::renderPlot( {
    plotInput()
  },
  bg = "transparent"
  ) 
  
  
  ###############color show
  output$colours_HMP <- shiny::renderPlot( {
    if (is.null(input$Show_col_switch_HMP)){return()}
    if (input$Show_col_switch_HMP > 0) {
      par(mar=c(3,4,2,2))
      RColorBrewer::display.brewer.all(n = NULL, type = "all", select = NULL,
                         colorblindFriendly = TRUE)
    }
  },
  bg = "transparent"
  ) 
  
  
  ################### Download button
  output$image_down_hmp <- shiny::renderUI({
    if (is.null(input$submit_HMP)) {
      return()
    }
    
    if (input$submit_HMP > 0 ) {
      list(shiny::downloadButton("downloadImage10",
                          label = "Download Plot", class = "butt1"
      ))
    }
  })
  ############# Download image
  
  output$downloadImage10 <- shiny::downloadHandler(
    filename = "Heatmap.png",
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
  output$data_set_HMP = shiny::renderUI({
    
    
    
    list(
      shiny::selectInput(
        "filenames_hmp", "Choose a dataset:",
        list.files(
          pattern = c("heatmap_1.csv|heatmap_2.csv")
        )
      ),
      shiny::downloadButton("downloadData10", label = "Download csv file", class = "butt10",)
    )
    
    
    
  })
  
  datasetInput = shiny::reactive({
    switch(input$filenames_hmp,
           filenames_hmp
    )
  })
  
  output$downloadData10 = shiny::downloadHandler(
    filename = function() {
      input$filenames_hmp
    },
    content = function(file) {
      write.csv(read.csv
                (input$filenames_hmp, header = TRUE, sep = ","), file, row.names = FALSE)
    }
  )
  ######################### end data set download
}
