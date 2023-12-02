library(viridis)
library(viridisLite)
library(hrbrthemes)
library(ggthemes)

BP_server <- function(input, output,session) {
  
  
  
  ########################## BP 
  csvfile_BP <- reactive({
    csvfile_BP <- input$file1_BP
    if (is.null(csvfile_BP)){return(NULL)}
    dt_BP  <- read.csv(csvfile_BP $datapath, header=input$header, sep=",")
    dt_BP 
  })
  
  output$var_BP  <- renderUI({
    if(is.null(input$file1_BP$datapath)){
      return()
    }
    else{
      list (radioButtons("xBP", "Select the x axis variables", choices =    names(csvfile_BP())),
            radioButtons("yBP", "Select the y axis variable", choices = names(csvfile_BP())),
            actionBttn(
              inputId = "submit_BP",
              label = "DRAW!",
              color = "danger",
              style = "jelly"
            )
      )
    }
  })
  ############## control panel for plots
  output$cp_BP <- renderUI({
    if (is.null(input$file1_BP$datapath)){return()}
    if (is.null(input$submit_BP)){return()}
    if (input$submit_BP > 0) {
      list(
        fluidRow(
          column(4,
                 textInput("xlab_BP", "Enter required x-axis title", "X-axis")
          ),
          column(4,
                 textInput("ylab_BP", "Enter required y-axis title", "Y-axis")
          ),
          column(4,
                 textInput("title_BP", "Enter required title", "title")
          ),
          column(4,
                 textInput("legend_BP", "Enter required legend title", "legend")
          ),
          
          
          column(4,
                 selectInput("pal.col_BP", "Choose Colour pattern (plot):",
                             list(`Sequential` = list("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"),
                                  `Qualitative` = list("Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3"),
                                  `Diverging` = list("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral"))
                 )
          ), 
          column(4,
                 selectInput("Legend_Position_BP", "Choose legend position:",
                             choices = c("none","right","left","top","bottom"),
                             selected = "right")
          ),
          column(
            4,
            selectInput(
              "theme_BP", "Choose your theme:",
              list("normal","economist","minimal","grey","light","void","tufte","stata","wsj","calc","hc")
            )
          ),
          column(3,
                 materialSwitch(inputId = "Show_col_switch_BP", label = "Show available colours", status = "danger")
          ),
          
          column(
            4,
            materialSwitch(inputId = "manual_change_BP", label = "Show me manual controls", status = "danger")
          )
        )
        
      )
      
    }
  })
  
  #################manual changes of the plot
  
  output$manual_BP <- renderUI({
    if (is.null(input$file1_BP$datapath)) {
      return()
    }
    if (is.null(input$submit_BP)) {
      return()
    }
    if (is.null(input$manual_change_BP)) {
      return()
    }
    if (input$submit_BP > 0 && input$manual_change_BP > 0) {
      list(
        fluidRow(
          
          column(4,
                 selectInput("face_BP", "select font face",
                             choices = c("plain","italic","bold","bold.italic"),
                             selected = "plain")
          ),
          column(4,
                 selectInput("font_selector_BP", "select a font",
                             choices = c(
                               "Arial" ,"Calibri","Georgia","Helvetica" ,"Palatino","Garamond",
                               "Times New Roman" ,"Baskerville","Courier New","Verdana","Century Schoolbook",
                               "Liberation Serif"
                             ),
                             selected = "Arial")),
          column(4,
                 selectInput("colour2_BP","choose colour of axis title and label:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(4,
                 selectInput("colour4_BP","choose colour of legend title and label:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(4,
                 selectInput("colour3_BP","choose colour of plot title:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(5,
                 sliderInput("size1_BP", "axis label size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 sliderInput("size2_BP", "axis title size:",
                             min = 10, max = 20, value = 10
                 )
          ),
          column(5,
                 sliderInput("size3_BP", "legend label size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 sliderInput("size4_BP", "legend title size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 sliderInput("size5_BP", "plot title size:",
                             min = 15, max = 30, value = 15)
          ),
          column(5,
                 sliderInput("angle_BP","required angle of x-axis labels (degrees)",min=0, max=135, value= 0, step = 15)
          ),
          column(5,
                 sliderInput("Barwidth_BP","select required bar width",
                             min=0.1, max=1, value=0.9)
          )
        )
      )
    }
  }) 
  
  ############### plotting
  plotInput <- reactive({
    if (is.null(input$file1_BP$datapath)) {
      return()
    }
    if (is.null(input$submit_BP)) {
      return()
    }
    if (input$submit_BP > 0) {
      x <- as.matrix(csvfile_BP()[, input$xBP])
      y <- as.matrix(csvfile_BP()[, input$yBP])
      
      data_BP <- data.frame(
        xvar = x,
        yvar = y
       
      )
      
      data_BP$xvar <- factor(data_BP$xvar, levels = unique(data_BP$xvar)) ### sorting factors bcz T1, T2 etc can cause problem
      #data_BP$colour <- factor(data_BP$colour, levels = unique(data_BP$colour))
      nb.col <- nlevels(data_BP$xvar) # number of colouring factors
      
      if (is.null(input$pal.col_BP)){
        mycolors <- colorRampPalette(brewer.pal(8, "Blues"))(nb.col) 
      } else {
        # Changed here for a debugging
        mycolors <- colorRampPalette(brewer.pal(8, input$pal.col_BP))(nb.col)
      }
      
      
      
      #mycolors <- colorRampPalette(brewer.pal(8, input$pal.col_BP))(nb.col) # colorrampallete code
      p <- ggplot2::ggplot(data_BP, aes(fill = xvar, y = yvar, x = xvar)) + # plot using aesthetics
        geom_bar(position = "dodge", stat = "identity",width=input$Barwidth_BP
        )+
        scale_fill_manual(values = mycolors) + # apply the color scale to fill the aesthetic of a plot
        xlab(input$xlab_BP) + # to give labels on x-axis
        ylab(input$ylab_BP) + # to give labels on y axis
        ggtitle(input$title_BP) + # to give title of plot
        labs(fill= input$legend_BP)+
        theme(axis.text.x=element_text(angle=input$angle_BP,color = input$colour2_BP, vjust = 0.5),
              axis.text.y=element_text(color = input$colour2_BP))+
        theme(
          axis.text = element_text(face = input$face_BP,family = input$font_selector_BP, size=input$size1_BP)
        )+
        theme(legend.text = element_text(face = input$face_BP,color = input$colour4_BP,family = input$font_selector_BP,size=input$size3_BP,)
        )+
        theme(axis.title  = element_text(size=input$size2_BP, family = input$font_selector_BP, face = input$face_BP, color = input$colour2_BP,hjust=0.5)
        )+
        theme(legend.title = element_text(face = input$face_BP,color = input$colour4_BP,family = input$font_selector_BP, size=input$size4_BP,)
        )+
        theme(plot.title = element_text(size=input$size5_BP,face = input$face_BP, family = input$font_selector_BP, hjust = 0.5,color = input$colour3_BP),
        )+
        theme(legend.position=input$Legend_Position_BP)
      
      
      #############################  
      if (input$submit_BP>0) {
        if (is.null(input$theme_BP)) {
          return(p)
        }
        if (input$theme_BP == "normal") {
          q <- p + theme_bw()
        } else if (input$theme_BP == "economist") {
          q <- p + ggthemes::theme_economist() 
        } else if (input$theme_BP == "grey") {
          q <- p + theme_gray() 
        } 
        else if (input$theme_BP == "minimal") {
          q <- p + theme_minimal()
        }else if (input$theme_BP == "light") {
          q <- p + theme_light()
        }
        else if (input$theme_BP == "void") {
          q <- p + theme_void()
        }
        else if (input$theme_BP == "tufte") {
          q <- p + ggthemes::theme_tufte()
        }
        else if (input$theme_BP == "stata") {
          q <- p + ggthemes::theme_stata()
        }
        else if (input$theme_BP == "wsj") {
          q <- p + ggthemes::theme_wsj()
        }
        else if (input$theme_BP == "calc") {
          q <- p + ggthemes::theme_calc()
        }
        else if (input$theme_BP == "hc") {
          q <- p + ggthemes::theme_hc()
        }
        q <- q+theme(axis.text.x=element_text(angle=input$angle_BP,color = input$colour2_BP, vjust = 0.5),
                     axis.text.y=element_text(color = input$colour2_BP))+
          theme(
            axis.text = element_text(face = input$face_BP,family = input$font_selector_BP, size=input$size1_BP)
          )+
          theme(legend.text = element_text(face = input$face_BP,color = input$colour4_BP,family = input$font_selector_BP,size=input$size3_BP,)
          )+
          theme(axis.title  = element_text(size=input$size2_BP, family = input$font_selector_BP, face = input$face_BP, color = input$colour2_BP,hjust=0.5)
          )+
          theme(legend.title = element_text(face = input$face_BP,color = input$colour4_BP,family = input$font_selector_BP, size=input$size4_BP,)
          )+
          theme(plot.title = element_text(size=input$size5_BP,face = input$face_BP, family = input$font_selector_BP, hjust = 0.5,color = input$colour3_BP),
          )+
          theme(legend.position=input$Legend_Position_BP)
        q
      }
      else{
        return(p) 
      }
    }
    
  })
  ################################plot output 
  output$BP <- renderPlot( {
    plotInput()
  },
  bg = "transparent"
  ) 
  
  
  ###############color show
  output$colours_BP <- renderPlot( {
    if (is.null(input$Show_col_switch_BP)){return()}
    if (input$Show_col_switch_BP > 0) {
      par(mar=c(3,4,2,2))
      display.brewer.all(n = NULL, type = "all", select = NULL,
                         colorblindFriendly = TRUE)
    }
  },
  bg = "transparent"
  ) 
  
  
  ################### Download button
  output$image_down_BP <- renderUI({
    if (is.null(input$submit_BP)) {
      return()
    }
    
    if (input$submit_BP > 0 ) {
      list(downloadButton("downloadImage1",
                          label = "Download Plot", class = "butt1"
      ))
    }
  })
  ############# Download image
  
  output$downloadImage1 <- downloadHandler(
    filename = "Bar plot.png",
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
  output$data_set_BP = renderUI({
    
    list(
      selectInput(
        "filenames_bp", "Choose a dataset:",
        list.files(
          pattern = c("Bar plot 1.csv|Bar plot 2.csv")
        )
      ),
      downloadButton("downloadData1", label = "Download csv file", class = "butt1",)
    )
    
    
    
  })
  
  datasetInput = reactive({
    switch(input$filenames_bp,
           filenames_bp
    )
  })
  
  output$downloadData1 = downloadHandler(
    filename = function() {
      input$filenames_bp
    },
    content = function(file) {
      write.csv(read.csv
                (input$filenames_bp, header = TRUE, sep = ","), file, row.names = FALSE)
    }
  )
  
  #########################
}