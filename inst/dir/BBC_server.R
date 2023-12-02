BBC_server <- function(input, output,session) {



  ########################## cBP
  csvfile_BBC <- reactive({
    csvfile_BBC <- input$file1_BBC
    if (is.null(csvfile_BBC)){return(NULL)}
    dt_bbc <- read.csv(csvfile_BBC $datapath, header=input$header, sep=",")
    dt_bbc
  })

  output$var_BBC  <- renderUI({
    if(is.null(input$file1_BBC$datapath)){
      return()
    }
    else{
      list (radioButtons("xbbc", "Select the x axis variables", choices =    names(csvfile_BBC())),
            radioButtons("ybbc", "Select the y axis variables)", choices = names(csvfile_BBC())),
            radioButtons("cbbc", "Select the colouring variable", choices = names(csvfile_BBC())),
            #radioButtons("wbbc", "Select the other if any", choices = names(csvfile_BBC())),
            radioButtons("abbc", "Select the size variable", choices = names(csvfile_BBC())),

            actionBttn(
              inputId = "submit_BBC",
              label = "DRAW!",
              color = "danger",
              style = "jelly"
            )
      )
    }
  })

  ############## control panel for plots
  output$cp_BBC <- renderUI({
    if (is.null(input$file1_BBC$datapath)){return()}
    if (is.null(input$submit_BBC)){return()}
    if (input$submit_BBC > 0) {
      list(
        fluidRow(
          column(4,
                 textInput("xlab_bbc", "Enter required x-axis title", "X-axis")
          ),
          column(4,
                 textInput("ylab_bbc", "Enter required y-axis title", "Y-axis")
          ),
          column(4,
                 textInput("title_bbc", "Enter required title", "title")
          ),
          column(4,
                 textInput("colour1_bbc", "Enter required legend title", "legend")
          ),


          column(4,
                 selectInput("pal.col_BBC", "Choose Colour pattern (plot):",
                             c("Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd",
                               "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3",
                               "BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral"),
                             "Blues"
                 )
          ),
          column(4,
                 selectInput("Legend_Position_BBC", "Choose legend position:",
                             choices = c("none","right","left","top","bottom"),
                             selected = "right")
          ),
          column(
            4,
            selectInput(
              "theme_bbc", "Choose your theme:",
              list("normal","economist","minimal","grey","light","void","tufte","stata","wsj","calc","hc")
            )
          ),
          column(3,
                 materialSwitch(inputId = "Show_col_switch_BBC", label = "Show available colours", status = "danger")
          ),
          column(
            4,
            materialSwitch(inputId = "manual_change_bbc", label = "Show me manual controls", status = "danger")
          )
        )

      )

    }
  })


  #################manual changes of the plot

  output$manual_BBC <- renderUI({
    if (is.null(input$file1_BBC$datapath)) {
      return()
    }
    if (is.null(input$submit_BBC)) {
      return()
    }
    if (is.null(input$manual_change_bbc)) {
      return()
    }
    if (input$submit_BBC > 0 && input$manual_change_bbc > 0) {
      list(
        fluidRow(

          column(4,
                 selectInput("face_bbc", "select font face",
                             choices = c("plain","italic","bold","bold.italic"),
                             selected = "plain")
          ),
          column(4,
                 selectInput("font_selector_bbc", "select a font",
                             choices = c(
                               "Arial" ,"Calibri","Georgia","Helvetica" ,"Palatino","Garamond",
                               "Times New Roman" ,"Baskerville","Courier New","Verdana","Century Schoolbook",
                               "Liberation Serif"
                             ),
                             selected = "Arial")),

          column(4,
                 selectInput("colour2_bbc","choose colour of axis title and label:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(4,
                 selectInput("colour4_bbc","choose colour of legend title and label:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(4,
                 selectInput("colour3_bbc","choose colour of plot title:",
                             choices = c("darkblue","red","blue","black","yellow","orange","purple","brown","cyan","magenta"),
                             selected = "black")
          ),
          column(5,
                 sliderInput("size1_bbc", "axis label size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 sliderInput("size2_bbc", "axis title size:",
                             min = 10, max = 20, value = 10
                 )
          ),
          column(5,
                 sliderInput("size3_bbc", "legend label size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 sliderInput("size4_bbc", "legend title size:",
                             min = 10, max = 20, value = 10)
          ),
          column(5,
                 sliderInput("size5_bbc", "plot title size:",
                             min = 15, max = 30, value = 15)
          ),
          column(5,
                 sliderInput("angle_bbc","required angle of x-axis labels (degrees)",min=0, max=135, value=0, step = 15)
          )

        )
      )
    }
  })

  ############### plotting
  plotInput <- reactive({
    if (is.null(input$file1_BBC$datapath)) {
      return()
    }
    if (is.null(input$submit_BBC)) {
      return()
    }
    if (input$submit_BBC > 0) {
      x <- as.matrix(csvfile_BBC()[, input$xbbc])
      y <- as.matrix(csvfile_BBC()[, input$ybbc])
      z <- as.matrix(csvfile_BBC()[, input$cbbc])
      #w <- as.matrix(csvfile_BBC()[, input$wbbc])
      a <- as.matrix(csvfile_BBC()[, input$abbc])

      data_bbc <- data.frame(
        xvar = x,
        yvar = y,
        treatment =z,
        #was = w,
        average =a
      )

      #data_bbc$xvar <- factor(data_bbc$xvar, levels = unique(data_bbc$xvar)) ### sorting factors bcz T1, T2 etc can cause problem
      data_bbc$treatment <- factor(data_bbc$treatment, levels = unique(data_bbc$treatment))
      nb.col <- nlevels(data_bbc$treatment) # number of colouring factors

      if (is.null(input$pal.col_BBC)){
        mycolors <- colorRampPalette(brewer.pal(8, "Blues"))(nb.col)
      } else {
        # Changed here for a debugging
        mycolors <- colorRampPalette(brewer.pal(8, input$pal.col_BBC))(nb.col)
      }


      ########################
      #mycolors <- colorRampPalette(brewer.pal(8, input$pal.col_BBC))(nb.col) # colorrampallete code
      p <- ggplot2::ggplot(data_bbc, aes( color = treatment, y = yvar, x = xvar, size = average)) + # plot using aesthetics
        #geom_bar( stat = "identity",width=input$Barwidth_cbp) + # geometric bar operation is used to give the type of bar plot stacked at dodged.
        geom_point(alpha=0.8)+ ## used for creating scatter plots or adding individual data points to an existing plot
        scale_size(range = c(1,20),name = 'average',labels = NULL)+
        #facet_wrap(~was)+
        scale_color_manual(values = mycolors ) + # apply the color scale to fill the aesthetic of a plot
        xlab(input$xlab_bbc) + # to give labels on x-axis
        ylab(input$ylab_bbc) + # to give labels on y axis
        ggtitle(input$title_bbc) + # to give title of plot
        labs(fill= input$colour1_bbc)+
        #scale_fill_viridis(discrete=FALSE, guide=FALSE, option="A") +
        theme(axis.text.x=element_text(angle=input$angle_bbc,color = input$colour2_bbc, vjust = 0.5),
              axis.text.y=element_text(color = input$colour2_bbc))+
        theme(
          axis.text = element_text(face = input$face_bbc,family = input$font_selector_bbc, size=input$size1_bbc)
        )+
        theme(legend.text = element_text(face = input$face_bbc,color = input$colour4_bbc,family = input$font_selector_bbc,size=input$size3_bbc,)
        )+
        theme(axis.title  = element_text(size=input$size2_bbc, family = input$font_selector_bbc, face = input$face_bbc, color = input$colour2_bbc,hjust=0.5)
        )+
        theme(legend.title = element_text(face = input$face_bbc,color = input$colour4_bbc,family = input$font_selector_bbc, size=input$size4_bbc,)
        )+
        theme(plot.title = element_text(size=input$size5_bbc,hjust = 0.5,family = input$font_selector_bbc,color = input$colour3_bbc,face = input$face_bbc)
        )+
        theme(legend.position = input$Legend_Position_BBC)




      if (input$submit_BBC>0) {
        if (is.null(input$theme_bbc)) {
          return(p)
        }
        if (input$theme_bbc == "normal") {
          q <- p + theme_bw()
        } else if (input$theme_bbc == "economist") {
          q <- p + ggthemes::theme_economist()
        } else if (input$theme_bbc == "grey") {
          q <- p + theme_gray()
        }
        else if (input$theme_bbc == "minimal") {
          q <- p + theme_minimal()
        }else if (input$theme_bbc == "light") {
          q <- p + theme_light()
        }
        else if (input$theme_bbc == "void") {
          q <- p + theme_void()
        }
        else if (input$theme_bbc == "tufte") {
          q <- p + ggthemes::theme_tufte()
        }
        else if (input$theme_bbc == "stata") {
          q <- p + ggthemes::theme_stata()
        }
        else if (input$theme_bbc == "wsj") {
          q <- p + ggthemes::theme_wsj()
        }
        else if (input$theme_bbc == "calc") {
          q <- p + ggthemes::theme_calc()
        }
        else if (input$theme_bbc == "hc") {
          q <- p + ggthemes::theme_hc()
        }

        q<- q+theme(axis.text.x=element_text(angle=input$angle_bbc,color = input$colour2_bbc, vjust = 0.5),
                    axis.text.y=element_text(color = input$colour2_bbc))+
          theme(
            axis.text = element_text(face = input$face_bbc,family = input$font_selector_bbc, size=input$size1_bbc)
          )+
          theme(legend.text = element_text(face = input$face_bbc,color = input$colour4_bbc,family = input$font_selector_bbc,size=input$size3_bbc,)
          )+
          theme(axis.title  = element_text(size=input$size2_bbc, family = input$font_selector_bbc, face = input$face_bbc, color = input$colour2_bbc,hjust=0.5)
          )+
          theme(legend.title = element_text(face = input$face_bbc,color = input$colour4_bbc,family = input$font_selector_bbc, size=input$size4_bbc,)
          )+
          theme(plot.title = element_text(size=input$size5_bbc,hjust = 0.5,family = input$font_selector_bbc,color = input$colour3_bbc,face = input$face_bbc)
          )+
          theme(legend.position = input$Legend_Position_BBC)
        q
      }
      else{
        return(p)
      }
    }

  })
  ################################plot output
  output$bbc <- renderPlot( {
    plotInput()
  },
  bg = "transparent"
  )


  ###############color show
  output$colours_BBC <- renderPlot( {
    if (is.null(input$Show_col_switch_BBC)){return()}
    if (input$Show_col_switch_BBC > 0) {
      par(mar=c(3,4,2,2))
      display.brewer.all(n = NULL, type = "all", select = NULL,
                         colorblindFriendly = TRUE)
    }
  },
  bg = "transparent"
  )


  ################### Download button
  output$image_down_bbc <- renderUI({
    if (is.null(input$submit_BBC)) {
      return()
    }
    if (input$submit_BBC > 0) {
      list(downloadButton("downloadImage11",
                          label = "Download Plot", class = "butt1"
      ))
    }
  })
  ############# Download image

  output$downloadImage11 <- downloadHandler(
    filename = "Bubble chart.png",
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
  output$data_set_BBC = renderUI({



    list(
      selectInput(
        "filenames_bbc", "Choose a dataset:",
        choices = list.files(path = system.file("extdata", package = "grapesDraw"),
                             pattern = "Bubble chart [1-2].csv",
                             full.names = TRUE)
      ),
      downloadButton("downloadData11", label = "Download csv file", class = "butt11",)
    )



  })

  datasetInput = reactive({
    switch(input$filenames_bbc,
           filenames_bbc
    )
  })

  output$downloadData11 = downloadHandler(
    filename = function() {
      input$filenames_bbc
    },
    content = function(file) {
      write.csv(read.csv
                (input$filenames_bbc, header = TRUE, sep = ","), file, row.names = FALSE)
    }
  )
  ######################### end data set download
}
