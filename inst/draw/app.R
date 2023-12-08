library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(ggthemes)

## Multiple Bar Plot
source("dir/MBP_ui.R")
source("dir/MBP_server.R")
source("dir/BP_ui.R")
source("dir/BP_server.R")
source("dir/EBP_ui.R")
source("dir/EBP_server.R")
source("dir/BOXP_ui.R")
source("dir/BOXP_server.R")
source("dir/MEBP_ui.R")
source("dir/MEBP_server.R")
source("dir/CBP_ui.R")
source("dir/CBP_server.R")
source("dir/LPP_ui.R")
source("dir/LPP_server.R")
source("dir/RLP_ui.R")
source("dir/RLP_server.R")
source("dir/VLP_ui.R")
source("dir/VLP_server.R")
source("dir/HMP_ui.R")
source("dir/HMP_server.R")
source("dir/BBC_ui.R")
source("dir/BBC_server.R")
source("dir/CORR_ui.R")
source("dir/CORR_server.R")
source("dir/CPAC_ui.R")
source("dir/CPAC_server.R")
source("dir/DDG_ui.R")
source("dir/DDG_server.R")
##################################################### global end


############ ui
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(title = "GRAPES DRAW"), # dashboard Head


  dashboardSidebar( # Sidebarmenu
    sidebarMenu(
      menuItem("Home", tabName = "Home", icon = icon("home")),
      menuItem("Bar Plot",
        tabName = "bp", icon = icon("align-left"),
        menuItem("Barplot",
          tabName = "draw_bp", icon = icon("pencil")
        ),
        menuItem("Error barplot",
          tabName = "draw_ebp", icon = icon("pencil")
        )
      ),
      menuItem("Multiple Bar Plot",
        tabName = "mbp", icon = icon("align-left"),
        menuItem("Multiple bar plot",
          tabName = "draw_mbp", icon = icon("pencil")
        ),
        menuItem("Multiplr error bar",
          tabName = "draw_mebp", icon = icon("pencil")
        )
      ),
      menuItem("Circular Bar Plot",
        tabName = "cbp", icon = icon("align-left"),
        menuItem("Circular bar plot",
          tabName = "draw_cbp", icon = icon("pencil")
        )
      ),
      menuItem("Lollipop chart",
        tabName = "lpp", icon = icon("align-left"),
        menuItem("Lollipop chart",
          tabName = "draw_lpp", icon = icon("pencil")
        )
      ),
      menuItem("Box plot",
        tabName = "boxp", icon = icon("align-left"),
        menuItem("Box plot with jitters",
          tabName = "draw_boxp", icon = icon("pencil")
        )
      ),
      menuItem("Ridgeline plot",
        tabName = "boxp", icon = icon("align-left"),
        menuItem("Ridgeline plot",
          tabName = "draw_rlp", icon = icon("pencil")
        )
      ),
      menuItem("Violin plot",
        tabName = "vlp", icon = icon("align-left"),
        menuItem("Violin plot",
          tabName = "draw_vlp", icon = icon("pencil")
        )
      ),
      menuItem("Heatmap",
        tabName = "hmp", icon = icon("align-left"),
        menuItem("Heatmap",
          tabName = "draw_hmp", icon = icon("pencil")
        )
      ),
      menuItem("Bubble chart",
        tabName = "bbc", icon = icon("align-left"),
        menuItem("Bubble chart",
          tabName = "draw_bbc", icon = icon("pencil")
        )
      ),
      menuItem("Correlogram",
        tabName = "corr", icon = icon("align-left"),
        menuItem("Correlogram",
          tabName = "draw_corr", icon = icon("pencil")
        )
      ),
      menuItem("Circular packing",
        tabName = "cpac", icon = icon("align-left"),
        menuItem("Circular packing",
          tabName = "draw_cpac", icon = icon("pencil")
        )
      ),
      menuItem("Dendrogram",
        tabName = "ddg", icon = icon("align-left"),
        menuItem("Dendrogram",
          tabName = "draw_ddg", icon = icon("pencil")
        )
      ),
      div(
        style = "display: flex; justify-content: center;",
        img(
          src = "logo.png", align = "center",
          width = "200", height = "200"
        )
      )
    )
  ),
  dashboardBody(
    tabItems(
      # draw_ebp tab content
      tabItem(tabName = "draw_mbp", MBP_ui()),
      tabItem(tabName = "draw_bp", BP_ui()),
      tabItem(tabName = "draw_ddg", DDG_ui()),
      tabItem(tabName = "draw_cpac", CPAC_ui()),
      tabItem(tabName = "draw_corr", CORR_ui()),
      tabItem(tabName = "draw_bbc", BBC_ui()),
      tabItem(tabName = "draw_hmp", HMP_ui()),
      tabItem(tabName = "draw_rlp", RLP_ui()),
      tabItem(tabName = "draw_vlp", VLP_ui()),
      tabItem(tabName = "draw_lpp", LPP_ui()),
      tabItem(tabName = "draw_ebp", EBP_ui()),
      tabItem(tabName = "draw_boxp", BOXP_ui()),
      tabItem(tabName = "draw_mebp", MEBP_ui()),
      tabItem(tabName = "draw_cbp", CBP_ui()),

      # home tab content
      tabItem(
        tabName = "Home",
        tags$h3(
          HTML("<b> Introduction to grapesDraw </b>")
        ),
        tags$p(
          HTML("<p  style='text-align: justify;'>Welcome to the realm of data visualization, a critical facet of agricultural research where comprehending and presenting data is as challenging as the fieldwork itself. Our mission is to assist researchers in effectively visualizing and comprehending the potential inherent in their data.</p>
<p  style='text-align: justify;'>grapesDraw serves as an gateway to the future of data exploration in agriculture. Through 14 designed tools, we harness the power of grapesDraw to transform raw numbers into captivating narratives. Our aim is to be your reliable ally on this stimulating journey, facilitating the translation of data into clear, visually compelling stories.</p>
<p  style='text-align: justify;'>Through grapesDraw, we intend to introduce you to a visual landscape of charts, plots, and graphs that transcend mere information, seamlessly weaving data narratives into aesthetically pleasing representations. We have tried to merge simplicity with sophistication, ensuring that the genuine story behind your data effortlessly shines through.</p>
<p  style='text-align: justify;'>grapesDraw operates as an open-source platform, governed by the <b>GNU Public License Version 3</b>. For any concerns or issues encountered, we encourage users to kindly raise them on our GitHub repository. Your feedback is invaluable in enhancing and ensuring a more seamless and effective data visualization experience. </p>

                   ")
        ),
        div(
          style = "display: flex; justify-content: center;",
        tags$h3(
          HTML("<b>Your data, Your story, Your masterpiece</b>")
        )
        ),
        tags$br(),
        div(
          style = "display: flex; justify-content: center;",
          img(
            src = "logo.png", align = "center",
            width = "200", height = "200"
          )
        )
      )
    )
  )
)

################ server
server <- function(input, output, session) {
  MBP_server(input, output)
  LPP_server(input, output)
  EBP_server(input, output)
  BOXP_server(input, output)
  MEBP_server(input, output)
  CBP_server(input, output)
  RLP_server(input, output)
  VLP_server(input, output)
  HMP_server(input, output)
  BBC_server(input, output)
  CORR_server(input, output)
  CPAC_server(input, output)
  DDG_server(input, output)
  BP_server(input, output)
}
########################## server end
shinyApp(ui = ui, server = server)
