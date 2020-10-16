# shiny_dashpage ----------------------------------------------------------
library(shinydashboard)
library(shiny)
library(dashboardthemes)
library(shinyalert)
library(shinybusy) 
library(bs4Dash)
library(shinyjs)
# module define -----------------------------------------------------------


mod_ui <- function(id,compare,id_button,tagname,choice){
  ns <- NS(id)
  tagList(
    conditionalPanel(
      condition = compare,
      radioButtons(id_button,tagname,choices=choice)
    )
  )
}

line_break <- tags$div(
  HTML("<br>")
)



# whether you delete the table
modal_confirm <- modalDialog(
  "Did you reset the table?",
  title = "Reset table",
  footer = tagList(
    actionButton("cancel", "Cancel"),
    actionButton("Yes", "Not yet", class = "btn btn-danger")
  )
)
# app ---------------------------------------------------------------------



bs4DashPage(
  # dashboardHeader(
  #   # title = shinyDashboardLogo(
  #   #   theme = "blue_gradient",
  #   #   boldText = "Meta",
  #   #   mainText = "Tools",
  #   #   badgeText = "v1.1.1"
  #   # )
  #   ),
  bs4DashSidebar(
    bs4SidebarUserPanel(
      text = "Meta tools"
    ),
    div(id="press",
    sidebarMenu(id="sidebar",
                div(id="doi",
                menuItem("DOI extractor", tabName = "apa", icon = icon("angle-double-up"))),
                div(id='Coding',
                menuItem("Coding helper", tabName = "Coding", icon = icon("archive")))
    ))
  ),
  bs4DashNavbar(skin = "dark"),
  bs4DashBody(
    useShinyjs(),
    useShinyalert(),
    ### changing theme
    shinyDashboardThemes(
      theme = "grey_dark"
    ),
    tags$head(
      tags$link(rel="stylesheet",type="text/css",href="custom1.css")
    ),
    # Boxes need to be put in a row (or column)
    fluidRow(
      add_busy_spinner(spin = "fading-circle",color="#FFFFFF"),
      column(10, 
             tags$div(id = "placeholder"))
    )
  ),
  bs4DashControlbar(skin='dark',
                    title="Please choose",
    fileInput("files","Select your data",accept = c(".ciw",".rif",".txt",".bib")),
  actionButton('reset', 'Reset'),
  line_break,
  mod_ui("ui_1","input.sidebar== 'apa'","download_w","Choose what part to download",c("title"="title","doi"="doi")),
  mod_ui("ui_2","input.sidebar == 'Coding'","symmetric","Choose what table to download",c("Symmetric"="sym","Asymmetric"="asy")),
  mod_ui("ui_3","input.sidebar == 'Coding'","output_f","Choose what format to download",c("Comma"=",","Semicolon"=";")),
  mod_ui("ui_3","input.sidebar == 'Coding'","delete_star","Delete stars",c("Yes"="Y","No"="N")),
  actionButton("Load", "Load the File"),
  line_break,
  downloadButton("downloadData", "Download")
  )
)