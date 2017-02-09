library(shiny)
library(RYeti)

shinyUI(pageWithSidebar(

  headerPanel("AnalyzeSpec"),

  sidebarPanel(
    radioButtons(inputId = "separator", label = "Select separator of .csv file",
                 choices = list(semicolon = ";", comma = ",")),
    radioButtons(inputId = "decimal", label = "Select decimal sign of .csv file",
                 choices = list(comma = ",", period = ".")),
    fileInput('file1', 'Browse to Yeti output file (.csv)'),
    #numericInput('measurementInput', 'Type measurement number to plot', value = 1),
    uiOutput('measurementInput')
    ),

  mainPanel(
    tabsetPanel(
      tabPanel('Plot', plotOutput('spectralplot'),
               plotOutput('PSS_plot')),
      tabPanel('Metadata', tableOutput('metadata')),
      tabPanel('Spectral data', tableOutput('spectraldata'))

      )
    )
))
