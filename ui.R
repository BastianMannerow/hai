###################### ui for marking outliers app ######################
### load libraries
#library(easypackages)
#libraries("shiny", "tidyverse", "sysfonts", "showtext", "dplyr", "reshape2", "readxl", "DT")
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(sysfonts)
library(showtext)
library(reshape2)
library(readxl)
library(DT)
### load Roboto font and change scale view
font_add_google("Roboto Condensed", family = "Roboto")
showtext_auto()
options(scipen = 999)

ui <- fluidPage(
  includeCSS("www/style.css"),
  #navbarPage("Ausreißer App"),
  tags$a(
    href="https://jil.sh/projekte/hai-kooperative-haushaltsaufstellung-mit-augmented-intelligence/", 
    tags$img(src="HAI_Logo.png", 
             alt="HAI",
             align = "center",
             width="50")
  ),
  tags$a(
    href="https://jil.sh", 
    tags$img(src="JIL_Logo.png", 
             alt="JIL",
             align = "right",
             width="200")
  ),
  titlePanel("Ausreißer App"),
  p("Bitte markieren Sie in der Graphik, welche Erträge Sie als Ausreißer bewerten. Hierfür klicken Sie einfach auf den jeweiligen Punkt."),
  
  fluidPage(
    fluidRow(
      column(3,
        p("Passen Sie die Filter-Optionen für die Graphik nach Ihrem Bedarf an."),
        radioGroupButtons(
          inputId = "pickArt",
          label = "Wählen Sie die Art der Beträge.",
          choices = c("Soll-Werte", "Ist-Werte", "Differenz"),
          #choices = c("Soll-Werte", "Ist-Werte"),
          justified = TRUE
        ),
        pickerInput(
          inputId = "pickProdukt",
          label = "Wählen Sie ein Produkt.",
          choices = c("A", "B", "C"),
        ),
        pickerInput(
          inputId = "pickTitel",
          label = "Wählen Sie die Titel. ",
          choices = c("a","b","c"),
          multiple = TRUE,
          selected = c("a","b","c"),
          options = list(
            'actions-box' = TRUE,
            'deselect-all-text' = "Alle abwählen",
            'select-all-text' = "Alle auswählen",
            'none-selected-text' = "Keine Titel ausgewählt")
        ),
        sliderTextInput(
          inputId = "pickZeitraum",
          label = "Wählen Sie den Zeitraum.", 
          choices = c(2010,2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021),
          selected = c(2010,2021)
        ),
        sliderTextInput(
          inputId = "pickWertebereich",
          label = "Wählen Sie den angezeigten Wertebereich.", 
          choices = c(0,100,1000,10000,50000,100000),
          selected = c(0,100000)
        ),
        # UI Output for the warning message of out of range data points
        uiOutput("outOfRangeMessage"),
      ),
      #--------------------------------------------
      # Adjusts the plot with the corresponding buttons
      tags$head(
        tags$style(HTML("
          .shiny-fluid-row {
              display: flex;
              flex-wrap: nowrap;
              align-items: stretch;
            }
          .shiny-column {
            min-width: 150px;
            margin-right: 0px;
          }
          .buttons-column {
            display: flex;
            flex-direction: column;
            align-items: flex-start;
            margin-right: 0px;
          }
          .buttons-panel {
            padding-top: 90px;
            display: block;
            width: 100%;
            margin-right: 0px;
            margin-bottom: 50px;
          }
          .plot-container {
              flex: 1;
              display: flex;
              flex-direction: column; 
          }
          "))
      ),
      fluidRow(class = "shiny-fluid-row",
               column(class = "shiny-column buttons-column", width = 2,
                      wellPanel(class = "buttons-panel",
                                uiOutput("dynamicButtons")
                      )
               ),
               column(class = "shiny-column", width = 10,
                      wellPanel(class = "plot-container",
                        plotOutput("plot1", width = "100%"), click = "clicked", hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
                        uiOutput("hover_info", style = "pointer-events: none")
                      )
               )
      ),
      #-----------------------------------------
    ),
  fluidRow(
    column(6,
      wellPanel(
        dataTableOutput('mydata'),
        actionButton('save_to_global', "Tabelle speichern", style = "margin-top: 20px"),
        actionButton("remove", "Eintrag löschen", style = "margin-top: 20px")
      )
    ),
    # the detailed view of a title
    column(6,
       wellPanel(
         plotOutput("timeSeriesPlot"),
         plotOutput("detailPlot")
       )
    )
  )
)

