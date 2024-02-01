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

# load data 
df_kapitel <- read_csv("./Data/hh_sh_ep14_kapitel.csv", col_types = cols(Kapitel = col_character()))
df_zweck <- read_csv("./Data/hh_sh_ep14_zweck.csv", col_types = cols(Kapitel = col_character(), Gesamttitel = col_character()))
df_zweck <- slice(df_zweck, 21:40) # subset (20 rows), can be uncommented later

### ui
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
          choiceNames = c("Soll-Werte", "Ist-Werte", "Differenz"),
          choiceValues = c("df_soll", "df_ist", "df_diff"),
          justified = TRUE
        ),
        pickerInput(
          inputId = "pickKapitel",
          label = "Wählen Sie die Kapitel. ",
          choices = df_kapitel$Kapitel[1:3],
          multiple = TRUE,
          selected = df_kapitel$Kapitel[1:3]
        ),
        pickerInput(
          inputId = "pickTitel",
          label = "Wählen Sie die Titel. ",
          choices = df_zweck$Gesamttitel,
          selected = df_zweck$Gesamttitel,
          multiple = TRUE,
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
          choices = c(-10000,-1000,0,100,1000,10000,50000,100000),
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
              border: 1px solid #D3D3D3;
              padding-bottom: 34px;
          }
          .shiny-column {
            min-width: 150px;
            margin-right: 0px;
          }
          .buttons-column {
            display: flex;
            flex-direction: column;
            align-items: flex-start;
            justify-content: flex-start;
            margin-right: -60px;
            position: relative;
            z-index: 9999;
          }
          .buttons-panel {
            margin-right: 0px;
            display: block;
            width: 100%;
            border: none;
            box-shadow: none;
          }
          .plot-container {
              flex: 1;
              display: flex;
              flex-direction: column;
          }
          @media screen and (min-aspect-ratio: 16/9) {
            .buttons-panel {
              padding-top: 50px;
            }
          }
          @media screen and (max-aspect-ratio: 16/9) {
            .buttons-panel {
              padding-top: 58px;
            }
          }
        "))
      )
      ,
      fluidRow(class = "shiny-fluid-row",
               column(class = "shiny-column buttons-column", width = 1,
                      wellPanel(class = "buttons-panel", style = "background-color: transparent;",
                                tags$div(style = "background-color: #197084; color: white; padding: 5px; display: inline-block;", "Details:"),
                                uiOutput("dynamicButtons")
                      )
               ),
               column(class = "shiny-column", width = 10,
                      wellPanel(class = "plot-container", style = "border: 0px solid #D3D3D3;",
                                plotOutput("plot1", width = "100%", click = "clicked", hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
                        uiOutput("hover_info", style = "pointer-events: none"))
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
         plotOutput("combinedPlot")
       )
    )
  )
)

