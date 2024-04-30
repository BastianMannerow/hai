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
library(shinyjs)
library(fontawesome)
### load Roboto font and change scale view
font_add_google("Roboto Condensed", family = "Roboto")
showtext_auto()
options(scipen = 999)

# load data 
df_kapitel <- read_csv("./Data/hh_sh_ep14_kapitel.csv", col_types = cols(Kapitel = col_character()))
df_zweck <- read_csv("./Data/hh_sh_ep14_zweck.csv", col_types = cols(Kapitel = col_character(), Gesamttitel = col_character()))
df_zweck <- slice(df_zweck, 21:40) # subset (20 rows), can be uncommented later

### ui
ui <- fluidPage(includeCSS("www/style.css"),
  tags$head(
      tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Roboto+Condensed:wght@400;700&display=swap');
      
      /* Mini Headlines */
      body, html {
        /*font-size: 1em !important;*/
        /* color: red !important; */
        font-family: 'Roboto Condensed', sans-serif !important;
        color: black !important;
        font-size: 16px !important;
      }
      
      @media (min-width: 1000px) {
        body, html {
          font-size: 16px !important;
        }
      }

      @media (min-width: 3000px) {
        body, html {
          font-size: 32px !important;
        }
      }
      
      /* Headlines */
      h1, h2, h3, h4, h5, h6 {
        font-family: 'Roboto Condensed', sans-serif !important;
        color: black !important;
        font-size: 1.5em !important;
      }
      
      /* Dashboard zur Identifikation von Ausreißern in den Haushaltsdaten */
      h2{
        font-family: 'Roboto Condensed', sans-serif !important;
        color: black !important;
        font-size: 2em !important;
      }
      
      /* Normal text size */
      .btn, p, .input-group .form-control, .slider .slider-handle, .slider .slider-tick-label {
        font-family: 'Roboto Condensed', sans-serif !important;
        font-size: 0.8em !important;
      }
      
      .buttons-column {
          min-width: 120px !important;
          margin-right: -2.375em !important;
          margin-top: -1.4375em !important;
      }
      .custom-button:hover {
        background-color: #555;
        color: white;
      }
      
    ")),
    
    
    tags$script(HTML("$(document).on('shiny:connected', function(event) {
      function setSizes() {
        var screenWidth = window.screen.width;
        var screenHeight = window.screen.height;
        Shiny.setInputValue('screenSize', {width: screenWidth, height: screenHeight});
          
        var width = $(window).width();
        var height = $(window).height();
        Shiny.setInputValue('windowSize', {width: width, height: height});
      }
      setSizes();
      $(window).on('resize', setSizes);
    });"))
  ),
  tags$h1(style = "font-size: 2em;",
    tags$a(
    href="https://jil.sh/projekte/hai-kooperative-haushaltsaufstellung-mit-augmented-intelligence/", 
    tags$img(src="HAI_Logo.png", 
            alt="HAI",
            align = "center",
            width="50")
    ),
    span(style = "vertical-align: bottom; 
        font-size: 1.5em; 
        color: rgb(0,143,168);
        font-weight: bold;", 
        "aushalt Schleswig-Holstein"),
    tags$a(
      href="https://jil.sh", 
      tags$img(src="JIL_Logo.png", 
             alt="JIL",
             align = "right",
             width="200")
    )
  ),
  titlePanel("Dashboard zur Identifikation von Ausreißern in den Haushaltsdaten"),
  #fluidRow 1, ganz außen (hellgrau)
  fluidRow(column(12, style = 
                  "background-color: rgb(250,250,250); 
                  border: thin solid rgb(220,220,220);
                  margin: 10px 15px 10px 15px;", 
      h3("Daten im Überblick"),
  fluidRow(class = "Selection Column",
           column(3, #fluidRow 2, innere mit Input-Column
      p(HTML(paste(fa("filter"))),
        "Passen Sie die Filter-Optionen für die Graphik nach Ihrem Bedarf an."
      ),
      radioGroupButtons(
        inputId = "pickArt",
        label = "Wählen Sie die Art der Beträge.",
        choiceNames = c("Soll-Werte", "Ist-Werte", "Differenz"),
        choiceValues = c("df_soll", "df_ist", "df_diff"),
        justified = TRUE
      ),
      pickerInput(
        inputId = "pickKapitel",
        label = "Kapitel - Auswahl",
        choices = df_kapitel$Kapitel[1:3],
        multiple = TRUE,
        selected = df_kapitel$Kapitel[1:3]
      ),
      pickerInput(
        inputId = "pickTitel",
        label = "Titel - Auswahl",
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
        label = "Wählen Sie den Wertebereich.", 
        choices = c(-10000,-1000,0,100,1000,10000,50000,100000),
        selected = c(0,100000)
      ),
      # UI Output for the warning message of out of range data points
      uiOutput("outOfRangeMessage"),
    ), # Ende Input column
    column(9, # Plot-Spalte in fluidRow 2
      p(HTML(paste(fa("magnifying-glass-plus"))),
        "Bitte markieren Sie in der Graphik, welche Beträge Sie als Ausreißer bewerten. 
        Klicken Sie dafür auf einen Datenpunkt. Klicken Sie auf einen Titel, um eine 
        Detailansicht des Haushaltstitels zu öffnen."),
      #--------------------------------------------
      # Adjusts the plot with the corresponding buttons
      tags$head(
        tags$style(HTML("
        .shiny-fluid-row {
          display: flex;
          background-color: white;
          flex-wrap: nowrap;
          justify-content: center;
          align-items: flex-start;
          border: 1px solid #D3D3D3;
          overflow-x: auto;
        }
        .buttons-column {
          flex: 0 1 auto; 
          min-width: 120px;
          width: 8.33%; 
          align-self: center;
          display: flex;
          flex-direction: column;
          align-items: flex-start;
          justify-content: flex-start;
          position: relative;
          z-index: 9999;
        }
        .shiny-column {
          flex: 1 1 auto; 
          width: 91.66%;
        }
        .buttons-panel, .plot-container {
          width: 100%; 
          display: block;
          border: none;
        }
        .plot-container {
          background-color: transparent;
        }
        
        @media (max-width: 767px) {
        .buttons-column, .shiny-column {
          flex: 1 1 100%; 
          min-width: 0;
          }
        }
        
        @media (min-width: 1920px) {
        .buttons-column {
          min-width: 120px;
          margin-right: -38px;
          margin-top: -15px;
        }
  
      @media (min-width: 1680px) {
        .buttons-column {
          min-width: 120px;
          margin-right: -38px;
          margin-top: -23px;
        }
        
        @media (min-width: 1280px) {
        .buttons-column {
          min-width: 120px;
          margin-right: -38px;
          margin-top: -15px;
        }
        
        .shiny-column {
          align-self: center;
          flex-grow: 1;
        }
      "))
      ), # Ende head mit CSS Anpassungen
      fluidRow(class = "shiny-fluid-row", # fluidRow 3, Scatter-Plot
        column(class = "buttons-column", width = 1,
            wellPanel(class = "buttons-panel", style = "background-color: transparent;",
                      tags$div(style = "background-color: #197084; color: white; padding: 5px; display: inline-block; white-space: nowrap;",
                               "Titel", HTML(paste(rep("&nbsp;", 1), fa("arrow-pointer"), collapse = ""))),
              uiOutput("dynamicButtons")
            )
        ),
        column(class = "shiny-column", width = 11,
            wellPanel(class = "plot-container", style = "border: 0px solid #D3D3D3;",
                plotOutput("plot1", width = "100%", height = "100%", click = "clicked", hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
                uiOutput("hover_info", style = "pointer-events: none"))
        )
      ) # Ende fluidRow 3
      #----------------------------------------- 
    ) # Ende Plot-Spalte in fluidRow 2
  ) # Ende fluidRow2
  )), # Ende column und fluidRow 1
      
  fluidRow(
    column(6,
      wellPanel(
        h3("Tabelle: Identifizierte Ausreißer"),
        div(
          dataTableOutput('mydata'), 
          class = "text-center"
        ),
        actionButton("remove", "Markierte Elemente löschen", style = "margin-top: 20px")
      )
    ),
    # the detailed view of a title
    column(6,
      wellPanel(
        h3("Titel im Detail"),
        plotOutput("combinedPlot")
      )
    )
  )
)

