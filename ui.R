###################### ui for marking outliers app ######################
### load libraries
#library(easypackages)
#libraries("shiny", "tidyverse", "sysfonts", "showtext", "dplyr", "reshape2", "readxl", "DT")
library(shiny)
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
  
  navbarPage("",
             tabPanel("Version 1",
                      fluidPage(
                        p("Hinweis: Hier befinden sich alle Datenpunkte auf einer Skala zwischen 0 und 1000."),
                        wellPanel(
                        div(
                          style = "position:relative",
                          # set height of plot to 400px, otherwise problems with hover function as sometimes the hover$coords_css info was
                          # not the same as the image size created by plotOutput
                          plotOutput("plot1", height = "400px", width = "100%", click = "clicked", hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
                          uiOutput("hover_info", style = "pointer-events: none")
                        ),
                        dataTableOutput('mydata'),
                        actionButton('save_to_global', "Tabelle speichern", style = "margin-top: 20px"),
                        actionButton("remove", "Eintrag löschen", style = "margin-top: 20px"),
                      ))
             ),
             tabPanel("Version 2",
                      fluidPage(
                        p("Hinweis: Hier handelt es sich um dieselbe Datenverteilung wie in Version 1, nur dass \nes zwei Datenpunkte gibt, die deutlich größer sind. Deshalb \nverschiebt sich die Skala von 0 bis 1000 zu 0 bis 15000."),
                        wellPanel(
                        div(
                          style = "position:relative",
                          plotOutput("plot2", height = "400px", width = "100%"))
                      ))),
             tabPanel("Version 3",
                      fluidPage(
                        p("Hinweis: Hier wird für jede Haushaltsposition des Produkts eine eigene Graphik mit individueller Skala erstellt."),
                        wellPanel(
                          div(
                            style = "position:relative",
                            plotOutput("plot3", height = "150px", width = "100%"),
                            plotOutput("plot4", height = "150px", width = "100%"),
                            plotOutput("plot5", height = "150px", width = "100%")
                          )
                        )
                      )),
             tabPanel("Version 4",
                      fluidPage(
                        p("Hinweis: Hier werden alle Auszahlungswerte für das Produkt aufgelistet."),
                        wellPanel(
                          div(
                            style = "position:relative",
                            plotOutput("plot6", height = "1200px", width = "100%"),
                          )
                        )
                      ))
             
  )
)


