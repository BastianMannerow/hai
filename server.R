###################### server for marking outliers app ######################
### load libraries
library(rsconnect)
library(shiny)
library(tidyverse)
library(sysfonts)
library(showtext)
library(reshape2)
library(readxl)
library(DT)
library(scales)
library(grid)
library(fontawesome)
library(patchwork)
library(shinyjs)

# own methods
source("visualisation/detailedView.R")
source("visualisation/mainPlot.R")
source("visualisation/anomalyTable.R")
source("utilities/dynamicButtons.R")
source("utilities/slider.R")
source("utilities/pointsNotVisibleWarning.R")
source("utilities/importData.R")
source("utilities/calculateMainPlotHeight.R")
source("utilities/handleWindowInput.R")
source("utilities/plotAnomalyTableAdapter.R")
source("utilities/dataFiltering.R")

### load Roboto font and change scale view
font_add_google("Roboto Condensed", family = "Roboto")
showtext_auto()
options(scipen = 999)

#------------------------------------------------------------------------------- Options
plot_font_family <- "Roboto"
legend_size = 5
mini_headline_font_size <- 16

headline_font_size <- paste0(mini_headline_font_size * 1.5)
normal_text_font_size <- paste0(mini_headline_font_size * 0.8)
axis_font_size <- 5

#------------------------------------------------------------------------------- Import data

### get data
df_ist <- importDFIst()
df_soll <- importDFSoll()
df_diff <- importDFDiff()
df_kapitel <- importDFKapitel()
df_zweck <- importDFZweck()
df_anomaly <- importDFAnomaly()

### set up server
shinyServer(function(input, output, session) {
  # Handling of the screen size and triggers refreshing of the mainPlot
  sourceRefreshPlot(session)
  
  # Varibales which force the mainPlot to refresh
  refreshMainPlot <- reactiveVal(TRUE)
  lastDimensions <- reactiveValues(width = 1, height = 1)
  
  observe_external(session, lastDimensions, refreshMainPlot)
  
  # ---------------------------------------------------------------------------- Reactive Data
  ## Make a point unclicked
  last_click <- reactiveVal(NULL)
  
  # Handles the data filtering based on user input
  curr_art <- reactiveVal()
  observe({
    curr_art(input$pickArt)
  })
  
  # the min and max year based on slider, which are used for the mainPlot title
  selected_years <- reactive({
    as.numeric(input$pickZeitraum)
  })
  
  minYear <- reactive({
    if (is.null(selected_years())) {
      return(NULL)
    }
    min(selected_years())
  })
  
  maxYear <- reactive({
    if (is.null(selected_years())) {
      return(NULL)
    }
    max(selected_years())
  })
  
  # Function to generate the title for mainPlot based on user selection
  scatterTitle <- reactive({
    if (is.null(minYear()) || is.null(maxYear())) {
      return("Zeitraum wird initialisiert.")
    }
    createScatterTitle(input, minYear(), maxYear())
  })
  
  # The selected title, needed for dynamic buttons and detail view
  selectedTitle <- reactiveVal()
  
  # an anomaly dataframe to distinguish between AI and User
  anomalies <- reactiveValues(data = data.frame(Gesamttitel = character(), Jahr = numeric(), Anomalie = logical(), Art = character()))
  
  # the dataframe for the anomalies presented in the table
  anomaly_table <- reactiveValues(x = df_anomaly)
  
  # df to save anomaly points (from the table) for coloring in the plot
  selected <- createSelectedPoints(input, anomaly_table, curr_art)
  
  # data when a category is picked by the user (soll, ist, diff)
  pickedCategoryDataframe <- reactive({
    get(input$pickArt)
  })
  
  # save current choices from pickTitel in a reactive value
  current_titel <- reactiveVal()
  observe({
    current_titel(input$pickTitel)
    session$clientData$output_plot1_width
  })
  
  ## filter data for the main plot
  scatterData <- createScatterData(input, pickedCategoryDataframe, anomalies, anomaly_table$x)
  
  # dataframe for the sliders
  sliderDataframe <- createSliderDataframe(input, df_ist, df_soll, df_diff)
  
  #------------------------------------------------------------------------------ Functionality
  # when pickKapitel is changed, the choices for pickTitel are changed accordingly
  updateTitleChoices(input, session, df_zweck, current_titel)
  
  # Warns about missing datapoints
  output$outOfRangeMessage <- generateOutOfRangeMessage(pickedCategoryDataframe, input, scatterData)
  
  # Overrides the time and value slider for a dynamic effect
  dynamicSlider <- updateTimeSlider(session, sliderDataframe)
  updateValueSlider(session, sliderDataframe)
  
  # Calculates the button and plot height and width
  getbutton_width <- reactive({
    getButtonWidth(session)
  })
  getbutton_height <- reactive({
    calculateButtonHeight(normal_text_font_size)
  })
  getPlotHeight <- reactive({
    calculatePlotHeight (input, scatterData, session, getbutton_height)
  })
  
  # Create buttons for each entry on y axis
  output$dynamicButtons <- renderUI({
    df_scatter <- scatterData()
    generateDynamicButtons(df_scatter, df_zweck, selectedTitle, normal_text_font_size, plot_font_family, getbutton_height, getbutton_width)
  })
  
  # Checks if a dynamic button was pressed
  observe({
    df_scatter <- scatterData()
    observeButtonPress(input, df_scatter, selectedTitle)
  })
  # Adapter between MainPlot and Anomaly Table
  setupAnomalyInteractions(input, output, session, anomaly_table, curr_art, scatterData, last_click, anomalies)
  
  #------------------------------------------------------------------------------ Visualisation
  ## visualize the main plot
  output$plot1 <- generateMainPlot(scatterData, input, session, getPlotHeight, selected, scatterTitle, plot_font_family, headline_font_size, normal_text_font_size, refreshMainPlot)
  # Hover Info
  output$hover_info <- generateMainPlotHoverInfo(input, scatterData, nearPoints)
  
  # Generate the detailed view
  observeEvent(c(selectedTitle(), anomaly_table$x), {
    generateDetailPlot(df_scatter, df_zweck, df_ist, df_soll, anomaly_table, selectedTitle,
                       plot_font_family, normal_text_font_size, mini_headline_font_size, output)
  })
  
  ## Anomaly Table
  output$mydata <- generateAnomalyTable(anomaly_table)
  setupDataTableInteractions(anomaly_table, session, input)
  callback <- c(
    '$("#remove").on("click", function(){',
    '  table.rows(".selected").remove().draw();',
    '});'
  )
})