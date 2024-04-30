###################### server for marking outliers app ######################
### load libraries
library(rsconnect)
#library(easypackages)
#libraries("shiny", "tidyverse", "sysfonts", "showtext", "dplyr", "reshape2", "readxl", "DT")
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
source("plots/detailedView.R")
source("plots/mainPlot.R")
source("utilities/dynamicButtons.R")
source("utilities/slider.R")
source("utilities/pointsNotVisibleWarning.R")
source("utilities/importData.R")
source("utilities/calculateMainPlotHeight.R")
source("utilities/handleWindowInput.R")

### load Roboto font and change scale view
font_add_google("Roboto Condensed", family = "Roboto")
showtext_auto()
options(scipen = 999)

#-------------------------------------------------------------------------------
# Global Changes
plot_font_family <- "Roboto"
legend_size = 5
mini_headline_font_size <- 16

headline_font_size <- paste0(mini_headline_font_size * 1.5)
normal_text_font_size <- paste0(mini_headline_font_size * 0.8)
axis_font_size <- 5
#-------------------------------------------------------------------------------

### get data
df_ist <- importDFIst()
df_soll <- importDFSoll()
df_diff <- importDFDiff()
df_kapitel <- importDFKapitel()
df_zweck <- importDFZweck()


# a persistent anomaly dataframe to distinguish between AI and User
anomalies <- reactiveValues(data = data.frame(Gesamttitel = character(), Jahr = numeric(), Anomalie = logical()))

### set up server
shinyServer(function(input, output, session) {
  # Handling of the screen size and triggers refreshing of the mainPlot
  sourceRefreshPlot(session)
  sourceHandleWindowSize
  
  # ---------------------------------------------------------------------------- Reactive Data
  
  ## reading for selecting dataset reactive: https://stackoverflow.com/questions/57128917/update-pickerinput-by-using-updatepickerinput-in-shiny
  # reaktiver Platzhalter für aktuelles df
  reac_data <- reactive({
    get(input$pickArt)
  })
  curr_art <- reactiveVal()
  observe({
    curr_art(input$pickArt)
  })
  
  # save current choices from pickTitel in a reactive value, to save them as selected 
  # when pickKapitel is changed, further reading: https://stackoverflow.com/questions/60122122/shiny-observeevent-updateselectinput-inputs-resetting
  current_titel <- reactiveVal()
  observe({
    current_titel(input$pickTitel)
    session$clientData$output_plot1_width
  })
  
  ## filter data for scatter plot
  scatterData <- reactive({
    selected_years <- as.numeric(input$pickZeitraum)
    selected_values <- input$pickWertebereich
    selected_title <- input$pickTitel
    numeric_cols <- names(reac_data())[!grepl("Anomalie", names(reac_data())) & sapply(reac_data(), is.numeric)]
    df_scatter <- melt(reac_data(), id.vars = "Gesamttitel", measure.vars = numeric_cols)
    colnames(df_scatter)[which(names(df_scatter) == "variable")] <- "year"
    df_scatter$year <- as.numeric(as.character(df_scatter$year))
    
    df_scatter <- df_scatter %>%
      filter(year >= selected_years[1] & year <= selected_years[2],
             value >= selected_values[1] & value <= selected_values[2],
             Gesamttitel %in% selected_title)
    
    df_scatter$anomaly <- sapply(1:nrow(df_scatter), function(i) {
      row <- df_scatter[i,]
      anomaly_col_name <- paste0(row$year, "_Anomalie")
      if(anomaly_col_name %in% names(reac_data())) {
        # searches for the anomaly based on year and title
        anomaly_value <- reac_data()[reac_data()$Gesamttitel == row$Gesamttitel, anomaly_col_name]
        # handles empty entries
        if(length(anomaly_value) > 0 && !is.na(anomaly_value)) {
          return(anomaly_value)
        } else {
          return(NA)
        }
      } else {
        return(NA)
      }
    })
    
    # override persistent anomaly clicks
    df_scatter$anomaly <- mapply(function(title, year) {
      any(anomalies$data$Gesamttitel == title & anomalies$data$Jahr == year)
    }, df_scatter$Gesamttitel, df_scatter$year)
    
    return(df_scatter)
  })
  
  # Receive relevant data
  scatterDataframe <- reactive({
    if (input$pickArt == "df_ist"){
      dataframe <- df_ist
    } else if (input$pickArt == "df_soll"){
      dataframe <- df_soll
    } else if (input$pickArt == "df_diff"){
      dataframe <- df_diff
    }
    selected_title <- input$pickTitel
    dataframe <- dataframe %>%
      filter(Gesamttitel %in% selected_title)
    
    return(dataframe)
  })
  
  # ---------------------------------------------------------------------------- Switch between dataframes in view
  # when pickKapitel is changed, the choices for pickTitel are changed accordingly
  observeEvent(input$pickKapitel, {
    updatePickerInput(
      session = session,
      inputId = "pickTitel",
      choices = filter(df_zweck, df_zweck$Kapitel %in% input$pickKapitel)["Gesamttitel"],
      selected = current_titel())
  }, ignoreInit = TRUE)
  
  
  
  #------------------------------------------------------------------------------ Functionality
  # Warns about missing datapoints
  output$outOfRangeMessage <- renderUI({
    if (pointsOutsideRange(reac_data, input$pickTitel, scatterData) > 0) {
      span(style = "color: red;", paste(pointsOutsideRange(reac_data, input$pickTitel, scatterData), "Datenpunkte liegen außerhalb des angezeigten Bereichs."))
    } else {
      return(NULL)
    }
  })
  
  # Overrides the time slider for a dynamic effect
  dynamicSlider <- updateTimeSlider(session, scatterDataframe)
  
  # Overrides the value slider for a dynamic effect
  updateValueSlider(session, scatterDataframe)
  
  # Calculates the main plots height
  
  # Count the amount of buttons, to scale the plot in ui.R
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
  
  # Define reactiveVal for selected title
  selectedTitle <- reactiveVal()
  
  # Checks if a button was pressed
  observe({
    df_scatter <- scatterData()
    observeButtonPress(input, df_scatter, selectedTitle)
  })
  
  # Generate the detailed view
  observeEvent(selectedTitle(), {
    generateDetailPlot(df_scatter, df_zweck, df_ist, df_soll, rv, selectedTitle,
                       plot_font_family, normal_text_font_size, mini_headline_font_size, output)
  })
  
  ## visualize the main plot
  output$plot1 <- generateMainPlot(scatterData, input, session, getPlotHeight, selected, scatterTitle, plot_font_family, headline_font_size, normal_text_font_size)
  #------------------------------------------------------------------------------
  
  scatterTitle <- reactive({
    if (input$pickArt == "df_ist"){
      titel <- "Verteilung der Ist-Werte 2012 bis 2021 (in Euro)"
    } else if (input$pickArt == "df_soll"){
      titel <- "Verteilung der Soll-Werte 2012 bis 2021 (in Euro)"
    } else if (input$pickArt == "df_diff"){
      titel <- "Verteilung der Differenz 'Soll-Ist' von 2012 bis 2021 (in Euro)"
    }
    return(titel)
  })
  
  
  ## visualize hover info and tooltip, copied from: https://gitlab.com/-/snippets/16220
  # new information for cursor position was added to plot_hover in 2018: https://github.com/rstudio/shiny/pull/2183
  output$hover_info <- renderUI({
    hover <- input$plot_hover 
    if (nrow(scatterData()) == 0) return(NULL)
    point <- nearPoints(scatterData(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate horizontal and vertical point position inside the image as percent of total dimensions
    h_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    v_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # create style property for tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure the tooltip will be on top
    # pixel coordinates are dependency of the horizontal and vertical position of the cursor, reaching
    # the right or bottom edge of plotting changes the position of the tooltip panel
    if (h_pct < 0.8 && v_pct < 0.8){
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", hover$coords_css$x + 2, "px; top:", hover$coords_css$y +2, "px;")
    } else if (h_pct >=0.8 && v_pct <0.8){ # cursor close to right edge
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", hover$coords_css$x - 300, "px; top:", hover$coords_css$y +2, "px;")
    } else if (h_pct < 0.8 && v_pct >=0.8){ # cursor close to bottom edge
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", hover$coords_css$x + 2, "px; top:", hover$coords_css$y - 150, "px;")
    } else {
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", hover$coords_css$x - 300, "px; top:", hover$coords_css$y - 150, "px;")
    }
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Titel: </b>", point$Gesamttitel, "<br/>",
                    "<b> Jahr: </b>", point$year, "<br/>",
                    "<b> Wert: </b>", point$value, "<br/>")))
    )
  })
  
  # initialize a tibble with anomalies from AI output (artifical data, not real)
  df_new <- read_csv("./Data/hh_sh_ep14_fakeAI.csv", col_types = "cccdcc")
  
  # add the icons
  df_new <- df_new %>% mutate(Ursprung = if_else(startsWith(Ursprung, "User"),
                                                 paste(fa("user"), "Nutzer"),
                                                 if_else(startsWith(Ursprung, "AI"),
                                                         paste(fa("microchip"), "KI System"),
                                                         Ursprung)))
  # save the tibble as reactive value
  rv <- reactiveValues(x = df_new)
  
  ## render table
  output$mydata <- renderDT({
    data = isolate(rv$x) # isolate inhibits dependency between data and render-function, datatable is only updated, not rendered, when data changes
    datatable(data,
              escape = FALSE,
              extensions = "Buttons", 
              editable = list(target = "cell", disable = list(columns = c(0, 1, 2, 3, 4))),
              class = 'compact stripe', 
              caption = "Hier erscheinen Ihre ausgewählten Datenpunkte. Ergänzen Sie im Kommentar, warum Sie den Datenpunkt als Ausreißer werten. Sie können Einträge auch löschen.",
              rownames = FALSE, 
              options = list(
                paging = FALSE,
                searching = FALSE,
                fixedColumns = FALSE,
                autoWidth = FALSE,
                dom = 'Bfrtip',
                info = FALSE,
                buttons = list(list(
                  extend = 'collection',
                  buttons = c('csv', 'excel', 'pdf'),
                  text = 'Download Ausreißer-Liste'
                )),
                ordering = TRUE,  # Aktiviert die Sortierung für die Tabelle
                columnDefs = list(
                  list(className = 'dt-center', targets = "_all"),
                  list(orderable = TRUE, targets = c(0, 1, 2, 3, 4)),  # enables sorting for all columns
                  list(orderable = FALSE, targets = 5)  # Deactivates the commentar function
                )
              ))
  })
  
  callback <- c(
    '$("#remove").on("click", function(){',
    '  table.rows(".selected").remove().draw();',
    '});'
  )
  
  ## Helpful blog articles that show the use of reactive datatables, proxy and replaceData
  ## https://www.travishinkelman.com/dt-datatable-crud/
  ## https://thatdatatho.com/r-shiny-data-table-proxy-replace-data/
  
  ## create a proxy object for our datatable-object, this allows us to manipulate the datatable
  proxy <- dataTableProxy('mydata')
  # set an observer to reactive values inside the observer-function (our reactive value is rv$x)
  # every time rv$x is changed, the observer calls for the replaceData-function and updates the proxy-object (i.e. our datatable)
  observe({
    replaceData(proxy, unique(rv$x), resetPaging = FALSE, rownames = FALSE) 
    # resetPaging and rownames are needed to set to FALSE, because in our renderDT-function we set them to FALSE, 
    # otherwise no matching table found
  })
  
  ## df to save anomaly points (from the table) for coloring in the plot
  selected <- reactive({
    ## get data from rv$x to the same structure like in df and 
    # filter curr_art (Betragsart) and input$pickTitel
    if (curr_art() == "df_ist"){
      selected_points <- rv$x %>% filter(Art == "Ist") %>%
        select(Gesamttitel=Titel, year=Jahr, value=Wert, Ursprung)
    } else if (curr_art() == "df_soll"){
      selected_points <- rv$x %>% filter(Art == "Soll") %>%
        select(Gesamttitel=Titel, year=Jahr, value=Wert, Ursprung)
    } else{
      selected_points <- rv$x %>% filter(Art == "Diff") %>%
        select(Gesamttitel=Titel, year=Jahr, value=Wert, Ursprung)
    }
    if (!is.null(input$pickTitel)){
      selected_points <- filter(selected_points, Gesamttitel %in% input$pickTitel)
    }
    
    # Update Ursprung column based on presence of "Nutzer"
    selected_points <- selected_points %>%
      mutate(Ursprung = str_detect(Ursprung, "Nutzer"))
    
    return(selected_points)
  })
  
  ## anomaly toggling to distinguish between user and ai
  toggleAnomaly <- function(title, year) {
    if(any(anomalies$data$Gesamttitel == title & anomalies$data$Jahr == year)) {
      anomalies$data <- anomalies$data[!(anomalies$data$Gesamttitel == title & anomalies$data$Jahr == year), ]
    } else {
      anomalies$data <- rbind(anomalies$data, data.frame(Gesamttitel = title, Jahr = year, Anomalie = TRUE))
    }
  }
  
  ## add data to table from clicked points in plot
  observeEvent(input$clicked, {
    if (curr_art() == "df_ist"){
      art <- "Ist"
    } else if (curr_art() == "df_soll"){
      art <- "Soll"
    } else {
      art <- "Diff"
    }
    
    pointsnear <- nearPoints(scatterData(), input$clicked, threshold = 5, maxpoints = 1)
    if (nrow(pointsnear) > 0) {
      pointsnear$year <- as.character(pointsnear$year)
      toggleAnomaly(pointsnear$Gesamttitel, as.numeric(pointsnear$year))
      
      # check if already clicked
      if (!is.null(last_click()) && identical(last_click(), list(pointsnear$Gesamttitel, pointsnear$year, pointsnear$value, art))) {
        # delete for next clicking
        rv$x <- rv$x %>% 
          filter(!(Titel == pointsnear$Gesamttitel & Jahr == pointsnear$year & Wert == pointsnear$value & Art == art))
        last_click(NULL)
      } else {
        rv$x <- rv$x %>% 
          bind_rows(tibble(Ursprung = "User", Titel = pointsnear$Gesamttitel, Jahr = pointsnear$year, Wert = pointsnear$value, Art = art, Kommentar = ""))
        rv$x <- rv$x %>% 
          mutate(Ursprung = if_else(startsWith(Ursprung, "User"),
                                    paste(fa("user"), "Nutzer"),
                                    if_else(startsWith(Ursprung, "AI"),
                                            paste(fa("microchip"), "KI System"),
                                            Ursprung)))
        last_click(list(pointsnear$Gesamttitel, pointsnear$year, pointsnear$value, art))
      }
    }
  })
  
  ## Make a point unclicked
  last_click <- reactiveVal(NULL)
  
  ## delete rows in data table
  observeEvent(input[["remove"]], {
    req(input[["mydata_rows_selected"]])
    indices <- input[["mydata_rows_selected"]]
    rv$x <- rv$x %>% filter(!row_number() %in% indices)
    session$clientData$output_plot1_width
  })
  
  ## save comments in datatable
  observeEvent(input$mydata_cell_edit, {
    info = input$mydata_cell_edit
    i = info$row
    j = info$col + 1 # column index offset of 1 because ID column (rownames) is hidden
    v = info$value
    rv$x[i,j] = v
    session$clientData$output_plot1_width
  })
  
  ## save table to global variable
  observeEvent(input$save_to_global, {
    assign('df_ausreißer', unique(rv$x), envir = .GlobalEnv)
    saveRDS(df_ausreißer, file = "resultData.rds")
    # Pop-Up Dialog after table was saved, more information here: https://shiny.rstudio.com/reference/shiny/latest/modaldialog
    showModal(modalDialog(
      title = "Vielen Dank!",
      "Die Tabelle wurde gespeichert."
    ))
    session$clientData$output_plot1_width
  })
})


### open RDS Files
# filename <- file.choose()
# resultData <- readRDS(filename)
