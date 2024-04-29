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
df_ist <- read_csv("./Data/hh_sh_ep14_ist.csv", col_types = cols(Gesamttitel = col_character()))
df_soll <- read_csv("./Data/hh_sh_ep14_soll.csv", col_types = cols(Gesamttitel = col_character()))
df_diff <- read_csv("./Data/hh_sh_ep14_diff.csv", col_types = cols(Gesamttitel = col_character()))
df_kapitel <- read_csv("./Data/hh_sh_ep14_kapitel.csv", col_types = cols(Kapitel = col_character()))
df_zweck <- read.table("./Data/hh_sh_ep14_zweck.csv", sep = ",", header = TRUE, 
                       fileEncoding = "UTF-8", 
                       colClasses = c(Kapitel="character", Gesamttitel="character"))

# Should be moved into an own method, but here we are...
#df_ist <- df_ist %>% mutate(Gesamttitel = paste(substr(Gesamttitel,1,4), substr(Gesamttitel,5,6), substr(Gesamttitel,7,9), sep = " "))
#df_soll <- df_soll %>% mutate(Gesamttitel = paste(substr(Gesamttitel,1,4), substr(Gesamttitel,5,6), substr(Gesamttitel,7,9), sep = " "))
#df_diff <- df_diff %>% mutate(Gesamttitel = paste(substr(Gesamttitel,1,4), substr(Gesamttitel,5,6), substr(Gesamttitel,7,9), sep = " "))
#df_zweck <- df_zweck %>% mutate(Gesamttitel = paste(substr(Gesamttitel,1,4), substr(Gesamttitel,5,6), substr(Gesamttitel,7,9), sep = " "))

# Adding the column Anomalies for each year to be able to distinguish between AI and Human anomaly
df_ist <- df_ist %>%
  mutate(across(matches("^[0-9]"), ~., .names = "{.col}_Anomalie")) %>%
  mutate(across(ends_with("Anomalie"), ~ 0))
df_soll <- df_soll %>%
  mutate(across(matches("^[0-9]"), ~., .names = "{.col}_Anomalie")) %>%
  mutate(across(ends_with("Anomalie"), ~ 0))
df_diff <- df_diff %>%
  mutate(across(matches("^[0-9]"), ~., .names = "{.col}_Anomalie")) %>%
  mutate(across(ends_with("Anomalie"), ~ 0))

# a persistent anomaly dataframe to distinguish between AI and User
anomalies <- reactiveValues(data = data.frame(Gesamttitel = character(), Jahr = numeric(), Anomalie = logical()))



### set up server
shinyServer(function(input, output, session) {
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
  # when pickKapitel is changed, the choices for pickTitel are changed accordingly
  observeEvent(input$pickKapitel, {
    updatePickerInput(
      session = session,
      inputId = "pickTitel",
      choices = filter(df_zweck, df_zweck$Kapitel %in% input$pickKapitel)["Gesamttitel"],
      selected = current_titel())
  }, ignoreInit = TRUE)
  
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
        # Sucht den Anomaliewert basierend auf dem Gesamttitel und dem Jahr
        anomaly_value <- reac_data()[reac_data()$Gesamttitel == row$Gesamttitel, anomaly_col_name]
        # Stellt sicher, dass der Anomaliewert nicht leer oder NA ist
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
  
  
  
  
  ## filter data for scatter plot
  alternative_scatterData <- reactive({
    selected_title <- input$pickTitel
    
    # ignore Anomaly
    numeric_cols <- names(reac_data())[!grepl("Anomalie", names(reac_data())) & sapply(reac_data(), is.numeric)]
    df_scatter <- melt(reac_data(), id.vars = "Gesamttitel", measure.vars = numeric_cols)
    colnames(df_scatter)[which(names(df_scatter) == "variable")] <- "year"
    df_scatter$year <- as.numeric(as.character(df_scatter$year))
    
    df_scatter <- df_scatter %>%
      filter(Gesamttitel %in% selected_title) %>%
      drop_na(value)  # Remove NA
    
    return(df_scatter)
  })
  
  
  # Counts data points, which are not visible
  pointsOutsideRange <- reactive({
    filtered_out_count <- nrow(alternative_scatterData()) - nrow(scatterData())
    filtered_out_count
  })
  
  # Implements a message to warn the user about non-visible points
  output$outOfRangeMessage <- renderUI({
    if (pointsOutsideRange() > 0) {
      span(style = "color: red;", paste(pointsOutsideRange(), "Datenpunkte liegen außerhalb des angezeigten Bereichs."))
    } else {
      return(NULL)
    }
  })
  
  #------------------------------------------------------------------------------
  # Count the amount of buttons, to scale the plot in ui.R
  number_of_buttons <- reactive({
    df_scatter <- scatterData() 
    titelListe <- unique(df_scatter$Gesamttitel)
    length(titelListe)
  })
  
  # Create buttons for each entry on y axis
  output$dynamicButtons <- renderUI({
    df_scatter <- scatterData()
    generateDynamicButtons(df_scatter, df_zweck, selectedTitle, normal_text_font_size, plot_font_family, getbutton_height, getbutton_width)
  })
  
  
  # Define reactiveVal for selected title
  selectedTitle <- reactiveVal()
  
  
  # Generate the detailed view
  observe({
    df_scatter <- scatterData() 
    titelListe <- factor(unique(df_scatter$Gesamttitel), levels = unique(df_scatter$Gesamttitel))
    sortedTitelListe <- rev(levels(titelListe))
    
    lapply(sortedTitelListe, function(titel) {
      btn_id <- paste0("button_", gsub(" ", "_", titel))
      observeEvent(input[[btn_id]], {
        selectedTitle(titel) # Update the reactive value
        
        # Rendering of combined plot
        output$combinedPlot <- renderPlot({
          req(selectedTitle())
          title <- selectedTitle()
          
          purpose <- df_zweck %>%
            filter(title == Gesamttitel) %>%
            select(Zweckbestimmung)
          
          # Get Data and removes Anomalie in year, whch ensures a clean x axis.
          ist_values <- df_ist %>%
            filter(Gesamttitel == title) %>%
            gather(key = "year", value = "value_ist", -Gesamttitel) %>%
            filter(!grepl("Anomalie", year))
          
          soll_values <- df_soll %>%
            filter(Gesamttitel == title) %>%
            gather(key = "year", value = "value_soll", -Gesamttitel) %>%
            filter(!grepl("Anomalie", year))
          
          combined_df <- left_join(ist_values, soll_values, by = "year") %>%
            mutate(difference = value_soll - value_ist,
                   fill_color = ifelse(difference < 0, "Negative Differenz", "Positive Differenz"))
          
          
          # Handle NA
          ist_values <- ist_values %>%
            na.omit()
          
          soll_values <- soll_values %>%
            na.omit() 
          
          
          # Import anomalies
          anomalies <- subset(rv$x, Titel == title)
          
          soll_anomalies <- anomalies %>% 
            filter(Art == "Soll")
          
          ist_anomalies <- anomalies %>% 
            filter(Art == "Ist")
          
          diff_anomalies <- anomalies %>% 
            filter(Art == "Diff")
          
          # Time Series
          timeSeriesPlot = generateTimeSeriesPlot(soll_values, ist_values, soll_anomalies, ist_anomalies, plot_font_family, normal_text_font_size)
          
          # Difference Plot (Second Half)
          differencePlot = generateDifferencePlot(combined_df, diff_anomalies, plot_font_family, normal_text_font_size)
          
          # Combine the Plots
          title_with_breaks <- insert_breaks_every_n_chars(paste(title, " - ", purpose))
          combinedPlot <- timeSeriesPlot / differencePlot + plot_layout(guides = "collect") + plot_annotation(title = title_with_breaks) + theme(plot.margin = margin(1, 1, 1, 1), plot.title = element_text(size = mini_headline_font_size, family = plot_font_family))
          
          return(combinedPlot)
        })
      })
    })
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
  
  # Overrides the time slider for a dynamic effect
  dynamicSlider <- updateTimeSlider(session, scatterDataframe)
  
  # Overrides the value slider for a dynamic effect
  updateValueSlider(session, scatterDataframe)
  
  # dynamic scaling for plot and button
  getbutton_width <- reactive({
    width <- session$clientData$output_buttonsPanel_width
    #width <- 100
    return(width)
  })
  
  getbutton_height <- reactive({
    numeric_part <- substr(normal_text_font_size, 1, nchar(normal_text_font_size) - 2)
    calc_temp <- as.numeric(numeric_part) * 3.5
    height <- calc_temp 
    return(height)
  })
  
  getPlotHeight <- reactive({
    if(input$screenSize$height == 1200) {
      height <- getbutton_height() * (number_of_buttons() + 2.6) + 12
    }
    else if(input$screenSize$height == 1080) {
      height <- getbutton_height() * (number_of_buttons() + 2.6) + 12
    }
    else if(input$screenSize$height == 720) {
      height <- getbutton_height() * (number_of_buttons() + 2.2) + 8
    }
    else{ # Apple
      height <- getbutton_height() * (number_of_buttons() + 1.8) + 12
    }
  })
  
  getPlotWidth <- reactive({
    #width <- 1200 * (1903/input$screenSize$width)
    width <- 1200 * (input$windowSize$width/1903)
    #width <- 1200
    return(width)
  })
  
  observeEvent(input$screenSize, {
    screen_width <- input$screenSize$width
    screen_height <- input$screenSize$height
    message(paste("Aktuelle Bildschirmauflösung: Breite =", screen_width, "Höhe =", screen_height))
  })
  
  observe({
    width <- input$windowSize$width
    height <- input$windowSize$height
    # message(paste("Aktuelle Fenstergröße: Breite =", width, "Höhe =", height))
  })
  
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
  
  ## visualize data
  output$plot1 <- renderPlot({
    df_scatter <- scatterData()
    if (nrow(df_scatter) == 0) {
      return(ggplot() +
               annotate("text", x = 0.5, y = 0.5, size = 5, 
                        label = "Keine Titel zur Ansicht ausgewählt.", vjust = 0.5, hjust = 0.5) +
               theme_void() +
               xlim(0, 1) + ylim(0, 1))
    }
    
    df_scatter$year <- as.numeric(as.character(df_scatter$year))
    last_years <- sapply(split(df_scatter, df_scatter$year), function(df) {
      if (all(is.na(df$value))) {
        return(NA)
      } else {
        return(max(df$year, na.rm = TRUE))
      }
    })
    last_year <- max(last_years, na.rm = TRUE)
    
    mainPlot = generateMainPlot(df_scatter, last_year, plot_font_family, headline_font_size, normal_text_font_size, scatterTitle, selected)
    
    selected_range <- sort(as.numeric(input$pickWertebereich))
    mainPlot <- mainPlot + xlim(selected_range[1], selected_range[2])
    mainPlot
  }, 
  # dynamic scaling of the plot
  height = function() {
    getPlotHeight()
  },
  width = function() {
    session$clientData$output_plot1_width
  }
  )
  observe({
    invalidateLater(1000, session) # Überprüft alle 1000 Millisekunden
    session$clientData$output_plot1_width
  })
  
  observe({
    width <- input$windowSize$width
    height <- input$windowSize$height
    session$clientData$output_plot1_width
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
