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

### load Roboto font and change scale view
font_add_google("Roboto Condensed", family = "Roboto")
showtext_auto()
options(scipen = 999)

# Variable which regulates the font of all text elements
plot_font_family <- "Roboto"

### get data
df_ist <- read_csv("./Data/hh_sh_ep14_ist.csv", col_types = cols(Gesamttitel = col_character()))
#df_ist <- slice(df_ist, 21:40) # subset (20 rows), can be uncommented later
df_soll <- read_csv("./Data/hh_sh_ep14_soll.csv", col_types = cols(Gesamttitel = col_character()))
#df_soll <- slice(df_soll, 21:40) # subset (20 rows), can be uncommented later
df_diff <- read_csv("./Data/hh_sh_ep14_diff.csv", col_types = cols(Gesamttitel = col_character()))
#df_diff <- slice(df_diff, 21:40) # subset (20 rows), can be uncommented later
df_kapitel <- read_csv("./Data/hh_sh_ep14_kapitel.csv", col_types = cols(Kapitel = col_character()))
df_zweck <- read.table("./Data/hh_sh_ep14_zweck.csv", sep = ",", header = TRUE, 
                       fileEncoding = "UTF-8", 
                       colClasses = c(Kapitel="character", Gesamttitel="character"))
#df_zweck <- slice(df_zweck, 21:40) # subset (20 rows), can be uncommented later

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
    # extract the time period
    selected_years <- as.numeric(input$pickZeitraum)
    # extract the range
    selected_values <- input$pickWertebereich
    # extract selected Gesamttitel
    selected_title <- input$pickTitel
    # melt data for scatter plot
    df_scatter <- melt(reac_data(), id.vars = "Gesamttitel")
    # Rename 'variable' column to 'year'
    colnames(df_scatter)[which(names(df_scatter) == "variable")] <- "year"
    
    # convert year column into numeric values
    df_scatter$year <- as.numeric(as.character(df_scatter$year))
    
    # execute the filtering
    df_scatter <- df_scatter %>%
      filter(year >= selected_years[1] & year <= selected_years[2],
             value >= selected_values[1] & value <= selected_values[2],
             Gesamttitel %in% selected_title)
    session$clientData$output_plot1_width
    return(df_scatter)
  })
  
  ## filter data for scatter plot
  alternative_scatterData <- reactive({
    # extract selected Gesamttitel
    selected_title <- input$pickTitel
    # melt data for scatter plot
    df_scatter <- melt(reac_data(), id.vars = "Gesamttitel")
    # Rename 'variable' column to 'year'
    colnames(df_scatter)[which(names(df_scatter) == "variable")] <- "year"
    
    # convert year column into numeric values
    df_scatter$year <- as.numeric(as.character(df_scatter$year))
    
    # execute the filtering
    df_scatter <- df_scatter %>%
      filter(Gesamttitel %in% selected_title) %>%
      drop_na(value)  # Remove rows where the value is NA
    
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
  number_of_buttons <- reactive({
    df_scatter <- scatterData() 
    titelListe <- unique(df_scatter$Gesamttitel)
    length(titelListe)
  })
  
  # Create buttons for each entry on y axis
  output$dynamicButtons <- renderUI({
    df_scatter <- scatterData()
    titelListe <- factor(df_scatter$Gesamttitel)
    
    sortedTitelListe <- rev(levels(titelListe))
    buttons <- lapply(sortedTitelListe, function(titel) {
      btn_id <- paste0("button_", gsub(" ", "_", titel))
      
      # dynamic css with font-family
      selected <- ifelse(!is.null(selectedTitle()) && !is.na(selectedTitle()) && titel == selectedTitle(), TRUE, FALSE)
      backgroundColor <- if(selected) "#841919" else "#197084" # Outlines a clicked button
      
      actionButton(
        inputId = btn_id,
        label = titel,
        class = "custom-button",
        style = paste0("font-size: 12px; background-color: ", backgroundColor, "; color: white; height: ", getbutton_height(), "px; width: ", getbutton_width(), "px; font-family: '", plot_font_family, "';")
      )
      
    })
    do.call(tagList, buttons)
  })
  
  
  # Define reactiveVal for selected title
  selectedTitle <- reactiveVal()
  
  # Handles long titles and purposes in the detailed plot with length of n
  insert_breaks_every_n_chars <- function(s, n = 95) {
    # dynamic characters to divide it
    regex <- sprintf("(?<=\\G.{%d})", n)
    
    # Divides the string
    parts <- strsplit(s, split = regex, perl = TRUE)[[1]]
    paste(parts, collapse = "\n")
  }
  
  
  
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
          
          title_with_breaks <- insert_breaks_every_n_chars(paste(title, " - ", purpose))
          
          # Get Data
          ist_values <- df_ist %>%
            filter(Gesamttitel == title) %>%
            gather(key = "year", value = "value_ist", -Gesamttitel)
          soll_values <- df_soll %>%
            filter(Gesamttitel == title) %>%
            gather(key = "year", value = "value_soll", -Gesamttitel)
          combined_df <- left_join(ist_values, soll_values, by = "year") %>%
            mutate(difference = value_soll - value_ist,
                   fill_color = ifelse(difference < 0, "Negative Differenz", "Positive Differenz"))
          
          # Handle NA
          ist_values <- na.omit(ist_values)
          soll_values <- na.omit(soll_values)
          
          # Import anomalies
          anomalies <- subset(rv$x, Titel == title)
          soll_anomalies <- anomalies %>% 
            filter(Art == "Soll")
          
          ist_anomalies <- anomalies %>% 
            filter(Art == "Ist")
          
          diff_anomalies <- anomalies %>% 
            filter(Art == "Diff")
          
          # Time Series
          soll_values$Anomalie <- ifelse(soll_values$year %in% soll_anomalies$Jahr, "Soll-Anomalie", "Soll-Werte")
          timeSeriesPlot <- ggplot(soll_values, aes(x = year, y = value_soll)) +
            geom_bar(aes(fill = Anomalie), stat = "identity", width = 0.7, show.legend = TRUE) +
            geom_line(data = ist_values, aes(x = year, y = value_ist, color = "Ist-Werte", group = 1), size = 2) +
            geom_point(data = ist_anomalies, aes(x = Jahr, y = Wert, color = "Ist-Anomalie"), size = 5) +
            scale_fill_manual(values = c("Soll-Werte" = "#d3d3d3", "Soll-Anomalie" = "#841919"), name = "Soll-Werte") +
            scale_color_manual(values = c("Ist-Werte" = "#197084", "Ist-Anomalie" = "#841919"), name = "Ist-Werte") +
            labs(y = "Absolutwerte") +
            theme_minimal() +
            theme(text = element_text(family = plot_font_family), axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank())
          
          combined_df$Anomalie <- ifelse(combined_df$year %in% diff_anomalies$Jahr, "Anomalie", "Negative Differenz")
          detailPlot <- ggplot(combined_df, aes(x = year, y = difference, fill = Anomalie)) +
            geom_bar(stat = "identity", aes(fill = ifelse(difference > 0, "Positive Differenz", Anomalie))) +
            geom_hline(yintercept = 0, linetype = "dashed") +
            scale_fill_manual(values = c("Anomalie" = "#841919", "Positive Differenz" = "#28841980", "Negative Differenz" = "#84191980"), name = "Differenz: Soll - Ist") +
            labs(y = "Zieldifferenz") +
            theme_minimal() +
            theme(text = element_text(family = plot_font_family), axis.title.x = element_blank())
          
          combinedPlot <- timeSeriesPlot / detailPlot + plot_layout(guides = "collect") + plot_annotation(title = title_with_breaks) + theme(plot.margin = margin(1, 1, 1, 1), plot.title = element_text(size = 12, family = plot_font_family))
          
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
  
  
  
  # Overrides the slider for a dynamic effect
  observe({
    years <- names(scatterDataframe())[-1]  # Deletes Gesamttitel
    years <- as.character(years)
    
    # 'selected' first and last year
    updateSliderTextInput(session, "pickZeitraum",
                          choices = years,
                          selected = c(years[1], tail(years, 1)))
    session$clientData$output_plot1_width
  })
  
  
  # Now the values
  observe({
    df <- scatterDataframe()
    df <- df[,-1]
    # Checks the interval of the whole dataframe
    if(all(is.na(df))) {
      minWert <- 0
      maxWert <- 1
    } else {
      minWert <- min(df, na.rm = TRUE)
      maxWert <- max(df, na.rm = TRUE)
    }
    
    #if(!is.finite(minWert)) minWert <- 0
    #if(!is.finite(maxWert)) maxWert <- 1
    
    # smart logarithmic rounding, to have a reasonable interval between slider inputs
    roundDownToNearest <- function(x) {
      exp <- ifelse(x == 0, 0, floor(log10(abs(x))))
      base <- 10^exp
      if(x > 0) {
        return(floor(x/base)*base)
      } else {
        return(-ceiling(abs(x)/base)*base)
      }
    }
    
    # Rounding up to an appropriate value
    roundUpToNearest <- function(x) {
      if(x == 0) return(0)
      exp <- floor(log10(abs(x)))
      base <- 10^exp
      if(x > 0) {
        upper <- ceiling(x/base)*base
        # examines if a multiple value is closer than base
        if (upper - x <= base / 2) {
          return(upper)
        } else {
          # rounding based on difference (important for a reasonable x axis)
          finerIncrement <- 10^(exp-1)
          finerUpper <- ceiling(x/finerIncrement)*finerIncrement
          if(finerUpper > x) {
            return(finerUpper)
          } else {
            return(upper) # shouldn't be used, just in case for bug fixing
          }
        }
      } else {
        # negative case
        lower <- -floor(abs(x)/base)*base
        return(lower)
      }
    }
    
    minWertRounded <- roundDownToNearest(minWert)
    maxWertRounded <- roundUpToNearest(maxWert)
    
    logMin <- log10(max(minWertRounded, 1))
    logMax <- log10(maxWertRounded)
    logRange <- seq(logMin, logMax, length.out = 10) # Number of slider choices
    choices <- 10^logRange
    choicesRounded <- unique(sapply(choices, roundUpToNearest))
    
    # final rounding to receive int from double
    choicesRounded <- round(choicesRounded)
    
    if (choicesRounded[1] > minWert) {
      choicesRounded[1] <- minWertRounded
    }
    if (choicesRounded[length(choicesRounded)] != maxWertRounded) {
      choicesRounded[length(choicesRounded)] <- maxWertRounded
    }
    
    values <- c(minWertRounded, maxWertRounded)
    
    updateSliderTextInput(session, "pickWertebereich",
                          choices = choicesRounded,
                          selected = values)
    session$clientData$output_plot1_width
  })
  
  # dynamic scaling for plot and button
  getbutton_width <- reactive({
    width <- session$clientData$output_buttonsPanel_width
    #width <- 100
    return(width)
  })
  
  getbutton_height <- reactive({
    height <- 40
    return(height)
  })
  
  getPlotHeight <- reactive({
    if(input$screenSize$height == 1200) {
      height <- getbutton_height() * (number_of_buttons() + 2.8) + 12
      print(height)
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
    #print(input$windowSize$width)
    width <- 1200 * (input$windowSize$width/1903)
    #print(width)
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
    message(paste("Aktuelle Fenstergröße: Breite =", width, "Höhe =", height))
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
               annotate("text", x = 10, y = 10, size = 6, 
                        label = "Keine Titel zur Ansicht ausgewählt.") +
               theme_void())
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
    
    colors <- c("Anomalie" = "red", "Vorjahre" = "#838383", "Aktuell" = "#197084")
    
    p <- ggplot(df_scatter, aes(x = value, y = Gesamttitel)) + 
      geom_point(data = selected(), aes(x = value, y = Gesamttitel, colour = "Anomalie"), fill = "white", shape = 21, size = 5, stroke = 1.0) +
      geom_point(aes(colour = factor(ifelse(df_scatter$year == as.character(last_year), "Aktuell", "Vorjahre")), group = year), size = 4, alpha = ifelse(df_scatter$year == as.character(last_year), 1, 0.2)) + 
      labs(title = scatterTitle(),
           subtitle = "Einzelplan 14",
           caption = "Daten des Landes Schleswig-Holstein") +
      theme(plot.title = element_text(family = plot_font_family, size = 20, color = "gray16"),
            plot.subtitle = element_text(family = plot_font_family, size = 18),
            panel.background = element_rect(fill = "grey98"),
            axis.text.x = element_text(family = plot_font_family, size = 16),
            axis.text.y = element_blank(),
            axis.title.x = element_blank(),
            axis.ticks.x = element_line(color = "grey40"),
            axis.ticks.y = element_line(color = "grey40"),
            axis.title.y = element_blank(),
            plot.caption = element_text(family = plot_font_family, color = "gray12", size = 14)) +
      scale_color_manual(values = colors) +
      guides(colour = guide_legend(title = "Legende", override.aes = list(shape = c(16, 16, 16), size = c(5, 4, 4))))
    
    selected_range <- sort(as.numeric(input$pickWertebereich))
    p <- p + xlim(selected_range[1], selected_range[2])
    p
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
                                               paste(fa("user"), "User"),
                                               if_else(startsWith(Ursprung, "AI"),
                                                       paste(fa("microchip"), "KI-system"),
                                                       Ursprung)))
  # save the tibble as reactive value
  rv <- reactiveValues(x = df_new)
  
  ## render table
  output$mydata <- renderDT({
    data = isolate(rv$x) # isolate inhibits dependency between data and render-function, datatable is only updated, not rendered, when data changes
    datatable(data,
              escape = FALSE,
              extensions = "Buttons", 
              editable = list(target = "cell", disable = list(columns = c(0, 1, 2))),
              class = 'compact stripe', 
              caption = "Hier erscheinen Ihre ausgewählten Datenpunke. Ergänzen Sie im Kommentar, warum Sie den Datenpunkt als Ausreißer werten. Sie können Einträge auch löschen.",
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
                  text = 'Download Ausreißer-Liste')),
                ordering = FALSE,
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
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
      selected_points <- rv$x %>% filter(Art == "Ist") %>% select(Gesamttitel=Titel, year=Jahr, value=Wert)
    } else if (curr_art() == "df_soll"){
      selected_points <- rv$x %>% filter(Art == "Soll") %>% select(Gesamttitel=Titel, year=Jahr, value=Wert)
    } else{
      selected_points <- rv$x %>% filter(Art == "Diff") %>% select(Gesamttitel=Titel, year=Jahr, value=Wert)
    }
    if (!is.null(input$pickTitel)){
      selected_points <- filter(selected_points, Gesamttitel %in% input$pickTitel)
    }
    return(selected_points)
  })
  
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
      rv$x <- rv$x %>% bind_rows(tibble(Ursprung = "User", Titel = pointsnear$Gesamttitel, Jahr = pointsnear$year, Wert = pointsnear$value, Art = art, Kommentar = ""))
      rv$x <- rv$x %>% mutate(Ursprung = if_else(startsWith(Ursprung, "User"),
                                                 paste(fa("user"), "Nutzer"),
                                                 if_else(startsWith(Ursprung, "AI"),
                                                 paste(fa("microchip"), "AI system"),
                                                 Ursprung)
                              ))
    }
    session$clientData$output_plot1_width
  })
  
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
