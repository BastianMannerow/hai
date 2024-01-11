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

### load Roboto font and change scale view
font_add_google("Roboto Condensed", family = "Roboto")
showtext_auto()
options(scipen = 999)

### get data
df_soll <- read_csv("./Data/hh_sh_ep14_soll.csv", col_types = cols(Gesamttitel = col_character()))
df_soll <- slice(df_soll, 1:20) # subset (first 20 rows), can be uncommented later
df_scatter <- melt(df_soll, id = "Gesamttitel", variable.name = "year") # melt data for scatter plot

### set up server
shinyServer(function(input, output, session) {
  
  ## visualize data
  output$plot1 <- renderPlot({
    ggplot(df_scatter, aes(x = value, y = Gesamttitel)) + 
      # outlines the selected points with a specific colour (red)
      geom_point(data = selected(), aes(x = value, y = Gesamttitel), colour = "red", fill = "white", shape = 21, size = 5, stroke = 1.0) +
      geom_point(aes(colour = Gesamttitel), size = 4) +
      # to color the selected points red
      # geom_point(data=selected(), colour= "red", size = 4)+
      labs(title = "Verteilung der Soll-Werte 2012 bis 2021 (in Euro)",
           subtitle = "Einzelplan 14",
           caption = "Daten des Landes Schleswig-Holstein") +
      theme(plot.title = element_text(family = "Roboto", size = 20, color = "gray16"),
            plot.subtitle = element_text(family = "Roboto", size = 18),
            panel.background = element_rect(fill = "grey96"),
            axis.text.x = element_text(size = 16),
            axis.text.y = element_text(size = 16),
            axis.title.x = element_blank(),
            axis.ticks.x = element_line(color = "grey40",),
            axis.ticks.y = element_line(color = "grey40"),
            axis.title.y = element_blank(),
            legend.position = "none",
            plot.caption = element_text(family = "Roboto",color = "gray12", size = 14))+
      scale_color_manual(values=c("#6B4BC7", "#197084", "#DF9E1F", "#6B4BC7", "#197084", "#DF9E1F", "#6B4BC7", "#197084",
                                  "#DF9E1F", "#6B4BC7", "#197084", "#DF9E1F", "#6B4BC7", "#197084", "#DF9E1F", "#6B4BC7",
                                  "#197084", "#DF9E1F", "#6B4BC7", "#197084"))
  })
  
    
  ## visualize hover info and tooltip, copied from: https://gitlab.com/-/snippets/16220
  # new information for cursor position was added to plot_hover in 2018: https://github.com/rstudio/shiny/pull/2183
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(df_scatter, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
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
  
  # initialize an empty tibble with column names used in table
  df_new <- tibble(Titel = character(), Jahr = character(), Wert = numeric(), Kommentar = character())
  # save the tibble as reactive value
  rv <- reactiveValues(x = df_new)

  ## render table
  output$mydata <- renderDT({
    data = isolate(rv$x) # isolate inhibits dependency between data and render-function, datatable is only updated, not rendered, when data changes
    datatable(data, extensions = "Buttons", 
              editable = list(target = "cell", disable = list(columns = c(0, 1, 2))),
              class = 'compact stripe', 
        caption = "Hier erscheinen Ihre ausgewählten Datenpunke, gern können Sie diese kommentieren.",
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
        ordering = FALSE)
        )
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
  
  ## color the data from the table
  selected <- reactive({
    ## get data from rv$x to the same structure like in df_ist_ausz
    selected_points <- rv$x %>% select(Gesamttitel=Titel, year=Jahr, value=Wert)
    return(selected_points)
  })
  
  ## add data to table from clicked points in plot
  observeEvent(input$clicked, {
    pointsnear <- nearPoints(df_scatter, input$clicked)
    wert <- pointsnear$value
    jahr <- pointsnear$year
    text <- pointsnear$Gesamttitel

    rv$x <- rv$x %>% bind_rows(tibble(Titel = text, Jahr = jahr, Wert = wert, Kommentar = ""))
  })
  
  ## delete rows in data table
  observeEvent(input[["remove"]], {
    req(input[["mydata_rows_selected"]])
    indices <- input[["mydata_rows_selected"]]
    rv$x <- rv$x %>% filter(!row_number() %in% indices)
  })
  
  ## save comments in datatable
  observeEvent(input$mydata_cell_edit, {
    info = input$mydata_cell_edit
    i = info$row
    j = info$col + 1 # column index offset of 1 because ID column (rownames) is hidden
    v = info$value
    rv$x[i,j] = v
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
  })
  
  
})


### open RDS Files
# filename <- file.choose()
# resultData <- readRDS(filename)
