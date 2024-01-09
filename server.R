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
## Auszahlungen Part 1
#df_ist_ausz <- read_xlsx("/Users/adhungel/Documents/Projekte/HAI/HL_HAI/126001_Ist.xlsx")
#df_ist_ausz <- read_xlsx("/Users/leawatermann/Desktop/Uni/Hiwi/R _Aufgabe/hai/126001_Ist.xlsx")
df_ist_auszX <- read_excel("Testdata.xlsx")
df_ist_auszX <- df_ist_auszX %>%
  ## subset "Auszahlungen" (first 20 rows)
  slice(97:137)
## Delete rows with only zeros
df_ist_ausz <- df_ist_auszX[!apply(df_ist_auszX[, -1], 1, function(row) all(row == 0)), ] %>%
  ## create subsets to reduce complexity of the plot
  slice(1:10) %>%
  ## Melt data for jitter plot
  melt(.,  id.vars = "Text", variable.name = 'series')

df_ist_ausz2 <- df_ist_auszX[!apply(df_ist_auszX[, -1], 1, function(row) all(row == 0)), ] %>%
  ## create subsets to reduce complexity of the plot
  slice(11:20) %>%
  ## Melt data for jitter plot
  melt(.,  id.vars = "Text", variable.name = 'series')

df_ist_ausz3 <- df_ist_auszX[!apply(df_ist_auszX[, -1], 1, function(row) all(row == 0)), ] %>%
  ## create subsets to reduce complexity of the plot
  slice(1:1) %>%
  ## Melt data for jitter plot
  melt(.,  id.vars = "Text", variable.name = 'series')

df_ist_ausz4 <- df_ist_auszX[!apply(df_ist_auszX[, -1], 1, function(row) all(row == 0)), ] %>%
  ## create subsets to reduce complexity of the plot
  slice(4:4) %>%
  ## Melt data for jitter plot
  melt(.,  id.vars = "Text", variable.name = 'series')

df_ist_ausz5 <- df_ist_auszX[!apply(df_ist_auszX[, -1], 1, function(row) all(row == 0)), ] %>%
  ## create subsets to reduce complexity of the plot
  slice(12:12) %>%
  ## Melt data for jitter plot
  melt(.,  id.vars = "Text", variable.name = 'series')

df_ist_ausz6 <- df_ist_auszX[!apply(df_ist_auszX[, -1], 1, function(row) all(row == 0)), ] %>%
  ## Melt data for jitter plot
  melt(.,  id.vars = "Text", variable.name = 'series')




### set up server
shinyServer(function(input, output, session) {
  
  ## visualize data
  output$plot1 <- renderPlot({
    ggplot(df_ist_ausz, aes(value, Text)) + 
      # Zeichne zuerst einen größeren Punkt mit weißer Füllung und rotem Rand
      geom_point(data = selected(), aes(value, Text), colour = "red", fill = "white", shape = 21, size = 5, stroke = 1.0) +
      # Zeichne dann die ursprünglichen Punkte darüber
      geom_point(aes(colour = Text), size = 4) +
      # to color the selected points red
      # geom_point(data=selected(), colour= "red", size = 4)+
      labs(title = "Verteilung der Ist-Auszahlungen 2010 bis 2021 (in Euro)",
           subtitle = "Produkt 126001 - Feuerwehr",
           caption = "Daten der Hansestadt Lübeck") +
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
  
  
  output$plot2 <- renderPlot({
    ggplot(df_ist_ausz2, aes(value, Text)) + 
      geom_jitter(aes(colour = Text), alpha = 0.8, height = 0, size = 4) +
      # to color the selected points red
      geom_point(data=selected(), colour= "red", size = 4)+
      labs(title = "Verteilung der Ist-Auszahlungen 2010 bis 2021 (in Euro)",
           subtitle = "Produkt 126001 - Feuerwehr",
           caption = "Daten der Hansestadt Lübeck") +
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
  
  
  
plot3 <- ggplot(df_ist_ausz3, aes(value, Text)) + 
      geom_point(aes(colour = Text), alpha = 0.8, height = 0, size = 4) +
      # to color the selected points red
      #geom_point(data=selected(), colour= "red", size = 4)+
      labs(title = "Verteilung der Ist-Auszahlungen 2010 bis 2021 (in Euro)",
           subtitle = "Produkt 126001 - Feuerwehr",
           caption = "") +
      scale_y_discrete(labels = label_wrap(35)) +
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

  
  
  
  
plot4 <- ggplot(df_ist_ausz4, aes(value, Text)) + 
      geom_point(aes(colour = Text), alpha = 0.8, height = 0, size = 4) +
      # to color the selected points red
      #geom_point(data=selected(), colour= "red", size = 4)+
      labs(title = "",
           subtitle = "",
           caption = "") +
      scale_y_discrete(labels = label_wrap(35)) +
      scale_x_continuous(breaks = c(0, 250, 500, 750, 1000), limits = c(0, 1000))+
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

  
  
plot5 <- ggplot(df_ist_ausz5, aes(value, Text)) + 
      geom_point(aes(colour = Text), alpha = 0.8, height = 0, size = 4) +
      # to color the selected points red
      #geom_point(data=selected(), colour= "red", size = 1)+
      labs(title = "",
           subtitle = "",
           caption = "Daten der Hansestadt Lübeck") +
      scale_y_discrete(labels = label_wrap(35)) +
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

  

## Solution to display the different plots in one straight among each other
## Source: https://stackoverflow.com/questions/41231876/aligning-setting-width-of-margin-figure-region-in-ggplot2
 
gl <- lapply(list(plot3, plot4, plot5), ggplotGrob)  
wd <- do.call(unit.pmax, lapply(gl, "[[", 'widths'))
gl <- lapply(gl, function(x) {
  x[['widths']] = wd
  x})

output$plot3 <- renderPlot({grid.newpage(); grid.draw(gl[[1]])})
output$plot4 <- renderPlot({grid.newpage(); grid.draw(gl[[2]])})
output$plot5 <- renderPlot({grid.newpage(); grid.draw(gl[[3]])})

 
  

output$plot6 <- renderPlot({
  ggplot(df_ist_ausz6, aes(value, Text)) + 
    geom_point(aes(colour = Text), alpha = 0.8, height = 0, size = 4) +
    # to color the selected points red
    geom_point(data=selected(), colour= "red", size = 4)+
    labs(title = "Verteilung der Ist-Auszahlungen 2010 bis 2021 (in Euro)",
         subtitle = "Produkt 126001 - Feuerwehr",
         caption = "Daten der Hansestadt Lübeck") +
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
                                         "#197084", "#DF9E1F", "#6B4BC7", "#197084",
                                         "#6B4BC7", "#197084", "#DF9E1F", "#6B4BC7", "#197084", "#DF9E1F", "#6B4BC7", "#197084",
                                         "#DF9E1F", "#6B4BC7", "#197084", "#DF9E1F", "#6B4BC7", "#197084", "#DF9E1F", "#6B4BC7",
                                         "#197084", "#DF9E1F", "#6B4BC7", "#197084", "#6B4BC7")) 
})





  
  
      
  ## visualize hover info and tooltip, copied from: https://gitlab.com/-/snippets/16220
  # new information for cursor position was added to plot_hover in 2018: https://github.com/rstudio/shiny/pull/2183
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(df_ist_ausz, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
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
      p(HTML(paste0("<b> Titel: </b>", point$Text, "<br/>",
                    "<b> Jahr: </b>", point$series, "<br/>",
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
    selected_points <- rv$x %>% select(Text=Titel, series=Jahr, value=Wert)
    return(selected_points)
  })
  
  ## add data to table from clicked points in plot
  observeEvent(input$clicked, {
    pointsnear <- nearPoints(df_ist_ausz, input$clicked)
    wert <- pointsnear$value
    jahr <- pointsnear$series
    text <- pointsnear$Text

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
