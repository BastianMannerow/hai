

renderMainPlot <- function(df_scatter, last_year, plot_font_family, headline_font_size, normal_text_font_size, scatterTitle, selected, refreshMainPlot){
  colors <- c("AI - Anomalie" = "red", "Vorjahre" = "#838383", "Aktuell" = "#197084", "User - Anomalie" = "red", "User - Anomalie - Aktuell" = "#197084")
  
  mainPlot <- ggplot(df_scatter, aes(x = value, y = Gesamttitel)) +
    # normal cases
    geom_point(aes(
      colour = factor(ifelse(df_scatter$year == as.character(last_year), "Aktuell", "Vorjahre")), group = year),
      size = 4, 
      alpha = ifelse(df_scatter$year == as.character(last_year), 1, 0.2))
  
  if (nrow(selected) > 0) {
    mainPlot <- mainPlot +
      # ai anomaly in the background, to ensure it doesnt cover nearby points
      geom_point(data = selected, aes(x = value, y = Gesamttitel, 
                                      colour = "AI - Anomalie"), 
                 fill = "white", 
                 shape = 21, 
                 size = 5, 
                 stroke = 1.0) + # AI selected
      # complex conditions for colouring the user anomalies based on its year
      geom_point(data = selected, aes(x = value, y = Gesamttitel, 
                                      colour = ifelse(selected$Ursprung == "TRUE" & selected$year == as.character(last_year), "User - Anomalie - Aktuell", "User - Anomalie")), fill = "red", shape = 21, size = 5, 
                 stroke = ifelse(selected$Ursprung == "TRUE" & selected$year == as.character(last_year), 1.0, 0.1), 
                 alpha = ifelse(selected$Ursprung == "TRUE", 1, 0), size = 4)
  }
  
  mainPlot <- mainPlot +
    labs(title = scatterTitle(),
         subtitle = "Einzelplan 14",
         caption = ifelse(refreshMainPlot(), "Daten des Landes Schleswig-Holstein (aktuelles Jahr)", "Daten des Landes Schleswig-Holstein (aktuelles Jahr) ")) + # useless but forces a refresh
    theme(plot.title = element_text(family = plot_font_family, size = headline_font_size, color = "gray16"),
          plot.subtitle = element_text(family = plot_font_family, size = normal_text_font_size),
          panel.background = element_rect(fill = "grey98"),
          axis.text.x = element_text(family = plot_font_family, size = normal_text_font_size),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_line(color = "grey40"),
          axis.ticks.y = element_line(color = "grey40"),
          axis.title.y = element_blank(),
          plot.caption = element_text(family = plot_font_family, color = "gray12", size = normal_text_font_size)) +
    scale_color_manual(values = colors) +
    guides(colour = guide_legend(title = "Legende"))
  
  return(mainPlot)
}


generateMainPlot <- function(scatterData, input, session, getPlotHeight, selected, scatterTitle, plot_font_family, headline_font_size, normal_text_font_size, refreshMainPlot) {
  renderPlot({
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
    
    # handle if anomalies are unselected by clearing selected
    if (input$pickAnomalyFilter == "titlesWithNoAnomaly") {
      anomalies_by_filter <- data.frame(value = numeric(0), Gesamttitel = character(0), stringsAsFactors = FALSE)
    } else {
      anomalies_by_filter <- selected()
    }
    # draw the final plot
    mainPlot = renderMainPlot(df_scatter, last_year, plot_font_family, headline_font_size, normal_text_font_size, scatterTitle, anomalies_by_filter, refreshMainPlot)
    
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
}

## visualize hover info and tooltip, copied from: https://gitlab.com/-/snippets/16220
# new information for cursor position was added to plot_hover in 2018: https://github.com/rstudio/shiny/pull/2183
generateMainPlotHoverInfo <- function(input, scatterData, nearPoints) {
  renderUI({
    hover <- input$plot_hover 
    if (nrow(scatterData()) == 0) return(NULL)
    point <- nearPoints(scatterData(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    h_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    v_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    if (h_pct < 0.8 && v_pct < 0.8){
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", hover$coords_css$x + 2, "px; top:", hover$coords_css$y +2, "px;")
    } else if (h_pct >= 0.8 && v_pct < 0.8){
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", hover$coords_css$x - 300, "px; top:", hover$coords_css$y +2, "px;")
    } else if (h_pct < 0.8 && v_pct >= 0.8){
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", hover$coords_css$x + 2, "px; top:", hover$coords_css$y - 150, "px;")
    } else {
      style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                      "left:", hover$coords_css$x - 300, "px; top:", hover$coords_css$y - 150, "px;")
    }
    
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Titel: </b>", point$Gesamttitel, "<br/>",
                    "<b> Jahr: </b>", point$year, "<br/>",
                    "<b> Wert: </b>", point$value, "<br/>")))
    )
  })
}
# Cursor changes to pointer when Hover Info is displayed
changeMainPlotHoverType <- function(input, scatterData, nearPoints) {
  renderUI({
    hover <- input$plot_hover 
    if (nrow(scatterData()) == 0) return(NULL)
    point <- nearPoints(scatterData(), hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) {
      css_string <- "
        #plot1 {  
        cursor: crosshair;}"
    } else {
      css_string <- "
        #plot1 {  
        cursor: pointer;}"
    }
    tags$style(HTML(css_string))
  })
}

# toggles the view based on the chosen chapter
updateTitleChoices <- function(input, session, df_zweck, current_titel) {
  observeEvent(input$pickKapitel, {
    updatePickerInput(
      session = session,
      inputId = "pickTitel",
      choices = filter(df_zweck, df_zweck$Kapitel %in% input$pickKapitel)["Gesamttitel"],
      selected = current_titel()
    )
  }, ignoreInit = TRUE)
}