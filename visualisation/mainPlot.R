

renderMainPlot <- function(df_scatter, last_year, plot_font_family, headline_font_size, normal_text_font_size, scatterTitle, selected){
  colors <- c("AI - Anomalie" = "red", "Vorjahre" = "#838383", "Aktuell" = "#197084", "User - Anomalie" = "red")
  
  
  mainPlot <- ggplot(df_scatter, aes(x = value, y = Gesamttitel)) + 
    geom_point(data = selected(), aes(x = value, y = Gesamttitel, colour = "AI - Anomalie"), fill = "white", shape = 21, size = 5, stroke = 1.0) + # AI selected
    geom_point(aes(colour = factor(ifelse(df_scatter$year == as.character(last_year), "Aktuell", "Vorjahre")), group = year), size = 4, alpha = ifelse(df_scatter$year == as.character(last_year), 1, 0.2)) + # regular
    geom_point(data = selected(), aes(x = value, y = Gesamttitel, colour = "User - Anomalie"), alpha = ifelse(selected()$Ursprung == "TRUE", 1, 0), size = 4) + # User Anomalies
    
    labs(title = scatterTitle(),
         subtitle = "Einzelplan 14",
         caption = "Daten des Landes Schleswig-Holstein") +
    theme(plot.title = element_text(family = plot_font_family, size = headline_font_size, color = "gray16"),
          plot.subtitle = element_text(family = plot_font_family, size = normal_text_font_size ),
          panel.background = element_rect(fill = "grey98"),
          axis.text.x = element_text(family = plot_font_family, size = normal_text_font_size ),
          axis.text.y = element_blank(),
          axis.title.x = element_blank(),
          axis.ticks.x = element_line(color = "grey40"),
          axis.ticks.y = element_line(color = "grey40"),
          axis.title.y = element_blank(),
          plot.caption = element_text(family = plot_font_family, color = "gray12", size = normal_text_font_size )) +
    scale_color_manual(values = colors) +
    guides(colour = guide_legend(title = "Legende"))
  
  return(mainPlot)
}

generateMainPlot <- function(scatterData, input, session, getPlotHeight, selected, scatterTitle, plot_font_family, headline_font_size, normal_text_font_size) {
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
    
    mainPlot = renderMainPlot(df_scatter, last_year, plot_font_family, headline_font_size, normal_text_font_size, scatterTitle, selected)
    
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