

generateMainPlot <- function(df_scatter, last_year, plot_font_family, headline_font_size, normal_text_font_size, scatterTitle, selected){
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