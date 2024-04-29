

generateTimeSeriesPlot <- function(soll_values, ist_values, soll_anomalies, ist_anomalies, plot_font_family, normal_text_font_size){
  soll_values$Anomalie <- ifelse(soll_values$year %in% soll_anomalies$Jahr, "Soll-Anomalie", "Soll-Werte")
  
  timeSeriesPlot <- ggplot(soll_values, aes(x = year, y = value_soll)) +
    geom_bar(aes(fill = Anomalie), stat = "identity", width = 0.7, show.legend = TRUE) +
    geom_line(data = ist_values, aes(x = year, y = value_ist, color = "Ist-Werte", group = 1), size = 2) +
    geom_point(data = ist_anomalies, aes(x = Jahr, y = Wert, color = "Ist-Anomalie"), size = 5) +
    scale_fill_manual(values = c("Soll-Werte" = "#d3d3d3", "Soll-Anomalie" = "#841919"), name = "Soll-Werte") +
    scale_color_manual(values = c("Ist-Werte" = "#197084", "Ist-Anomalie" = "#841919"), name = "Ist-Werte") +
    labs(y = "Absolutwerte") +
    theme_minimal() +
    theme(text = element_text(family = plot_font_family), 
          axis.text.y = element_text(size = normal_text_font_size),
          axis.title.x = element_blank(), 
          axis.text.x = element_blank(),
          legend.text = element_text(size = normal_text_font_size),
          legend.title = element_text(size = normal_text_font_size), 
          axis.title.y = element_text(size = normal_text_font_size),
          axis.ticks.x = element_blank())
}

generateDifferencePlot <- function(combined_df, diff_anomalies, plot_font_family, normal_text_font_size){
  # Illiminate false anomaly entries
  combined_df$Anomalie <- ifelse(combined_df$year %in% diff_anomalies$Jahr, "Anomalie", "Negative Differenz")
  
  detailPlot <- ggplot(combined_df, aes(x = year, y = difference, fill = Anomalie)) +
    geom_bar(stat = "identity", aes(fill = ifelse(difference > 0, "Positive Differenz", Anomalie))) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_fill_manual(values = c("Anomalie" = "#841919", "Positive Differenz" = "#28841980", "Negative Differenz" = "#84191980"), name = "Differenz: Soll - Ist") +
    labs(y = "Zieldifferenz") +
    theme_minimal() +
    theme(text = element_text(family = plot_font_family), 
          axis.text.x = element_text(size = normal_text_font_size),
          axis.text.y = element_text(size = normal_text_font_size),
          legend.text = element_text(size = normal_text_font_size),
          legend.title = element_text(size = normal_text_font_size),
          axis.title.y = element_text(size = normal_text_font_size),
          axis.title.x = element_blank())
}

# To call this function: 
# Handles long titles and purposes in the detailed plot with length of n (Currently not used)
insert_breaks_every_n_chars <- function(s, n = 80) {
  # dynamic characters to divide it
  parts <- c()
  
  while(nchar(s) > n) {
    pos <- n
    while(substring(s, pos, pos) != " " && pos > 1) {
      pos <- pos - 1
    }
    if(pos == 1) {
      pos <- n
    }
    parts <- c(parts, substr(s, 1, pos))
    s <- substr(s, pos + 1, nchar(s))
  }
  
  parts <- c(parts, s)
  paste(parts, collapse = "\n")
}