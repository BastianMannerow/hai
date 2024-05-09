


# the final generation of the detailed plot
generateDetailPlot <- function(df_scatter, df_zweck, df_ist, df_soll, rv, selectedTitle,
                               plot_font_family, normal_text_font_size, mini_headline_font_size, output) {
  
  plot_data <- filterDataForDetailedView(df_zweck, df_ist, df_soll, rv, selectedTitle)
  
  tryCatch({
    # Zeitreihenplot
    timeSeriesPlot <- generateTimeSeriesPlot(plot_data$soll_values, plot_data$ist_values, plot_data$soll_anomalies, plot_data$ist_anomalies, plot_font_family, normal_text_font_size)
    
    # Differenzplot
    differencePlot <- generateDifferencePlot(plot_data$combined_df, plot_data$diff_anomalies, plot_font_family, normal_text_font_size)
    
    # Plots kombinieren
    title_with_breaks <- insert_breaks_every_n_chars(paste(plot_data$title, " - ", plot_data$purpose))
    combinedPlot <- timeSeriesPlot / differencePlot + plot_layout(guides = "collect") + plot_annotation(title = title_with_breaks) +
      theme(plot.margin = margin(1, 1, 1, 1), plot.title = element_text(size = mini_headline_font_size, family = plot_font_family))
    
    output$combinedPlot <- renderPlot({ combinedPlot })
  }, error = function(e) {
    cat("Es gab einen Fehler bei der Erstellung der Detailansicht: ", e$message, "\n")
  })
}


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
  
  # Remove NA
  combined_df <- na.omit(combined_df)
  
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

# Filters all the neccassary dataframes
filterDataForDetailedView <- function(df_zweck, df_ist, df_soll, rv, selectedTitle) {
  req(selectedTitle)
  title <- selectedTitle()
  
  if (is.null(title) || title == "") {
    if (nrow(df_ist) > 0 && "Gesamttitel" %in% colnames(df_ist)) {
      title <- df_ist$Gesamttitel[1]
    } else {
      stop("Title is null or empty, and df_ist does not contain 'Gesamttitel' or is empty.")
    }
  }
  
  purpose <- df_zweck %>%
    filter(Gesamttitel == title) %>%
    pull(Zweckbestimmung)
  
  # Get Data and removes Anomalie in year, which ensures a clean x axis.
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
  
  # Return all necessary variables for plot generation
  list(title = title, purpose = purpose, ist_values = ist_values, soll_values = soll_values, combined_df = combined_df,
       soll_anomalies = soll_anomalies, ist_anomalies = ist_anomalies, diff_anomalies = diff_anomalies)
}
