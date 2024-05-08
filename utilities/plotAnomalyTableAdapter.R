## anomaly toggling to distinguish between user and ai
toggleAnomaly <- function(title, year, art, anomalies) {
  # checks if anomaly is selected another time
  existing_anomaly <- anomalies$data$Gesamttitel == title & anomalies$data$Jahr == year & anomalies$data$Art == art
  
  if (any(existing_anomaly)) {
    # delete deselected anomaly
    anomalies$data <- anomalies$data[!existing_anomaly, ]
  } else {
    # adding the new selected anomaly
    new_entry <- data.frame(Gesamttitel = title, Jahr = year, Art = art, Anomalie = TRUE)
    anomalies$data <- rbind(anomalies$data, new_entry)
  }
}

## add data to table from clicked points in plot
setupAnomalyInteractions <- function(input, output, session, rv, curr_art, scatterData, last_click, anomalies) {
  observeEvent(input$clicked, {
    art <- if (curr_art() == "df_ist") {
      "Ist"
    } else if (curr_art() == "df_soll") {
      "Soll"
    } else {
      "Diff"
    }
    
    pointsnear <- nearPoints(scatterData(), input$clicked, threshold = 5, maxpoints = 1)
    if (nrow(pointsnear) > 0) {
      pointsnear$year <- as.character(pointsnear$year)
      toggleAnomaly(pointsnear$Gesamttitel, as.numeric(pointsnear$year), input$pickArt, anomalies)
      
      if (!is.null(last_click()) && identical(last_click(), list(pointsnear$Gesamttitel, pointsnear$year, pointsnear$value, art))) {
        rv$x <- rv$x %>% 
          filter(!(Titel == pointsnear$Gesamttitel & Jahr == pointsnear$year & Wert == pointsnear$value & Art == art))
        last_click(NULL)
        
        showModal(modalDialog(
          title = "Der Punkt wurde von Ihnen abgewählt.",
          "Die Tabelle wurde aktualisiert und der Eintrag entfernt.",
          easyClose = TRUE,
          fade = TRUE,
          footer = NULL,
          size = "s"
        ))
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
        
        showModal(modalDialog(
          title = "Der Punkt wurde von Ihnen als Anomalie markiert.",
          "Er erscheint als neuer Eintrag in der Übersichtstabelle. Klicken Sie erneut auf den Punkt, 
          um die Auswahl rückgängig zu machen oder löschen Sie den Eintrag über die Tabelle.",
          easyClose = TRUE,
          fade = TRUE,
          footer = NULL,
          size = "s"
        ))
      }
    }
    
  })
}