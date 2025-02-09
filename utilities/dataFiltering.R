
# analyses user and ai detected anomalies and filters the dataframe accordingly to the user input
filterForAnomalies <- function(input, df, detectedAnomalies, userPickedAnomalies){
  
  # for initial loading (would crash if not implemented)
  if (is.null(input$pickAnomalyFilter)) {
    return(df)  
  } else {
    
    # Handles user input for Art
    nameTransfer <- switch(input$pickArt,
                          "df_soll" = "Soll",
                          "df_ist" = "Ist",
                          "df_diff" = "Diff")
    
    anomaly_titles <- detectedAnomalies %>% 
      dplyr::filter(Art == nameTransfer) %>%
      dplyr::pull(Titel)
    
    user_anomaly_titles <- userPickedAnomalies %>% 
      dplyr::filter(Art == input$pickArt) %>%
      dplyr::pull(Gesamttitel)
    
    all_titles <- c(anomaly_titles, user_anomaly_titles)
    
    # Filter the data based on user selection
    dataframe <- switch(input$pickAnomalyFilter,
                        "titlesWithAnomaly" = {
                          df %>% dplyr::filter(Gesamttitel %in% all_titles)
                        },
                        "titlesWithNoAnomaly" = {
                          df %>% dplyr::filter(!(Gesamttitel %in% all_titles))
                        },
                        df  # if all titles is selected, return the unfiltered df
    )
    return(dataframe)
  }
}


# dataframe for the sliders Zeitraum and Wertebereich
createSliderDataframe <- function(input, df_ist, df_soll, df_diff) {
  reactive({
    dataframe <- if (input$pickArt == "df_ist") {
      df_ist
    } else if (input$pickArt == "df_soll") {
      df_soll
    } else {
      df_diff
    }
    
    selected_title <- input$pickTitel
    dataframe <- dataframe %>% 
      filter(Gesamttitel %in% selected_title)
    
    return(dataframe)
  })
}

## filter data for scatter plot
createScatterData <- function(input, reac_data, anomalies, detectedAnomalies) {
  reactive({
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
        # searches for the anomaly based on year and title
        anomaly_value <- reac_data()[reac_data()$Gesamttitel == row$Gesamttitel, anomaly_col_name]
        # handles empty entries
        if(length(anomaly_value) > 0 && !is.na(anomaly_value)) {
          return(anomaly_value)
        } else {
          return(NA)
        }
      } else {
        return(NA)
      }
    })
    
    print(anomalies$Data)
    # override persistent anomaly clicks
    df_scatter$anomaly <- mapply(function(title, year) {
      any(anomalies$data$Gesamttitel == title & anomalies$data$Jahr == year)
    }, df_scatter$Gesamttitel, df_scatter$year)
    
    # Filter for anomalies
    df_scatter <- filterForAnomalies(input, df_scatter, detectedAnomalies, anomalies$data)
    
    return(df_scatter)
  })
}

## df to save anomaly points (from the table) for coloring in the plot
createSelectedPoints <- function(input, rv, curr_art) {
  reactive({
    ## get data from rv$x to the same structure like in df and 
    # filter curr_art (Betragsart) and input$pickTitel
    if (curr_art() == "df_ist"){
      selected_points <- rv$x %>% filter(Art == "Ist") %>%
        select(Gesamttitel=Titel, year=Jahr, value=Wert, Ursprung)
    } else if (curr_art() == "df_soll"){
      selected_points <- rv$x %>% filter(Art == "Soll") %>%
        select(Gesamttitel=Titel, year=Jahr, value=Wert, Ursprung)
    } else {
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
}

## Function to generate the title for scatter plot based on user selection
createScatterTitle <- function(input, minYear, maxYear) {
  if (input$pickArt == "df_ist") {
    titel <- paste("Verteilung der Ist-Werte", minYear, "bis", maxYear, "(in Euro)")
  } else if (input$pickArt == "df_soll") {
    titel <- paste("Verteilung der Soll-Werte", minYear, "bis", maxYear, "(in Euro)")
  } else if (input$pickArt == "df_diff") {
    titel <- paste("Verteilung der Differenz 'Soll-Ist' von", minYear, "bis", maxYear, "(in Euro)")
  }
  return(titel)
}
