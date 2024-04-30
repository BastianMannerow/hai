
# counts the amount of buttons
numberOfButtons <- function(scatterData) {
  df_scatter <- scatterData() 
  titelListe <- unique(df_scatter$Gesamttitel)
  length(titelListe)
}

# button width
getButtonWidth <- function(session) {
  session$clientData$output_buttonsPanel_width
}

# button height
calculateButtonHeight <- function(normal_text_font_size) {
  numeric_part <- substr(normal_text_font_size, 1, nchar(normal_text_font_size) - 2)
  calc_temp <- as.numeric(numeric_part) * 3.5
  height <- calc_temp 
  return(height)
}

# calculates plot height
calculatePlotHeight  <- function(input, scatterData, session, getbutton_height) {
  default_height <- 1080
  
  screen_height <- if(!is.null(input$screenSize$height) && length(input$screenSize$height) > 0) {
    input$screenSize$height
  } else {
    return(default_height)
  }
  
  
  if(input$screenSize$height == 1200) {
    height <- getbutton_height() * (numberOfButtons(scatterData) + 2.6) + 12
  }
  else if(input$screenSize$height == 1080) {
    height <- getbutton_height() * (numberOfButtons(scatterData) + 2.6) + 12
  }
  else if(input$screenSize$height == 720) {
    height <- getbutton_height() * (numberOfButtons(scatterData) + 2.2) + 8
  }
  else{ # Apple
    height <- getbutton_height() * (numberOfButtons(scatterData) + 1.8) + 12
  }
  return(height)
}