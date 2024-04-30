
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
  as.numeric(numeric_part) * 3.5
}

# calculates plot height
calculatePlotHeight  <- function(input, normal_text_font_size, scatterData, session) {
  button_height <- getButtonHeight(normal_text_font_size)
  num_buttons <- numberOfButtons(scatterData)
  base_height <- switch(
    as.character(input$screenSize$height),
    "1200" = button_height * (num_buttons + 2.6) + 12,
    "1080" = button_height * (num_buttons + 2.6) + 12,
    "720" = button_height * (num_buttons + 2.2) + 8,
    button_height * (num_buttons + 1.8) + 12  # Default case for Apple or other sizes
  )
  base_height
}