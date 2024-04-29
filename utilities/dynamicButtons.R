

generateDynamicButtons <- function(df_scatter, df_zweck, selectedTitle, normal_text_font_size, plot_font_family, getbutton_height, getbutton_width) {
  titelListe <- factor(df_scatter$Gesamttitel)
  sortedTitelListe <- rev(levels(titelListe))
  
  buttons <- lapply(sortedTitelListe, function(titel) {
    btn_id <- paste0("button_", gsub(" ", "_", titel))
    
    selected <- !is.null(selectedTitle()) && !is.na(selectedTitle()) && titel == selectedTitle()
    backgroundColor <- if(selected) "#841919" else "#197084"
    
    purpose <- df_zweck %>%
      filter(Gesamttitel == titel) %>%
      pull(Zweckbestimmung)
    
    tooltipText <- if (length(purpose) > 0) purpose else "Kein Zweck definiert"
    
    actionButton(
      inputId = btn_id,
      label = titel,
      title = tooltipText,
      class = "custom-button",
      style = paste0("font-size: ", normal_text_font_size, ";background-color: ", backgroundColor, "; color: white; height: ", getbutton_height(), "px; width: ", getbutton_width(), "px; font-family: '", plot_font_family, "';")
    )
  })
  
  do.call(tagList, buttons)
}

# Logging
if (interactive()) {
  print("dynamicButtons.R geladen")
}