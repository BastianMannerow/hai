
# Counts the missing datapoints
pointsOutsideRange <- function(pickedCategoryDataframe, selected_title, scatterData, minYear, maxYear) {
  df_scatter <- alternative_scatterData(pickedCategoryDataframe, selected_title, minYear, maxYear)
  filtered_out_count <- nrow(df_scatter) - nrow(scatterData())
  return(filtered_out_count)
}

# Helps for counting the data
alternative_scatterData <- function(pickedCategoryDataframe, selected_title, minYear, maxYear) {
  numeric_cols <- names(pickedCategoryDataframe())[!grepl("Anomalie", names(pickedCategoryDataframe())) & sapply(pickedCategoryDataframe(), is.numeric)]
  df_scatter <- reshape2::melt(pickedCategoryDataframe(), id.vars = "Gesamttitel", measure.vars = numeric_cols)
  colnames(df_scatter)[which(names(df_scatter) == "variable")] <- "year"
  df_scatter$year <- as.numeric(as.character(df_scatter$year))
  
  df_scatter <- dplyr::filter(df_scatter, Gesamttitel %in% selected_title)
  df_scatter <- tidyr::drop_na(df_scatter, value)  # Remove NA
  df_scatter <- dplyr::filter(df_scatter, Gesamttitel %in% selected_title & year >= minYear() & year <= maxYear()) # remove year
  
  return(df_scatter)
}

if (interactive()) {
  print("pointsNotVisibleWarning geladen")
}

# the entry point for the warning
generateOutOfRangeMessage <- function(pickedCategoryDataframe, input, scatterData, minYear, maxYear) {
  renderUI({
    if (pointsOutsideRange(pickedCategoryDataframe, input$pickTitel, scatterData, minYear, maxYear) > 0) {
      span(style = "color: red;", paste(pointsOutsideRange(pickedCategoryDataframe, input$pickTitel, scatterData, minYear, maxYear), "Datenpunkte liegen außerhalb des angezeigten Bereichs."))
    } else {
      return(NULL)
    }
  })
}