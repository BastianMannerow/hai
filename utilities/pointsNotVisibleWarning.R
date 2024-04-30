
# Counts the missing datapoints
pointsOutsideRange <- function(reac_data, selected_title, scatterData) {
  df_scatter <- alternative_scatterData(reac_data, selected_title)
  filtered_out_count <- nrow(df_scatter) - nrow(scatterData())
  return(filtered_out_count)
}

# Helps for counting the data
alternative_scatterData <- function(reac_data, selected_title) {
  numeric_cols <- names(reac_data())[!grepl("Anomalie", names(reac_data())) & sapply(reac_data(), is.numeric)]
  df_scatter <- reshape2::melt(reac_data(), id.vars = "Gesamttitel", measure.vars = numeric_cols)
  colnames(df_scatter)[which(names(df_scatter) == "variable")] <- "year"
  df_scatter$year <- as.numeric(as.character(df_scatter$year))
  
  df_scatter <- dplyr::filter(df_scatter, Gesamttitel %in% selected_title)
  df_scatter <- tidyr::drop_na(df_scatter, value)  # Remove NA
  return(df_scatter)
}

if (interactive()) {
  print("pointsNotVisibleWarning geladen")
}