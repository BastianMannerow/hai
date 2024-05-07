library(readr)
library(dplyr)

importDFIst <- function(){
  df_ist <- read_csv("./Data/hh_sh_ep14_ist.csv", col_types = cols(Gesamttitel = col_character()))
  addingAnomalyColumn(df_ist)
  df_ist <- formatGesamttitelName(df_ist)
  return(df_ist)
}

importDFSoll <- function(){
  df_soll <- read_csv("./Data/hh_sh_ep14_soll.csv", col_types = cols(Gesamttitel = col_character()))
  addingAnomalyColumn(df_soll)
  df_soll <- formatGesamttitelName(df_soll)
  return(df_soll)
}

importDFDiff <- function(){
  df_diff <- read_csv("./Data/hh_sh_ep14_diff.csv", col_types = cols(Gesamttitel = col_character()))
  addingAnomalyColumn(df_diff)
  df_diff <- formatGesamttitelName(df_diff)
  return(df_diff)
}

importDFKapitel <- function(){
  df_kapitel <- read_csv("./Data/hh_sh_ep14_kapitel.csv", col_types = cols(Kapitel = col_character()))
}

importDFZweck <- function(){
  df_zweck <- read.table("./Data/hh_sh_ep14_zweck.csv", sep = ",", header = TRUE, 
                         fileEncoding = "UTF-8", 
                         colClasses = c(Kapitel="character", Gesamttitel="character"))
  df_zweck <- formatGesamttitelName(df_zweck)
}

# Adding the column Anomalies for each year to be able to distinguish between AI and Human anomaly
addingAnomalyColumn <- function(df){
  df <- df %>%
    mutate(across(matches("^[0-9]"), ~., .names = "{.col}_Anomalie")) %>%
    mutate(across(ends_with("Anomalie"), ~ 0))
  return(df)
}

importDFAnomaly <- function() {
  # Laden des Datensatzes
  df_anomaly <- read_csv("./Data/hh_sh_ep14_fakeAI.csv", col_types = "cccdcc")
  df_anomaly <- formatGesamttitelName(df_anomaly)
  # Hinzufügen von Icons basierend auf dem Ursprung der Daten
  df_anomaly <- df_anomaly %>% 
    mutate(Ursprung = if_else(startsWith(Ursprung, "User"),
                              paste(fa("user"), "Nutzer"),
                              if_else(startsWith(Ursprung, "AI"),
                                      paste(fa("microchip"), "KI System"),
                                      Ursprung)))
  return(df_anomaly)
}

# changes the string of Gesamttitel
formatGesamttitelName <- function(df) {
  # fake AI uses Titel as column name
  target_col <- if ("Gesamttitel" %in% names(df)) {
    "Gesamttitel"
  } else if ("Titel" %in% names(df)) {
    "Titel"
  } else {
    stop("Es scheint einen Fehler im Dataframe zu geben.")
  }
  
  df <- df %>%
    mutate(!!target_col := paste(substr(!!sym(target_col), 1, 4),
                                 substr(!!sym(target_col), 5, 7),
                                 substr(!!sym(target_col), 8, nchar(!!sym(target_col))),
                                 sep = " "))
  return(df)
}

