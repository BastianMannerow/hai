library(readr)
library(dplyr)

importDFIst <- function(){
  df_ist <- read_csv("./Data/hh_sh_ep14_ist.csv", col_types = cols(Gesamttitel = col_character()))
  addingAnomalyColumn(df_ist)
  return(df_ist)
}

importDFSoll <- function(){
  df_soll <- read_csv("./Data/hh_sh_ep14_soll.csv", col_types = cols(Gesamttitel = col_character()))
  addingAnomalyColumn(df_soll)
  return(df_soll)
}

importDFDiff <- function(){
  df_diff <- read_csv("./Data/hh_sh_ep14_diff.csv", col_types = cols(Gesamttitel = col_character()))
  addingAnomalyColumn(df_diff)
  return(df_diff)
}

importDFKapitel <- function(){
  df_kapitel <- read_csv("./Data/hh_sh_ep14_kapitel.csv", col_types = cols(Kapitel = col_character()))
}

importDFZweck <- function(){
  df_zweck <- read.table("./Data/hh_sh_ep14_zweck.csv", sep = ",", header = TRUE, 
                         fileEncoding = "UTF-8", 
                         colClasses = c(Kapitel="character", Gesamttitel="character"))
}

# Gesamttitel still needs space in between (Issue from Christiane)
#df_ist <- df_ist %>% mutate(Gesamttitel = paste(substr(Gesamttitel,1,4), substr(Gesamttitel,5,6), substr(Gesamttitel,7,9), sep = " "))
#df_soll <- df_soll %>% mutate(Gesamttitel = paste(substr(Gesamttitel,1,4), substr(Gesamttitel,5,6), substr(Gesamttitel,7,9), sep = " "))
#df_diff <- df_diff %>% mutate(Gesamttitel = paste(substr(Gesamttitel,1,4), substr(Gesamttitel,5,6), substr(Gesamttitel,7,9), sep = " "))
#df_zweck <- df_zweck %>% mutate(Gesamttitel = paste(substr(Gesamttitel,1,4), substr(Gesamttitel,5,6), substr(Gesamttitel,7,9), sep = " "))

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
  
  # Hinzufügen von Icons basierend auf dem Ursprung der Daten
  df_anomaly <- df_anomaly %>% 
    mutate(Ursprung = if_else(startsWith(Ursprung, "User"),
                              paste(fa("user"), "Nutzer"),
                              if_else(startsWith(Ursprung, "AI"),
                                      paste(fa("microchip"), "KI System"),
                                      Ursprung)))
  
  return(df_anomaly)
}