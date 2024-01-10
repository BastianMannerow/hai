#### transform SH data to format used by outlier app ###
## load libraries
library(readxl)
library(tidyverse)

## import data
df_start <- read_excel("HAI_Haushalt_SH_EP14.xlsx", sheet = 2, na = "NaN")
df_zweck <- read_excel("HAI_Haushalt_SH_EP14.xlsx", sheet = 3)
df_kapitel <- read_excel("HAI_Haushalt_SH_EP14.xlsx", sheet = 4)

# reverse rows of dataframe (for reversing the years)
df_rev <- apply(df_start, 2, rev)
df_rev <- as.data.frame(df_rev)

# rearrange rows to group "Gesamttitel" together in ascending order
df_arrange <- arrange(df_rev, Gesamttitel)

titel <- unique(df_arrange$Gesamttitel)
years <- unique(df_arrange$Jahr)

# transpose df
df_t <- t(df_arrange)
df_t <- as.data.frame(df_t)

## form dataframe for Ist values

df_ist <- data.frame(matrix(ncol=10,nrow=0))
for (i in 1:length(titel)){
  start <- (i-1)*length(years)+1
  end <- i*length(years)
  vec <- df_t["Ist",start:end]
  colnames(vec) <- colnames(df_ist)
  df_ist <- rbind(df_ist, vec)
}
colnames(df_ist) <- years
df_ist <- df_ist %>% mutate(Gesamttitel = titel, .before ="2012")

## form dataframe for Soll values

df_soll <- data.frame(matrix(ncol=10,nrow=0))
for (i in 1:length(titel)){
  start <- (i-1)*length(years)+1
  end <- i*length(years)
  vec <- df_t["Soll",start:end]
  colnames(vec) <- colnames(df_soll)
  df_soll <- rbind(df_soll, vec)
}
colnames(df_soll) <- years
df_soll <- df_soll %>% mutate(Gesamttitel = titel, .before ="2012")

## form dataframe for difference (Soll-Ist) values

df_diff <- data.frame(matrix(ncol=10,nrow=0))
for (i in 1:length(titel)){
  start <- (i-1)*length(years)+1
  end <- i*length(years)
  vec <- df_t["Soll-Ist",start:end]
  colnames(vec) <- colnames(df_diff)
  df_diff <- rbind(df_diff, vec)
}
colnames(df_diff) <- years
df_diff <- df_diff %>% mutate(Gesamttitel = titel, .before ="2012")

## export all dataframes to csv
write.csv(df_ist, "./Data/hh_sh_ep14_ist.csv", row.names = FALSE)
write.csv(df_soll, "./Data/hh_sh_ep14_soll.csv", row.names = FALSE)
write.csv(df_diff, "./Data/hh_sh_ep14_diff.csv", row.names = FALSE)
write.csv(df_zweck, "./Data/hh_sh_ep14_zweck.csv", row.names = FALSE)
write.csv(df_kapitel, "./Data/hh_sh_ep14_kapitel.csv", row.names = FALSE)
