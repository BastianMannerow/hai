

updateTimeSlider <- function(session, scatterDataframe) {
  observe({
    years <- names(scatterDataframe())[-1] # Annahme: Gesamttitel ist die erste Spalte
    years <- years[!grepl("Anomalie", years)] # Filter out 'Anomalie'
    years <- as.character(years)
    
    updateSliderTextInput(session, "pickZeitraum",
                          choices = years,
                          selected = c(years[1], tail(years, 1)))
    
    # Die Funktion gibt die aktuelle Breite des Plots zurück
    session$clientData$output_plot1_width
  })
}

updateValueSlider <- function(session, scatterDataframe) {
  observe({
    df <- scatterDataframe()
    df <- df[,-1]
    # Checks the interval of the whole dataframe
    if(all(is.na(df))) {
      minWert <- 0
      maxWert <- 1
    } else {
      minWert <- min(df, na.rm = TRUE)
      maxWert <- max(df, na.rm = TRUE)
    }
    
    #if(!is.finite(minWert)) minWert <- 0
    #if(!is.finite(maxWert)) maxWert <- 1
    
    # smart logarithmic rounding, to have a reasonable interval between slider inputs
    roundDownToNearest <- function(x) {
      exp <- ifelse(x == 0, 0, floor(log10(abs(x))))
      base <- 10^exp
      if(x > 0) {
        return(floor(x/base)*base)
      } else {
        return(-ceiling(abs(x)/base)*base)
      }
    }
    
    # Rounding up to an appropriate value
    roundUpToNearest <- function(x) {
      if(x == 0) return(0)
      exp <- floor(log10(abs(x)))
      base <- 10^exp
      if(x > 0) {
        upper <- ceiling(x/base)*base
        # examines if a multiple value is closer than base
        if (upper - x <= base / 2) {
          return(upper)
        } else {
          # rounding based on difference (important for a reasonable x axis)
          finerIncrement <- 10^(exp-1)
          finerUpper <- ceiling(x/finerIncrement)*finerIncrement
          if(finerUpper > x) {
            return(finerUpper)
          } else {
            return(upper) # shouldn't be used, just in case for bug fixing
          }
        }
      } else {
        # negative case
        lower <- -floor(abs(x)/base)*base
        return(lower)
      }
    }
    
    minWertRounded <- roundDownToNearest(minWert)
    maxWertRounded <- roundUpToNearest(maxWert)
    
    logMin <- log10(max(minWertRounded, 1))
    logMax <- log10(maxWertRounded)
    logRange <- seq(logMin, logMax, length.out = 10) # Number of slider choices
    choices <- 10^logRange
    choicesRounded <- unique(sapply(choices, roundUpToNearest))
    
    # final rounding to receive int from double
    choicesRounded <- round(choicesRounded)
    
    if (choicesRounded[1] > minWert) {
      choicesRounded[1] <- minWertRounded
    }
    if (choicesRounded[length(choicesRounded)] != maxWertRounded) {
      choicesRounded[length(choicesRounded)] <- maxWertRounded
    }
    
    values <- c(minWertRounded, maxWertRounded)
    
    updateSliderTextInput(session, "pickWertebereich",
                          choices = choicesRounded,
                          selected = values)
    session$clientData$output_plot1_width
  })
}

# Logging
if (interactive()) {
  print("dynamicSlider.R geladen")
}