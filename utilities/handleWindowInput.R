observe_external <- function(session, lastDimensions, refreshMainPlot) {
  observe({
    width <- session$clientData$output_plot1_width
    height <- session$clientData$output_plot1_height
    
    if (!is.null(width) && !is.null(height)) {
      if (width != lastDimensions$width || height != lastDimensions$height) {
        lastDimensions$width <- width
        lastDimensions$height <- height
        
        refreshMainPlot(!refreshMainPlot())
      }
    }
  })
}