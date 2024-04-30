
sourceRefreshPlot <- function(session) {
  observe({
    invalidateLater(1000, session)
    session$clientData$output_plot1_width
  })
}

sourceHandleWindowSize <- function(input, session) {
  observe({
    width <- input$windowSize$width
    height <- input$windowSize$height
    session$clientData$output_plot1_width
  })
}