titleDetailButtonDesign <- function(titleDetails, selectedTitle){
  renderUI({
    # Access reactive values inside renderUI, which is a reactive context
    current_titles_list <- titleDetails$titlesList
    current_selected_title <- selectedTitle()
    
    # Check if the titles list is empty
    if (length(current_titles_list) == 0) {
      left_color <- "lightgrey"
      right_color <- "lightgrey"
    } else {
      current_index <- match(current_selected_title, current_titles_list)
      left_color <- if (!is.na(current_index) && current_index > 1) "black" else "lightgrey"
      right_color <- if (!is.na(current_index) && current_index < length(current_titles_list)) "black" else "lightgrey"
    }
    
    div(style = "display: flex; justify-content: space-around; align-items: center; margin-top: 20px;",
        actionButton("left_button", label = "", icon = icon("arrow-left"), style = sprintf("color: %s;", left_color)),
        div("Wechseln Sie zwischen bereits ausgewählten Titeln.", style = "text-align: center;"),
        actionButton("right_button", label = "", icon = icon("arrow-right"), style = sprintf("color: %s;", right_color))
    )
  })
}

# handles the navigation with the buttons and checks if no points are selected
navigate_left <- function(input, selectedTitle, titleDetails) {
  observeEvent(input$left_button, {
    if (length(titleDetails$titlesList) > 0) {
      current_index <- match(selectedTitle(), titleDetails$titlesList)
      if (!is.na(current_index) && current_index > 1) {
        selectedTitle(titleDetails$titlesList[[current_index - 1]])
      }
    }
  })
}

navigate_right <- function(input, selectedTitle, titleDetails) {
  observeEvent(input$right_button, {
    if (length(titleDetails$titlesList) > 0) {
      current_index <- match(selectedTitle(), titleDetails$titlesList)
      if (!is.na(current_index) && current_index < length(titleDetails$titlesList)) {
        selectedTitle(titleDetails$titlesList[[current_index + 1]])
      }
    }
  })
}
