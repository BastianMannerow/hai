library(DT)

generateAnomalyTable <- function(rv) {
  renderDT({
    data = isolate(rv$x)
    datatable(data,
              escape = FALSE,
              extensions = "Buttons", 
              editable = list(target = "cell", disable = list(columns = c(0, 1, 2, 3, 4))),
              class = 'compact stripe', 
              caption = "Hier erscheinen Ihre ausgewählten Datenpunkte. Ergänzen Sie im Kommentar, warum Sie den Datenpunkt als Ausreißer werten. Sie können Einträge auch löschen.",
              rownames = FALSE, 
              options = list(
                paging = FALSE,
                searching = FALSE,
                fixedColumns = FALSE,
                autoWidth = FALSE,
                dom = 'Bfrtip',
                info = FALSE,
                buttons = list(list(
                  extend = 'collection',
                  buttons = c('csv', 'excel', 'pdf'),
                  text = 'Download Ausreißer-Liste'
                )),
                ordering = TRUE,
                columnDefs = list(
                  list(className = 'dt-center', targets = "_all"),
                  list(orderable = TRUE, targets = c(0, 1, 2, 3, 4)),
                  list(orderable = FALSE, targets = 5)
                )
              ))
  })
}

setupDataTableInteractions <- function(rv, session, input) {
  proxy <- dataTableProxy('mydata')
  
  observe({
    replaceData(proxy, unique(rv$x), resetPaging = FALSE, rownames = FALSE)
  })
  
  observeEvent(input[["remove"]], {
    showModal(modalDialog(
      title="Möchten Sie die markierten Einträge aus der Tabelle löschen?",
      "Die dazugehörigen Punkte werden im oberen Plot dann nicht mehr als Anomalie gekennzeichnet sein.
      Bitte bestätigen Sie.",
      footer = tagList(actionButton("confirmDelete", "Löschen bestätigen"),
                       modalButton("Abbrechen")
      )
    ))
  })
  
  observeEvent(input$confirmDelete, {
    removeModal()
    req(input[["mydata_rows_selected"]])
    indices <- input[["mydata_rows_selected"]]
    rv$x <- rv$x %>% filter(!row_number() %in% indices)
  })
  
  observeEvent(input$mydata_cell_edit, {
    info = input$mydata_cell_edit
    i = info$row
    j = info$col + 1
    v = info$value
    rv$x[i, j] <- v
  })
  
  session$sendCustomMessage(type = 'bind-remove', message = list(selector = '#remove'))
}