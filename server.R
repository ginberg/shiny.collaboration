server = function(input, output) {
  
  # connect
  connection <- shiny.collections::connect()
  
  # We create collection object, where data$collection is reactive value.
  data <- shiny.collections::collection("visitordata", connection)
  column_names <- c("name", "description")
  
  isolate({
    # If we run the app for the first time, we should add some content
    if(is_empty(data$collection)) {
      shiny.collections::insert(data, list(name = "Ger", description = "Live collaboration with a rhandsontable! Add or delete rows by right clicking on this table."))
    }
  })
  
  # Reactive which gives list with changes.
  change_list <- reactive({
    changes <- NULL
    if(!is.null(input$datatable$changes$changes)) {
      changes <- input$datatable$changes$changes %>%
        map(function (change) {
          # +1 is needed as JS and R indexing differs.
          row <- change[[1]] + 1
          col <- change[[2]] + 1
          new_value <- change[[4]]
          list(row = row, col = col, val = new_value)
        })
    }
    changes[[1]]
  })
  
  # Here we observe for a change and update data using shiny.collections
  # insert function.
  observe({
    if (!is.null(change_list()$val)) {
      change_row <- as.list(data$collection[change_list()$row, ])
      change_col <- column_names[[change_list()$col]]
      change_row[[change_col]] <- change_list()$val
      shiny.collections::insert(data,
                                change_row,
                                conflict = "update")
    }
  })
  
  output$datatable <- renderRHandsontable({
    rhandsontable(data$collection[column_names], useTypes = TRUE) %>%
      hot_table(readOnly = FALSE)
  })
  
}