server = function(input, output) {

  # connect
  connection <- shiny.collections::connect()
  
  # We create collection object, where data$collection is reactive value.
  visitordata <- shiny.collections::collection("visitordata", connection)
  column_names <- c("name", "description")
  
  getTimeInMillis <- function(){
    return(as.character(as.numeric(Sys.time())*1000))
  }
  
  isolate({
    # If we run the app for the first time, we should add some content
    if(is_empty(visitordata$collection)) {
      shiny.collections::insert(visitordata, list(name = "Ger", description = "Live collaboration with a rhandsontable!", id = getTimeInMillis()))
      shiny.collections::insert(visitordata, list(name = "Ger", description = "Add or delete rows by right clicking on this table.", id = getTimeInMillis()))
      shiny.collections::insert(visitordata, list(name = "Ger", description = "Btw, do you like reactive programming?", id = getTimeInMillis()))
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
      
      change_row <- as.list(visitordata$collection[change_list()$row, ])
      change_col <- column_names[[change_list()$col]]
      
      # Neglect update if nothing changed
      if(!all(is.na(change_row))) {
        if(change_row[[change_col]] == change_list()$val){
          print("No update needed")
          return()
        }
      }
      
      # update value
      change_row[[change_col]] <- change_list()$val
      
      # set default values
      if(is.na(change_row$name)){
        change_row$name <- "Ger"
      }
      if(is.na(change_row$description)){
        change_row$description <- "Empty description"
      }
      
      # remove id field if empty, since the database should generate it
      new_item <- change_row
      if("id" %in% names(new_item) && is.na(new_item$id)){
        new_item$id <- getTimeInMillis()
      }
      shiny.collections::insert(visitordata,
                                new_item)
    }
  })
  
  output$datatable <- renderRHandsontable({
    rhandsontable(visitordata$collection %>% arrange(id) %>% select(column_names), useTypes = TRUE) %>%
      hot_table(readOnly = FALSE)
  })

}