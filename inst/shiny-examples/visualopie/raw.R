## raw.R ## Raw data tab  

raw_ui <- function(id) {
  #' Auxiliary tab for displaying raw data table
  #' 
  #' @param id - tab id
  
  ns <- NS(id)
  tabItem(
    tabName = id,
    #selectInput("sessionSelect", "Choose Log File", logNames, multiple = TRUE),
    DT::DTOutput(ns("dataTable1")),
    DT::DTOutput(ns("reducedTable")),
    DT::DTOutput(ns("memoryGameTable"))
  )
  
}

raw_data_table <- function(input, output, session, data = NULL) {
  #' Renders the table displaying the full set of data before contracting
  output$dataTable1 <- DT::renderDT({
    data
  })
} 

raw_reduced_table <- function(input, output, session, data = NULL) {
  #' Renders the table displaying the reduced the data 
  
  output$reducedTable <- DT::renderDT({
    data
  })
}

raw_memory_table <- function(input, output, session, data = NULL) {
  #' Renders the table displaying the reduced the data 
  
  output$memoryGameTable <- DT::renderDT({
    datatable(data)
  })
}
