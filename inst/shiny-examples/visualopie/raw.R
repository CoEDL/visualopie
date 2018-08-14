##################################
## Raw data page, developer only.
##################################

raw_ui <- function(id) {
  #' Auxiliary tab for displaying raw data table
  #'
  #' @param id - namespace for this tab.

  # namespace
  ns <- NS(id)

  tabItem(
    tabName = id,
    DT::DTOutput(ns("dataTable1")),
    DT::DTOutput(ns("reducedTable")),
    DT::DTOutput(ns("memoryGameTable"))
  )

}

raw_data_table <- function(input, output, session, data = NULL) {
  #' Renders the table displaying the full set of data before contracting
  #'
  #' @param input - Shiny inputs.
  #' @param output - Shiny outputs.
  #' @param session - Shiny session.
  #' @param data - raw data table to render.

  output$dataTable1 <- DT::renderDT({
    data
  })
}

raw_reduced_table <- function(input, output, session, data = NULL) {
  #' Renders the table displaying the reduced the data
  #'
  #' @param input - Shiny inputs.
  #' @param output - Shiny outputs.
  #' @param session - Shiny session.
  #' @param data - raw data table to render.

  output$reducedTable <- DT::renderDT({
    data
  })
}

raw_memory_table <- function(input, output, session, data = NULL) {
  #' Renders the table displaying the reduced the data
  #'
  #' @param input - Shiny inputs.
  #' @param output - Shiny outputs.
  #' @param session - Shiny session.
  #' @param data - raw data table to render.

  output$memoryGameTable <- DT::renderDT({
    datatable(data)
  })
}
