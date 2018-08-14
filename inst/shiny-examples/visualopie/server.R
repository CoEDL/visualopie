library(shiny)
library(shinydashboard)
library(tidyverse)
library(data.table)
library(DT)

source("global.R")
source("user.R")
source("activity.R")
source("language.R")
source("dashboard.R")
source("utility.R")
source("dataLoader.R")
source("raw.R")

################################
## Main Shiny Server functions
################################

server <- function(input, output, session) {
  #' Server function provides the logic for interacting with the various UI elements.
  #'
  #' All server functions exist here as an initial call, example dataLoader.R
  #'
  #' @param input  - Shiny inputs
  #' @param output - Shiny outputs
  #' @param session - Current session
  #'

  # Initialise and render general data log statistics at start-up.
  output$logsLoadedBox <- renderInfoBox({
    infoBox(
      tags$b("Logs Loaded"), 0, icon = icon("sticky-note"), color = "orange"
    )
  })

  output$numUserBox <- renderInfoBox({
    infoBox(
      tags$b("Number of Profiles"), 0, icon = icon("users"), color = "orange"
    )
  })

  output$numLanguages <- renderInfoBox({
    infoBox(
      tags$b("Number of Languages"), 0, icon = icon("comments"), color = "orange"
    )
  })

  ## Call Modules to start pages
  # Activity Page
  callModule(activity_stats_update, "activity", NULL)

  # Data Loader - where all visualising begins.
  dataLoader(input, output, session)

  # Update language page
  update_lang_stack_bars(input, output, session)

  ## Other global functions ##
  # Allow notifications to be removed manually
  observeEvent(input$remove, {
    if (!is.null(id))
      removeNotification(id)
    id <<- NULL
  })
}
