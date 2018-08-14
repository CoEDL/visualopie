library(shiny)
library(shinydashboard)
library(tidyverse)
library(data.table)
library(DT)

source("global.R")
source("raw.R")
source("dashboard.R")
source("user.R")
source("activity.R")
source("language.R")
source("dataLoader.R")

######################
## Main GUI Layout
######################

ui <- dashboardPage(
  dashboardHeader(
    title = "VisualOPIE",
    titleWidth = MENU_WIDTH
  ),

  dashboardSidebar(
    width = MENU_WIDTH,
    sidebarMenu(
      id = "sidebar",

      menuItem("User",
               tabName = "user",
               icon = icon("users")),
      menuItem("Language",
               tabName = "language",
               icon = icon("book")),
      menuItem("Activity",
               tabName = "activity",
               icon = icon("bullseye")),
      menuItem("Dashboard",
               tabName = "dashboard",
               icon = icon("dashboard"))
      # Raw Data shows a tab with developer data tables.
      # menuItem("Raw Data",
      #          tabName = "raw",
      #          icon = icon("database")),
    )
  ),

  dashboardBody(
    # Custom style sheet link for www/custom.css
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),

    # Directory selector and data loading functionality, on each page.
    fluidRow(
      # Main wrapper box
      box(
        title = tags$b("Load Data"), status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
        # File Loader section
        box(
          title = tags$b("OPIE logs"), status = "info", width = 6,
          directoryInput('directory', label = 'Select folder or device to load from', value = '~'),
          actionButton("loadButton", "Load Data")
        ),
        # File load options
        box(
          title = tags$b("Options"), status = "info", width = 6,
          radioButtons("loadOpts",
                       choices = list("Choose own folder" = 1, "Load from OPIE" = 2),
                       selected = 1, inline = TRUE,
                       label = "Choose where you are loading your logs from.")
        ),
        # Statistics for logs loaded, also acts as a visual indicator of success.
        box(
          width = 12, status = "info",
          infoBoxOutput("logsLoadedBox", width = 4),
          infoBoxOutput("numUserBox", width = 4),
          infoBoxOutput("numLanguages", width = 4)
        )
      )
    ),

    # Pages for each section of VisualOPIE, acts as tabs selectable on the sidebar.
    tabItems(
      user_ui("user"),
      lang_ui("language"),
      activity_ui("activity"),
      dash_ui("dashboard")
      # raw_ui("raw")
    )
  )
)
