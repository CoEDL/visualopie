## user.R ## displays user centric statistical information

user_ui <- function(id) {
  #' UI for user tab where user-centric data will be displayed.
  #'
  #' @param id - tab id

  ns <- NS(id)

  tabItem(
    tabName = id,

    fluidRow(
      class = "userRow",
      box(
        title = tags$b("User Options"), status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
        fluidRow(
          column(width = 6, p("Choose user(s): "))
        ),
        fluidRow(
          # Allow selection of one or more user(s) to visualise
          column(width = 6, selectInput(ns("userSelector"), label = NULL, c(), multiple = TRUE)),
          column(width = 6, actionButton(ns("userUpdateBtn"), "Visualise User(s)"))
        )
      )
    ),

    fluidRow(
      class = "userRow",
      box(
        title = tags$b("User usage stats"), status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
        fluidRow(
          column(width = 12, plotOutput(ns("userLanguageBar")))
        )
      )
    ),
    tags$head(tags$style(
      ".userRow { margin-top: 2rem; margin-bottom: 2rem}"
    ))
  )
}

update_user_selector <- function(input, output, session, data = NULL) {
  #' Updates the user select input for the user page.
  #' This function will be called upon data load to initialise and can be called again.
  #'
  #' To get the options selected use input$userSelector.
  #'
  #' @param input - Shiny inputs
  #' @param output - Shiny outputs
  #' @param session - Current shiny session
  #' @param data - main raw data table from robot.

  updateSelectInput(session, "userSelector", choices = unique(data$name), selected = unique(data$name))
}

plot_user_language_stats <- function(input, output, session, data = NULL) {
  #' Plots bar and pie charts for language usage for user(s).
  #' Update with update_user_stats event.
  #'
  #' @param input - Shiny inputs
  #' @param output - Shiny outputs
  #' @param session - Current shiny session
  #' @param data - main raw data table from robot

  # Get relevant data for languages only
  languageDT <- filter(data, type == 'language')

  output$userLanguageBar <- renderPlot({
    ggplot(languageDT, aes(x = value, y = times, fill = value, label = times)) +
      geom_bar(position = "dodge", stat = "identity") + facet_wrap(~name) +
      labs(x = "Languages", y = "", title = "Language Usage Count for Users") +
      scale_y_continuous(breaks = seq(0, max(languageDT$times), 2), limits = c(0, max(languageDT$times))) +
      geom_text(label = languageDT$times, vjust = -.25) +
      theme(plot.title=element_text(size = 16, face = 'bold', hjust = 0.5),
            axis.title=element_text(size = 12, face = 'bold'),
            axis.text.x=element_text(size = 12, face = 'bold'),
            legend.title=element_blank(),
            legend.position = 'bottom')
  })
}

update_user_stats <- function(input, output, session, data = NULL) {
  #' Update the user data based on selector.
  #'
  #' @param input - Shiny inputs
  #' @param output - Shiny outputs
  #' @param session - Current shiny session

  observeEvent(input$userUpdateBtn, {
    tmp <- data %>% filter(name %in% input$userSelector)
    plot_user_language_stats(input, output, session, data = tmp)
  })

}
