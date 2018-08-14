###############################
## Activity Visualisation Page
###############################

source('language.R')

######
## UI
######

activity_ui <- function(id) {
  #' User interface for the activity data page.
  #'
  #' The activity page should show activity usage information, and general results based on each activity if
  #' applicable.
  #'
  #' @param id - namespace id for activity functions.

  # Namespace
  ns <- NS(id)

  tabItem(
    tabName = id,
    fluidRow(
      class = "activityRow",
      box(
        title = tags$b("Activity Stats"), status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
        infoBoxOutput(ns("totalActivitiesBox"), width = 3),
        infoBoxOutput(ns("memoryPlayedBox"), width = 3),
        infoBoxOutput(ns("storiesPlayedBox"), width = 3),
        infoBoxOutput(ns("repsPlayedBox"), width = 3),
        DT::dataTableOutput(ns("activityTbl"))
        )
      ),
    fluidRow(
      class = "activityRow",
      box(
        title = tags$b("General Activity Data"), status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
        column(width = 6, plotOutput(ns("activityNumbersPie"))),
        column(width = 6, plotOutput(ns("activityTimeBar")))
      )
    ),
    fluidRow(
      class = "activityRow",
      box(
        title = tags$b("Memory Game Accuracy"), status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
        column(width = 6, plotOutput(ns("avgCorrectPlot"))),
        column(width = 6, plotOutput(ns("wordCorrectScatter")))
      )
    ),
    tags$head(tags$style(
      ".activityRow { margin-top: 1rem; margin-bottom: 1rem}"
      ))
    )

}


##################
## Data Rendering
##################

activity_stats_update <- function(input, output, session, data) {
  #' Update activity status information boxes. Default 0 if no data loaded.
  #'
  #' Each game makes a new log per attempt.
  #'
  #' @param input - Shiny inputs.
  #' @param output - Shiny outputs.
  #' @param session - Shiny session.
  #' @param data - Vector of containing logs from robot.

  totalAct <- 0
  numMemory <- 0
  numStory <- 0
  numRep <- 0

  if(!is.null(data)) {
    # Get total recorded logs per game + total
    totalAct <- length(data[[1]]) + length(data[[2]]) + length(data[[3]])
    numMemory <- length(data[[1]])
    numRep <- length(data[[2]])
    numStory <- length(data[[3]])
  }

  output$totalActivitiesBox <- renderInfoBox({
    infoBox(
      "Total Activities Run", totalAct, icon = icon("table"), color = "aqua", fill = TRUE
    )
  })
  output$memoryPlayedBox <- renderInfoBox({
    infoBox(
      "Memory Game", numMemory, icon = icon("graduation-cap"), color = "teal", fill = TRUE
    )
  })
  output$storiesPlayedBox <- renderInfoBox({
    infoBox(
      "Story", numStory, icon = icon("book"), color = "purple", fill = TRUE
    )
  })
  output$repsPlayedBox <- renderInfoBox({
    infoBox(
      "Repetition", numRep, icon = icon("microphone"), color = "maroon", fill = TRUE
    )
  })

  plot_activity_ratio(input, output, session, data)
}

derive_time_data <- function(input, output, session, data) {
  #' Derive time data from master tables. Sourced from facilitator logs only.
  #'
  #' @param input - Shiny inputs.
  #' @param output - Shiny outputs.
  #' @param session - Shiny session.
  #' @param data - master table to derive from.
  #' @return data table/frame of derived time data per activity.

  summary <- data %>% group_by(state) %>%
    filter(state %in% c("memory", "repetition", "story")) %>%
    summarise(MinDuration = round(min(duration), 2),
              MaxDuration = round(max(dur_min), 2),
              AvgDuration = round(mean(dur_min), 2))

  return(summary)
}

activity_tbl <- function(input, output, session, data) {
  #' Render activity time summary table.
  #'
  #' @param input - Shiny inputs.
  #' @param output - Shiny outputs.
  #' @param session - Shiny session.
  #' @param data - master data table from loaded data (facilitator logs).

  summary <- derive_time_data(input, output, session, data) %>%
    setnames(c("Activity", "Shortest Time (secs)", "Longest Time (mins)", "Average Time (mins)"))

  output$activityTbl <- DT::renderDataTable({
              datatable(summary,
              class = "display cell-border",
              options = list(dom = 't'))
  })
}

plot_activity_time <- function(input, output, session, data) {
  #' Server function that generates plot of times broken down by group.
  #'
  #' @param input - Shiny inputs.
  #' @param output - Shiny outputs.
  #' @param session - Shiny session.
  #' @param data - main data table of compiled log data.

  # Combine and group durational data for each activity for plotting
  summary <- derive_time_data(input, output, session, data) %>%
    mutate(MinDuration = MinDuration / 60) %>%
    melt(id = "state")

  output$activityTimeBar <- renderPlot({
    ggplot(summary, aes(x = state, y = value, fill = variable)) +
      geom_col(position = "dodge") +
      labs(x = "Activity", y = "Time (minutes)", fill = '', title = "Time per Activity") +
      theme(plot.title=element_text(size = 16, face = 'bold', hjust = 0.5),
            axis.title=element_text(size = 12, face = 'bold'),
            axis.text.x=element_text(size = 12, face = 'bold')
            )
  })
}

plot_activity_ratio <- function(input, output, session, data) {
  #' Generate pie for number of activities.
  #'
  #' @param input - Shiny inputs.
  #' @param output - Shiny outputs.
  #' @param session - Shiny session.
  #' @param data - main data table of compiled log data.

  # Get counts for activities
  numMemory <- length(data[[1]])
  numRep <- length(data[[2]])
  numStory <- length(data[[3]])

  # Form data table for chart
  activity <- c("Memory", "Repetition", "Story")
  n <- c(numMemory, numRep, numStory)
  dt <- data.table(activity, n) %>% mutate(percentage = round((n / sum(n) * 100), 2))

  output$activityNumbersPie <- renderPlot({
    ggplot(data = dt, aes(x = '', y = n, fill = activity)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar('y', start = 0, direction = 1) +
      labs(x = '', y = '', title = "Activity Popularity (% of Total Activities)", fill = "Activity") +
      theme(axis.ticks = element_blank(),
            axis.text = element_blank(),
            plot.title = element_text(size = 16, face = 'bold', hjust = 0.5)
            ) +
      geom_text(aes(y = n, label = paste0(percentage, '%')), position = position_stack(vjust = 0.5))
  })

}

plot_avg_words <- function(input, output, session, data) {
  #' Plot the average words correct/incorrect by level.
  #'
  #' Plot is a grouped bar plot.
  #'
  #' @param input - Shiny inputs.
  #' @param output - Shiny outputs.
  #' @param session - Shiny session.
  #' @param data - master data table of memory game logs to derive.

  # Calculate average words correct vs incorrect per level
  wordCorrectness <- data %>%
    group_by(level) %>%
    summarise(mean_cor = mean(score), mean_inc = mean(level - score)) %>%
    setnames(c("Level", "Avg Correct Guesses", "Avg Incorrect Guesses")) %>%
    melt(id = "Level")

  output$avgCorrectPlot <- renderPlot({
    ggplot(wordCorrectness, aes(x = Level, y = value, fill = variable)) +
      geom_col(position = "dodge") +
      scale_x_continuous(breaks = pretty(wordCorrectness$Level, n = 10)) +
      scale_y_continuous(breaks = pretty(wordCorrectness$Level, n = 10)) +
      labs(x = "Level", y = "Average Num. Words", title = "Average Words Correct VS Incorrect by Level") +
      theme(plot.title=element_text(size = 16, face = 'bold', hjust = 0.5),
            axis.title=element_text(size = 12, face = 'bold'),
            legend.title=element_blank(),
            legend.position = 'bottom')
  })
}

plot_word_level_scatter <- function(input, output, session, data) {
  #' Plots a scatter plot of words correct/incorrect by level.
  #'
  #' @param input - Shiny input
  #' @param output - Shiny output
  #' @param session - Shiny session
  #' @param data - raw data from load

  # Derive table of correctness each word/level
  scatterDT <<- data %>% group_by(level, session) %>% mutate(numCorrect = score, numIncorrect = level - score) %>%
    gather(correctness, value, c(numCorrect, numIncorrect))

  output$wordCorrectScatter <- renderPlot({
    ggplot(data = scatterDT, aes(x = level, y = value, shape = correctness, colour = correctness)) +
      geom_point(size = 2) +
      geom_jitter() +
      geom_smooth(method=lm) +
      scale_x_continuous(breaks = pretty(scatterDT$level, n = max(scatterDT$level))) +
      scale_y_continuous(breaks = pretty(scatterDT$value, n = max(scatterDT$value))) +
      labs(x = "Level", y = "", title = "Words Correct vs Incorrect per Level") +
      theme(plot.title=element_text(size = 16, face = 'bold', hjust = 0.5),
            axis.title=element_text(size = 12, face = 'bold'),
            legend.title=element_blank(),
            legend.position = 'bottom')
  })

}
