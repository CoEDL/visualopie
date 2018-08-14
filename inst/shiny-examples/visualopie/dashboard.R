#################################
## General Facilitator Dashboard
#################################

# TODO: Needs redesign as information is not particularly targetted.

######
## UI
######

dash_ui <- function(id) {
  #' User interface for the general facilitator dashboard.
  #'
  #' @param id - namespace id for activity functions.

  # Namespace
  ns <- NS(id)

  tabItem(
    tabName = id,

    fluidRow(
      class = "dashRow",
      box(
        title = tags$b("Activity Overview (Facilitator)"), status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
        column(6, plotOutput(ns("bar_state_duration"))),
        column(6, plotOutput(ns("activity_durations")))
      )
    ),
    fluidRow(
      class = "dashRow",
      box(
        title = tags$b("User Overview (Facilitator)"), status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
        column(6, plotOutput(ns("childDistribution"))),
        column(6, plotOutput(ns("boxChildPlot"))),
        column(6, plotOutput(ns("activity_compare")))
      )
    ),
    tags$style(".dashRow { margin-top: 2rem; margin-bottom: 2rem}")
  )
}

dash_box_child_plot<- function (input, output, session, data) {
  #' Displays duration based data including total runtime in each state
  #'
  #' @param input - Shiny inputs.
  #' @param output - Shiny outputs.
  #' @param session - Shiny session.
  #' @param data - dataframe derived from facilitator logs.

  output$boxChildPlot <- renderPlot({
    # Violin plot for child distribution per activity state
    ggplot(data, aes(x=state, y=max_child, fill = state)) +
      geom_violin() +
      labs(title="Max children distribution in each activity") +
      theme(plot.title=element_text(size = 16, face = 'bold', hjust = 0.5),
            axis.title=element_text(size = 12, face = 'bold'))
  })
}

dash_child <- function(input, output, session, data) {
  #' Generates plot of minimum durations vs number of children for each activity.
  #'
  #' @param input - Shiny inputs.
  #' @param output - Shiny outputs.
  #' @param session - Shiny session.
  #' @param data - dataframe derived from facilitator logs.

  children_per_activity <- data%>%
    filter(state!="login" & state!="out_of_app" & num_children!=0)%>%
    group_by(state,num_children)%>%
    summarize(time=sum(dur_min))

  output$childDistribution <- renderPlot({
    # Plot for children per state recorded in facilitator log
    ggplot(children_per_activity,aes(x=num_children,y=time,fill = state)) +
      geom_col() +
      facet_grid(. ~ state) +
      labs(title = "Children distribution per game",x="Children number", y="Total duration (min)") +
      theme_bw() +
      theme(plot.title=element_text(size = 16, face = 'bold', hjust = 0.5),
            axis.title=element_text(size = 12, face = 'bold'))
  })
}

dash_activity_duration_plot <- function(input, output, session, data) {
  #' Generates plot of duration per activity.
  #'
  #' @param input - Shiny inputs.
  #' @param output - Shiny outputs.
  #' @param session - Shiny session.
  #' @param data - dataframe derived from facilitator logs.

  df <- data %>% mutate(dur_min = round(dur_min, 2))

  output$bar_state_duration <- renderPlot({
    # Plot total duration (in minutes) of OPIE states.
    ggplot(data = df, aes(state, dur_min, fill = state)) +
      geom_col() +
      stat_summary(fun.y = sum, aes(label = ..y.., group = state, vjust = -.5), geom = "text") +
      labs(x = "OPIE State", y = "Duration (minutes)", title = "Total time spent in OPIE by state") +
      theme(plot.title=element_text(size = 16, face = 'bold', hjust = 0.5),
            axis.title=element_text(size = 12, face = 'bold'))
  })
}

dash_activity_comparison <- function(input, output, session, data) {
  #' Generates plot of duration x number of children x robot state
  #'
  #' @param input - Shiny inputs.
  #' @param output - Shiny outputs.
  #' @param session - Shiny session.
  #' @param data - dataframe derived from facilitator logs.

  output$activity_compare <- renderPlot({
    df <- data %>% mutate(dur_min = round(dur_min, 2)) %>% filter(state %in% c("memory", "story", "repetition"))
    df <- df %>% mutate_at(scale, .vars = vars(dur_min))

    # Duration x NumChildren x State
    ggplot(data = df, aes(max_child, dur_min, shape = state, color = state)) +
      geom_point(size = 2) +
      geom_smooth(span = 0.9, method=lm, se=TRUE, fullrange=TRUE) +
      labs(x = "Max Num Children", y = "Duration (minutes)", title = "Duration in activity per grouping of children.") +
      theme(plot.title=element_text(size = 16, face = 'bold', hjust = 0.5),
            axis.title=element_text(size = 12, face = 'bold'))
  })
}

dash_activity_ratio_plot <- function(input, output, session, data) {
  #' Generates activity ratio plot.
  #'
  #' @param input - Shiny inputs.
  #' @param output - Shiny outputs.
  #' @param session - Shiny session.
  #' @param data - dataframe derived from facilitator logs.

  # Sum all state durations by time(minutes)
  df <- data %>% group_by(state) %>% summarise(dur_min = sum(dur_min))

  # Aggregate for when OPIE running an activity
  in_activity <- filter(df, state %in% c("memory", "story", "repetition")) %>% summarise(total_minutes = sum(dur_min))
  in_activity["state"] <- "In Activity"

  # Aggregate when OPIE not running an activity
  non_activity <- filter(df, state %in% c("login", "out_of_app")) %>% summarise(total_minutes = sum(dur_min))
  non_activity["state"] <- "Out of Activity"

  activity_ratio <- bind_rows(in_activity, non_activity)
  activity_ratio <- mutate(activity_ratio,
                           percentage = (activity_ratio$total_minutes / sum(activity_ratio$total_minutes)) * 100)

  output$activity_durations <- renderPlot({
    # Plot ratio in/out of activity as a pie chart.
    ggplot(data = activity_ratio, aes(x = "", y = percentage, fill = state)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0, direction = 1) +
      scale_fill_manual(values = c("#fee8c8", "#e34a33")) +
      labs(x = "", y = "", title = "Time spent in activities VS out of activities") +
      geom_text(aes(y = percentage, label = round(percentage, 2)), position = position_stack(vjust = 0.5)) +
      theme(plot.title = element_text(size = 16, face = 'bold', hjust = 0.5),
            axis.ticks = element_blank(),
            axis.text = element_blank()
      )
  })
}
