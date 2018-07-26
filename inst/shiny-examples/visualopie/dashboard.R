## dashboard.R ## main page that displays overall information

dash_ui <- function(id) {
  #' User interface for the dashboard
  #' 
  #' @param id - the id used for the parent frame that will contain the dashboard ui
  
  ns <- NS(id) # Creating namespace allocator for dash_ui
  
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
  #' @param input
  #' @param output
  #' @param session
  #' @param data - dataframe which includes info including time, state, score, num_children, max_child
  
  output$boxChildPlot <- renderPlot({
    
    ggplot(data, aes(x=state, y=max_child, fill = state)) +
      geom_violin() +
      labs(title="Boxplot of max children distribution in each activity") + 
      theme(plot.title=element_text(size = 16, face = 'bold', hjust = 0.5),
            axis.title=element_text(size = 12, face = 'bold'))
  })
}

dash_child <- function(input, output, session, data) {
  #' Server function that generates plot of dur_min vs num_child for each activity. 
  #' 
  #' @param input
  #' @param output
  #' @param session
  #' @param data - dataframe which includes info including time, state, score, num_children, max_child
  
  
  output$childDistribution <- renderPlot({

    children_per_activity <- data%>%
      filter(state!="login" & state!="out_of_app" & num_children!=0)%>%
      group_by(state,num_children)%>%
      summarize(time=sum(dur_min))
    
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

  output$bar_state_duration <- renderPlot({
    df <- data %>% mutate(dur_min = round(dur_min, 2))
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
  
  output$activity_durations <- renderPlot({
    # Sum all state durations by time(minutes)
    df <- data %>% group_by(state) %>% summarise(dur_min = sum(dur_min))
    
    # Aggregate when OPIE running an activity
    in_activity <- filter(df, state %in% c("memory", "story", "repetition")) %>% summarise(total_minutes = sum(dur_min))
    in_activity["state"] <- "In Activity"
    
    # Aggregate when OPIE not running an activity
    non_activity <- filter(df, state %in% c("login", "out_of_app")) %>% summarise(total_minutes = sum(dur_min))
    non_activity["state"] <- "Out of Activity"
    
    activity_ratio <- bind_rows(in_activity, non_activity)
    activity_ratio <- mutate(activity_ratio, 
                             percentage = (activity_ratio$total_minutes / sum(activity_ratio$total_minutes)) * 100)
    # print(activity_ratio)
    
    # Plot ratio in/out of activity
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
