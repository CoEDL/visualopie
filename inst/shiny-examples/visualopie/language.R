## language.R ## tab that displays language centric statistical information


lang_ui <- function(id) {
  ns <- NS(id)
  tabItem(
    tabName = id,
    
    fluidRow(
      class = "languageRow",
      box(
        title = tags$b("Memory Game Words"), status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
        infoBoxOutput(ns("wordCountBox"), width = 4),
        infoBoxOutput(ns("mostUsedLang"), width = 4),
        infoBoxOutput(ns("mostCommonWord"), width = 4),
        DT::DTOutput(ns("memDerived"))
      ),
      
      box(
        title = tags$b("Word Visualisation"), status = "primary", solidHeader = TRUE, width = 12, collapsible = TRUE,
        fluidRow(
          column(width = 6, p("Choose language(s) to visualise: "))
        ),
        fluidRow(
          column(width = 6, selectInput(ns("langSelect"), label = NULL, c(), multiple = TRUE)),
          column(width = 6, actionButton(ns("langSelectButton"), "Visualise Language(s)"))
        ),
        fluidRow(
          class = "wordRow",
          column(width = 6, plotOutput(ns("langstack"))),
          column(width = 6, plotOutput(ns("stackbar")))
        ),
        
        
        fluidRow(
          column(width = 6, p("Choose word to visualise: "))
        ),
        fluidRow(
          column(width = 6, selectInput(ns("wordSelect"), label = NULL, c())),
          column(width = 6, actionButton(ns("wordSelectButton"), "Visualise Word"))
        ),
        column(width = 6, plotOutput(ns("wordbar"))),
        
        tags$head(tags$style(
          ".wordRow {margin-top: 4rem}"        
        ))
      )
    )
  )
}

lang_table <- function(data) {
  #' Build derived data table for languages
  #' 
  #' @param data - raw data table of memory game logs
  
  mylist.names <- c("word", "language", "level", "correct", "incorrect", "session")
  memlog <- vector("list", length(mylist.names))
  names(memlog) <- mylist.names
  
  for(i in 1:length(data$level)) {
    cor_vec <- unlist(strsplit(data$correct[i], split=","))
    incor_vec <- unlist(strsplit(data$incorrect[i], split=","))
    len <- (length(cor_vec) + length(incor_vec))
    
    # Add correct words
    memlog$word <- c(memlog$word, cor_vec)
    memlog$correct <- c(memlog$correct, rep(1, each = length(cor_vec)))
    memlog$incorrect <- c(memlog$incorrect, rep(0, each = length(cor_vec)))
    # Add incorrect words
    memlog$word <- c(memlog$word, incor_vec)
    memlog$incorrect <- c(memlog$incorrect, rep(1, each = length(incor_vec)))
    memlog$correct <- c(memlog$correct, rep(0, each = length(incor_vec)))
    
    memlog$level <- c(memlog$level, rep(data$level[i], each = len))
    memlog$language <- c(memlog$language, rep(data$language[i], each = len))
    memlog$session <- c(memlog$session, rep(data$session[i], each = len))
  }
  
  memRaw <- data.table(memlog$word, memlog$language, memlog$level, memlog$correct, memlog$incorrect, memlog$session) %>%
    setNames(c("word", "language", "level", "correct", "incorrect", "session")) %>%
    group_by(word, language, level, session) %>%
    summarise(correct = sum(correct), incorrect = sum(incorrect), number = (sum(correct)+sum(incorrect))) 
  
  memTbl <- data.table(memlog$word, memlog$language, memlog$level, memlog$correct, memlog$incorrect) %>%
    setNames(c("word", "language", "level", "correct", "incorrect")) %>%
    group_by(word, language, level) %>%
    summarise(correct = sum(correct), incorrect = sum(incorrect), number = (sum(correct)+sum(incorrect))) 
  
  return(list(memTbl, memRaw))
  
}

lang_table_viz <- function(input, output, session, data = NULL) {
  #' Renders the table displaying the reduced the data 
  #' 
  #' @param data - raw data table for language page
  
  memTbls <- lang_table(data)
  languages <- unique(memTbls[[1]]$language)
  updateSelectInput(session, "langSelect", choices = languages, selected = languages)
  
  output$memDerived <- DT::renderDataTable({
    datatable(memTbls[[1]], 
              class = "display cell-border",
              filter = 'top',
              options = list(
                order = list(list(1, 'asc'))
              ), 
              colnames = c("Word", "Language", "Memory Game Lvl","Times Correct", "Times Incorrect", "Times Seen")
    )
  })
}

update_stack_bars <- function(input, output, session) {
  
  observeEvent(input$langSelectButton, {
    tmp <- memoryGameTbl %>% filter(language %in% input$langSelect)
    lang_bar_plot(input, output, session, data = tmp)
    lang_stack_viz(input, output, session, data = tmp)
  })
  
}

lang_stack_viz <- function(input, output, session, data = NULL) {
  #' Renders the table displaying the reduced the data 
  #' 
  #' @param data - raw data table for language page
  
  memTbls <- lang_table(data)
  tmp <- memTbls[[1]]
  
  updateSelectInput(session, "wordSelect", choices = unique(tmp$word))
  
  tbl <- data.table(tmp$word, tmp$correct, tmp$incorrect) %>%
    set_names("word", "correct", "incorrect") %>%
    melt(id = "word") %>%
    group_by(word) %>% mutate(total = sum(value)) %>% ungroup() %>% arrange(total) %>%
    mutate(word = factor(word, levels = unique(as.character(word))))
  
  output$stackbar <- renderPlot({
    ggplot(data = tbl, aes(x = word, y = value, fill = variable, hjust = 0.25)) + 
      geom_col(stat = "identity") + coord_flip() +
      scale_fill_manual(values = c("#00b300","#ff1919")) 
      #stat_summary(fun.y = sum, aes(label = ..y.., group = variable), geom = "text")
  })
}


lang_bar_plot <- function(input, output, session, data = NULL) {
  #' Add doc later.

  memTbls <- lang_table(data)
  langBarData <- memTbls[[1]]
  
  langtbl <- data.table(langBarData$language, langBarData$correct, langBarData$incorrect) %>%
    set_names("language", "correct", "incorrect") %>%
    melt(id = "language")
  
  output$langstack <- renderPlot({
    ggplot(data = langtbl, aes(x = language, y = value, fill = variable, vjust = 0.5)) + 
      geom_col(stat = "identity") +
      scale_fill_manual(values = c("#00b300","#ff1919")) 
      #stat_summary(fun.y = sum, aes(label = ..y.., group = variable), geom = "text")
  })
}

update_word_bar <- function(input, output, session, data = NULL) {
  #'
  #'
  
  observeEvent(input$wordSelectButton, {
    plotWord(input, output, session, data = memoryGameTbl)
  })
  
}

plotWord <- function(input, output, session, data = NULL) {
  #' Add doc later
  #' 
  
  memTbls <- lang_table(data)
  wordData <- memTbls[[1]]
  
  if(!is.null(input$wordSelect) & !is.null(input$langSelect)) {
    
    wordData <- wordData %>% filter(language %in% input$langSelect) %>%
      filter(word == input$wordSelect) 
    
    wordTbl <<- data.table(wordData$level, wordData$correct, wordData$incorrect) %>%
      set_names("level", "correct", "incorrect") %>%
      melt(id="level")
    
    output$wordbar <- renderPlot({
      ggplot(wordTbl, aes(x = level, y = value, fill = variable)) +
        geom_col(width=0.4, position = position_dodge(width=0.5)) + 
        scale_x_continuous(breaks = seq(0, max(wordTbl$level), 1), lim = c(0, max(wordTbl$level)))
        
    })
  }
}


