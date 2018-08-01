## dataLoader.R ## Main function for loading data files.

source("global.R")
source("utility.R")
source('widgets/shiny-directory-input/directoryInput.R')
source("dashboard.R")

dataLoader <- function(input, output, session) {
  #' Server function to handle operations instigated by the load button
  #'
  #' @param input
  #' @param output
  #' @param session

  # Run folder selection
  folderSelect(input, output, session)

  # Parse and visualise when load button hit.
  observeEvent(input$loadButton, {
    main_start <- Sys.time()
    # Check if loading from OPIE -> append OPIE's log path.
    if(input$loadOpts == 2) {
      if(Sys.info()['sysname'] == "Windows") {
        logPath <<- paste(logPath, OPIE_DEFAULT_LOGS_WIN, sep = "")
      } else {
        logPath <<- paste(logPath, OPIE_DEFAULT_LOGS, sep = "")
      }
    }

    # print("*** START ***")
    # start_time <- Sys.time()
    # Empty list of folders found in selected device/folder
    folderList <- c()
    if(!is.null(logPath)) {
      # Get a list of all folders, including root path selected by user (or OPIE's log folder)
      folderList <- list.dirs(path = logPath, full.names = TRUE, recursive = FALSE)
      folderList <- grep(folderList, pattern = "/Unity|/ExternalAssets", inv = TRUE, value = TRUE)
      folderList <- c(folderList, logPath)
    }
    # end_time <- Sys.time()
    # print(paste0("Get folder paths - Time taken:", end_time - start_time))
    # Emtpy list of faciliator logs as data tables
    facilRawTbls <- list(NULL, NULL)

    # Vector of paths to logs from robot separated into types
    logsFromRobot <- c()

    # start_time <- Sys.time()
    # Individual facilitator logs as data tables in a list
    logs_from_facil <- facilLogs(input, output, session, folderList)
    # end_time <- Sys.time()
    # print(paste0("Get facil logs - Time taken:", end_time - start_time))

    # start_time <- Sys.time()
    # Vector of paths to all robot logs by type
    logsFromRobot <- robotLogs(folderList)
    # end_time <- Sys.time()
    # print(paste0("Get robot logs - Time taken:", end_time - start_time))

    # Table of memory game data
    memoryGameTbl <<- NULL

    # Table for main logs
    mainLogsTbl <<- NULL

    # start_time <- Sys.time()
    if(length(logs_from_facil) == 0) {
      showUINotification("No facilitator logs found.", msgType = "error")
    } else {
      # bind individual tables together, with session marker.
      facilRawTbls <- facilTable(logs_from_facil)
    }
    # end_time <- Sys.time()
    # print(paste0("Facil table - Time taken:", end_time - start_time))

    # start_time <- Sys.time()
    if(length(logsFromRobot$memory) == 0) {
      showUINotification("No robot logs found.", msgType = "error")
    } else {
      # form robot log data tables
      memoryGameTbl <<- memoryLogs(logsFromRobot)
      mainLogsTbl <<- mainTable(logsFromRobot)
    }
    # end_time <- Sys.time()
    # print(paste0("Robot table - Time taken:", end_time - start_time))

    # start_time <- Sys.time()
    # Basic log statistics
    numLogs <- length(logs_from_facil) + sum(lengths(logsFromRobot)) - length(logsFromRobot$rejected)
    infoDialogue(input, output, session, numLogs)

    # Number of Profiles
    if(!is.null(memoryGameTbl)) {
      userCount <- nrow(mainLogsTbl %>% group_by(name) %>% summarise(n_distinct(name)))
      numUsersDialogue(input, output, session, userCount)

      langCount <- nrow(memoryGameTbl %>% group_by(language) %>% summarise(n_distinct(name)))
      numLangDialogue(input, output, session, langCount)
    }

    # Update Activity Count + Plot the associated pie chart
    callModule(activity_stats_update, "activity", data = logsFromRobot)
    # end_time <- Sys.time()
    # print(paste0("Log stats - Time taken:", end_time - start_time))

    # start_time <- Sys.time()
    # Start all visualisations (see function in this script - 'dataLoader.R')
    vizData = list(facilRawTbls[[1]], facilRawTbls[[2]], memoryGameTbl, mainLogsTbl)
    start_visualisation(vizData)
    # end_time <- Sys.time()
    # print(paste0("Viz - Time taken:", end_time - start_time))
    # main_end <- Sys.time()
    # print(paste0("TOTAL TIME:", main_end - main_start))
    # print("*** END ***")
    }
  )
}

facilTable <- function(logs_from_facil) {
  #' Create data tables for developer processing from a list of all paths to facilitator logs.
  #'
  #' @param logs_from_facil - list of facilitator logs as direct paths.

  # start_time <- Sys.time()
  stateData <<- rbindlist(logs_from_facil)
  stateData[, "min_child"] <- -1
  stateData[, "max_child"] <- -1
  setnames(stateData, c("time","state","num_children","score","notes", "session", "duration",
                        "dur_min", "min_child", "max_child"))

  # end_time <- Sys.time()
  # print(paste0("> Facil Tbl state data - Time taken:", end_time - start_time))

  # start_time <- Sys.time()
  reducedData <<- collapsetbl(stateData)

  reducedData <<- reducedData%>%
    group_by(state)%>%
    mutate(dur_min = duration/60)%>%
    filter(duration <= (mean(duration)+2*sd(duration)) & duration >= (mean(duration)-2*sd(duration)) & duration >= 2.0)

  # end_time <- Sys.time()
  # print(paste0("> Facil table - reduced data - Time taken:", end_time - start_time))

  tables <- list(stateData, reducedData)
  return(tables)
}

memoryLogs <- function (logs) {
  #' Builds the data table consisting of all memory game logs with valid data entries.
  #' Logs without a level played are excluded.
  #'
  #' @param logs - vector of paths to each memory game log file.
  #' @return data.table of memory game information.

  mylist.names <- c("name", "language", "level", "correct", "incorrect", "session")
  memlog <- vector("list", length(mylist.names))
  names(memlog) <- mylist.names

  # start_time <- Sys.time()
  for(id in 1:length(logs$memory)) {
    txt <- tolower(readLines(logs$memory[id]))
    len <- length(search_phrase("level", txt))
    if(len > 0) {
      memlog$level <- c(memlog$level, search_phrase("level", txt))
      memlog$correct <- c(memlog$correct, search_phrase("^correct", txt))
      memlog$incorrect <- c(memlog$incorrect, search_phrase("^incorrect", txt))
      memlog$name <- c(memlog$name, rep(search_phrase("name", txt), each = len))
      memlog$language <- c(memlog$language, rep(search_phrase("language", txt), each = len))
      memlog$session <- c(memlog$session, rep(sub("_log", "", basename(logs$memory[id])), each=len))
    }
  }

  memTbl <- data.table(memlog$name, memlog$language, memlog$level, memlog$correct, memlog$incorrect, memlog$session) %>%
    setNames(c("name", "language", "level", "correct", "incorrect", "session"))

  # end_time <- Sys.time()
  # print(paste0("> Memlogs table creation - Time taken:", end_time - start_time))

  # start_time <- Sys.time()
  memTbl <- memTbl %>%
    mutate(score = as.numeric(sub(".*scored ", "", level))) %>%
    mutate(level = sub(".*level ", "", level)) %>%
    mutate(level = as.numeric(sub("\ :(.*)", "", level))) %>%
    mutate(name = sub(".*: ", "", name)) %>%
    mutate(correct = sub(".*: ", "", correct)) %>%
    mutate(incorrect = sub(".*: ", "", incorrect)) %>%
    mutate(language = sub(".*: ", "", language))

  # end_time <- Sys.time()
  # print(paste0(">Memlogs mutate table - Time taken:", end_time - start_time))

  # start_time <- Sys.time()
  # Order the logs by language
  memTbl <- memTbl[order(memTbl$language, memTbl$level),]
  # end_time <- Sys.time()
  # print(paste0(">Mem logs order table - Time taken:", end_time - start_time))

  return(memTbl)

}

mainTable <- function(logs) {

  mainTbl <- NULL

  mylist.names <- c("name", "type", "value", "times", "session")
  mainlog <- vector("list", length(mylist.names))
  names(mainlog) <- mylist.names

  for(id in 1:length(logs$main)) {
    txt <- tolower(readLines(logs$main[id]))

    langid <- search_phrase("languages used:", txt, id = 1)
    actid <- search_phrase("games played", txt, id = 1)
    endid <- search_phrase("-----------", txt, id = 1)

    langs <- txt[(langid+1):(actid-1)]
    langs <- langs[langs != ""]
    acts <- txt[(actid+1):(endid-1)]
    acts <- acts[acts != ""]

    for (lid in 1:length(langs)) {
      mainlog$name <- c(mainlog$name, sub(".*: ", "", search_phrase("student", txt)))
      mainlog$type <- c(mainlog$type, "language")
      mainlog$value <- c(mainlog$value, sub("\t.*", "", langs[lid]))
      mainlog$times <- c(mainlog$times, sub("(.*)\t", "", langs[lid]))
      mainlog$session <- c(mainlog$session, sub("_log.txt", "", basename(logs$main[id])))
    }

    for (aid in 1:length(acts)) {
      mainlog$name <- c(mainlog$name, sub(".*: ", "", search_phrase("student", txt)))
      mainlog$type <- c(mainlog$type, "activity")
      mainlog$value <- c(mainlog$value, sub("\t.*", "", acts[aid]))
      mainlog$times <- c(mainlog$times, sub("(.*)\t", "", acts[aid]))
      mainlog$session <- c(mainlog$session, sub("_log.txt", "", basename(logs$main[id])))

    }

    mainTbl <- data.table(mainlog$name, mainlog$type, mainlog$value, as.numeric(mainlog$times), mainlog$session) %>%
      setNames(c("name", "type", "value", "times", "session"))

  }

  return(mainTbl)

}


robotLogs <- function(folderList) {
  #' Gather robot logs and divide up by type.
  #'
  #' @param folderList - list of folders to search through, including selected root.
  #' @return Vector of logs divided into activities.

  logs <- c()

  # start_time <- Sys.time()
  # Search folders for logs
  for(i in 1:length(folderList)) {
    tempLogs <- grep(list.files(path = folderList[i], full.names = TRUE), pattern = "*.wav$", value = T, inv = T)
    tempLogs <- grep(tempLogs, pattern = "_log", value = T)
    logs <- c(logs, tempLogs)
  }
  # end_time <- Sys.time()
  # print(paste0(">Robot logs - get files - Time taken:", end_time - start_time))

  # Game log data setup
  mylist.names <- c("memory", "repetition", "story", "main", "rejected")
  gamelogs <- vector("list", length(mylist.names))
  names(gamelogs) <- mylist.names

  # start_time <- Sys.time()
  # Compile valid log paths into gamelogs
  if(length(logs) > 0) {
    for(id in 1:length(logs)) {
      txt <- tolower(read_file(logs[[id]]))

      if(grepl("activity : memory game", txt, fixed = TRUE)) {
        gamelogs$memory <- c(gamelogs$memory, logs[[id]])
      } else if (grepl("activity : word repetition", txt, fixed = TRUE)) {
        gamelogs$repetition <- c(gamelogs$repetition, logs[[id]])
      } else if (grepl("activity : story", txt, fixed = TRUE)) {
        gamelogs$story <- c(gamelogs$story, logs[[id]])
      } else if (grepl("main", logs[[id]], fixed = TRUE)) {
        gamelogs$main <- c(gamelogs$main, logs[[id]])
      } else {
        gamelogs$rejected <- c(gamelogs$rejected, logs[[id]])
      }
    }
  }

  gamelogs <<- gamelogs
  # end_time <- Sys.time()
  # print(paste0(">Robot logs compile game logs - Time taken:", end_time - start_time))

  return(gamelogs)
}

infoDialogue <- function(input, output, session, numLogs) {
  #' Output info box for logs
  #'
  #' @param numLogs - the number of logs loaded.
  output$logsLoadedBox <- renderInfoBox({
    infoBox(
      tags$b("Logs Loaded"), numLogs, icon = icon("table"), color = "green"
    )
  })
}

numUsersDialogue <- function(input, output, session, numUsers) {
  #' Updates the general number of users information box post data loading.
  #'
  #' @param numUsers - the number of users (numeric)
  output$numUserBox <- renderInfoBox({
    infoBox(
      tags$b("Number of Profiles"), numUsers, icon = icon("users"), color = "green"
    )
  })
}

numLangDialogue <- function(input, output, session, numLangs) {
  #' Updates the number of languages information box post data loading.
  #'
  #' @param numUsers - the number of users (numeric)
  output$numLanguages <- renderInfoBox({
    infoBox(
      tags$b("Number of Languages"), numLangs, icon = icon("users"), color = "green"
    )
  })
}

facilLogs <- function(input, output, session, folderList) {
  #' Get all log filenames with full path, excluding audio files.
  #' Searches all folders including the selected root folder.
  #'
  #' @param input - Shiny inputs
  #' @param output - Shiny outputs
  #' @param session - Current Shiny session

  # Search folders for facilitator logs and get log names
  logNames <- NULL
  folderLogs <- NULL
  # start_time <- Sys.time()
  for(i in 1:length(folderList)) {
    # Facilitator logs are .txt files (other logs don't have a file ext)
    tempLogNames <- grep(list.files(path = folderList[i], full.names = FALSE), pattern = "*.txt", value = T)
    tempLogs <- grep(list.files(path = folderList[i], full.names = TRUE), pattern = "*.txt", value = T)

    # We don't want the main profile log, nor app Config.
    tempLogNames <- grep(tempLogNames, pattern = "_main|Config", value = T, inv = T)
    tempLogs <- grep(tempLogs, pattern = "_main|Config", value = T, inv = T)

    # Compile names
    tempLogNames <- sub("_log.*", "", tempLogNames)
    logNames <- c(logNames, tempLogNames)

    # Compile logs
    folderLogs <- c(folderLogs, tempLogs)
  }
  # end_time <- Sys.time()
  # print(paste0("> Facillogs - grab log names and paths - time taken:", end_time - start_time))
  # Remove duplicates
  # All actual logs that are parsed
  # start_time <- Sys.time()
  allLogs <- c()
  if(length(logNames) > 0) {
    duplicates = duplicated(logNames)
    for(i in 1:length(folderLogs)) {
      if(duplicates[i] == FALSE) {
        allLogs <- c(allLogs, folderLogs[i])
      }
    }
  } else {
    allLogs <- folderLogs
  }

  # Names of log files for session marker
  logNames <- unique(logNames)
  # end_time <- Sys.time()
  # print(paste0(">Facillogs - remove duplicates - time taken:", end_time - start_time))

  # start_time <- Sys.time()
  # Combine logs into one data table, logs == list(data, data, ...), data == data.table
  logs <- lapply(allLogs, fread, sep = "\t")
  # end_time <- Sys.time()
  # print(paste0(">Facillogs - combine logs into one dtable - time taken:", end_time - start_time))

  # start_timme <- Sys.time()
  # Account for time per log and append
  if(length(logs) > 0) {
    logs <- time_continuity(logs)

    for(id in 1:length(logs)) {
      logs[[id]] <- logs[[id]]%>%
        mutate(session = logNames[id])%>%
        mutate(duration=c(diff(time),0))%>%
        mutate(dur_min=duration/60)
    }
  }
  # end_time <- Sys.time()
  # print(paste0(">Facillogs - timecont + mutate - time taken:", end_time - start_time))
  return(logs)
}

folderSelect <- function(input, output, session) {
  #' Select a device or folder and store selected path to logPath global.
  #' Assumes default is 'home'.
  observeEvent(
    ignoreNULL = TRUE,
    eventExpr = {
      input$directory
    },
    handlerExpr = {
      if (input$directory > 0) {
        # condition prevents handler execution on initial app launch

        # launch the directory selection dialog with initial path read from the widget
        path = choose.dir(default = readDirectoryInput(session, 'directory'))

        # update the widget value
        updateDirectoryInput(session, 'directory', value = tail(path, n = 1))
        logPath <<- tail(path, n = 1)
      }
    }
  )
}

collapsetbl <- function(dataTable) {
  #' Creates a reduced tibble by grouping each state's contiguous block of entries into one entry
  #'
  #' @param dataTable - data table containing raw data from multiple logs concatenated together
  #' @return a reduced data table with contiguous state-wise data collapsed into a single entry

  X <- rle(dataTable$state)
  Y <- cumsum(c(1, X$lengths[-length(X$lengths)]))
  reduced <- dataTable[Y]
  for(id in 1:length(Y)) {

    tempDurations <- dataTable$duration[Y[id]:(Y[id] + (X$lengths[id] - 1))]
    tempChildren <- dataTable$num_children[Y[id]:(Y[id] + (X$lengths[id] - 1))]

    reduced[id] <- reduced[id] %>%
      mutate(duration = sum(tempDurations), max_child = max(tempChildren), min_child = min(tempChildren))
  }

  return(reduced)
}

start_visualisation <- function(data) {
  #' Start visualisations, after data logs processed.
  #' This is also where we ensure whether plots can or cannot be loaded.
  #'
  #' @param data - a list of data tables derived from loaded logs.

  if(!is.null(data[[1]]) & !is.null(data[[2]])) {
    # Raw data tables
    callModule(raw_data_table, "raw", data = data[[1]])
    callModule(raw_reduced_table, "raw", data = data[[2]])

    # Dashboard Visualisations
    callModule(dash_activity_duration_plot, "dashboard", data = data[[2]])
    callModule(dash_activity_ratio_plot, "dashboard", data = data[[2]])
    callModule(dash_activity_comparison, "dashboard", data = data[[2]])
    callModule(dash_box_child_plot, "dashboard", data = data[[2]])
    callModule(dash_child, "dashboard", data = data[[1]])

    # Activity Visuliations
    # Note, activity pie charts for percentage of activities as a ratio is done as count is updated at data load.
    callModule(activity_tbl, "activity", data = data[[2]])
    callModule(plot_activity_time, "activity", data = data[[2]])

    # Success
    showUINotification("Facilitator logs visualised!")
  }

  if(!is.null(data[[3]])) {
    # Raw memory game data table
    callModule(raw_memory_table, "raw", data = data[[3]])

    # Language Visualisations
    callModule(lang_table_viz, "language", data = data[[3]])
    callModule(lang_stack_viz, "language", data = data[[3]])
    callModule(lang_bar_plot, "language", data = data[[3]])
    callModule(plotWord, "language", data = data[[3]])

    # Update stack visualiser
    callModule(update_stack_bars, "language")
    callModule(update_word_bar, "language")

    # Activity Language Visualisations
    callModule(plot_avg_words, "activity", data = data[[3]])
    callModule(plot_word_level_scatter, "activity", data = data[[3]])

    # User Page - update selector
    # callModule(update_user_selector, "user", data = data[[3]])

    # User Page - Visualisers
    callModule(plot_user_language_stats, "user", data = data[[4]])
    callModule(plot_user_activity_stats, "user", data = data[[4]])

    # User Page Update, update the selector first (Default - have all users selected)
    callModule(update_user_selector, "user", data = data[[4]])
    callModule(update_user_stats, "user", data = data[[4]])

    # Success
    showUINotification("Robot logs visualised!")
  }
}

