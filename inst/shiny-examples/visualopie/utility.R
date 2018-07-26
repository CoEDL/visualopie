## utility.R ## helper functions

# PATH <- "~/OPIE/logs/states"
# PATH <- "C://Users//Aninda Saha//Google Drive//University of Queensland//Winter Research 2018//Winter Projects 2018//Opie logs//amy"

time_continuity <- function(logs) {
  #' A utility function that accounts for smooth time flow in combined logs from different sessions.
  #' 
  #' @param logs - a list of logs from the different files 
  #' @return same list of logs is returned but with continuous time flow, as if the sessions were consecutively played.
  
  # Warning signs turned off
  options(warn = -1)
  
  for(df in 1:length(logs)) {
    setnames(logs[[df]], c("time","state","num_children","score","notes"))
    logs[[df]] <- logs[[df]]%>%
      mutate(time=(time-time[1])/1000)
  }
  
  if (length(logs) > 1) {
    for(num in 2:length(logs)) {
      last_val <- tail(logs[[num-1]][,1], n = 1L)
      for (row in 1:length(logs[[num]]$time)) {
        logs[[num]] <- logs[[num]]%>%
          mutate(time=time+last_val)
      }
    }
  }

  
  return(logs)
}

search_phrase <- function(phrase, log, id = 0) {
  #' Search log file for a particular phrase, storing indexes of line in a vector.
  #' 
  #' @param phrase - the phrase to search & match in a log file.
  #' @param log - a vector of lines from a log.
  #' @return vector of lines of matched phrase
  
  if (id == 0) {
    return( grep(phrase, log, value = TRUE))
  } else {
    return( grep(phrase, log, value = FALSE))
  }
  
}

showUINotification <- function(message, msgType = "message") {
  #' UI Notification system using shiny functions. Sends ID to global for manual removal of notifications.
  #' Otherwise, notifications will remove themselves after a few seconds.
  #' 
  #' Default message type is a standard 'message', shows a blue notification on lower right corner of app.
  #' 
  #' @param message - the message the notification will display.
  #' @param msgType - the type of message: "default" (Grey), "message" (Blue), "warning" (Yellow), "error" (Red)
  messageID <- NULL
  messageID <<- showNotification(message, type = msgType)
}
