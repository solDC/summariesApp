saveData <- function(error) {
  error <- as.data.frame(t(data))
  if (exists("data")) {
    data <<- rbind(responses, error)
  } else {
    data <<- error
  }
}

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")



