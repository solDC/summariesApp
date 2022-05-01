library(log4r)

# Log file setup
logDir <- "summariesApp/log/"
logPath <- paste0(logDir,"summariesAppLog.txt")
my_logfile <- file(logPath)

my_console_appender <- console_appender(layout = default_log_layout())
my_file_appender <- file_appender(my_logfile, append = TRUE, 
                                  layout = default_log_layout())

my_logger <- log4r::logger(threshold = "INFO", 
                           appenders= list(my_console_appender,my_file_appender))

log4r_info <- function(info_msg) {
  log4r::info(my_logger, info_msg)
}

log4r_error <- function(err_msg) {
  log4r::error(my_logger,err_msg)
}

log4r_debug <- function(debug_msg) {
  log4r::debug(my_logger, debug_msg)
}