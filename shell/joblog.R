library(stringr)
library(futile.logger)


# Utility to scan job log lines
# Log line is of the format "LEVEL [...] msg"
# Returns the msg part of the last LEVEL line in the log file
last.level.msg <- function(con, level = "TRACE") {
  line <- readLines(con)
  prefix <- paste0(level, " [")
  prefix.len <- str_length(prefix)
  foundset <- line[which(str_sub(line, 1, prefix.len) == prefix)]
  prefix.pat <- paste0("^", level, ".+\\]\\s*")
  gsub(prefix.pat, "", foundset[length(foundset)])
}


# Read the last TRACE line in the job log
get.progress <- function(con) {
  last.level.msg(con, "TRACE")
}


# Read the last DEBUG line in the job log
get.status <- function(con) {
  last.level.msg(con, "DEBUG")
}


# Write the status.json file with changed job information
job.update <- function(job, status = NULL, result = NULL, error_code = NULL, error_desc = NULL) {
  if (!is.null(result)) {
    job$status <- "completed"
    job$ended_at <- Sys.time()
    job$active <- FALSE
    job$result <- result
  } else if (!is.null(error_code)) {
    job$status <- "failed"
    job$ended_at <- Sys.time()
    job$active <- FALSE
    job$error_code <- error_code
    job$error_desc <- error_desc
  } else if (!is.null(status)) {
    job$status <- status
  }

  json <- jsonlite::toJSON(list(job = job), auto_unbox = TRUE, pretty = TRUE)
  write(json, file = file.path(job$id, "status.json"))
  job
}


# Use TRACE log for progress
job.progress <- function(job, msg, ..., capture = FALSE) {
  flog.trace(msg, ..., name = job$id, capture = capture)
  job
}


# Use DEBUG log for status changes
# Calling job.status also updates the json log with a new status type
job.status <- function(job, msg, ..., capture = FALSE) {
  updated <- job.update(job, status = msg, ...)
  flog.debug(msg, ..., name = job$id, capture = capture)
  updated
}


# Use ERROR log for error messages
job.error <- function(job, msg, ..., capture = FALSE) {
  flog.error(msg, ..., name = job$id, capture = capture)
  job
}


# Use INFO log for standard messages
job.info <- function(job, msg, ..., capture = FALSE) {
  flog.info(msg, ..., name = job$id, capture = capture)
  job
}


# Custom futile.logger layout that prints out job.id and pid
layout.job <- function(job.id, datetime.fmt="%Y-%m-%d %H:%M:%S")
{
  .where = -3 # get name of the function 3 deep in the call stack
              # that is, the function that has called flog.*
  function(level, msg, ...) {
    if (! is.null(substitute(...))) msg <- sprintf(msg, ...)
    the.level <- names(level)
    the.time <- format(Sys.time(), datetime.fmt)
    the.pid <- Sys.getpid()
    sprintf("%s [%s JOB=%s PID=%s] %s\n", names(level), the.time, job.id, the.pid, msg)
  }
}


# Create an S3 pdtjob object and start logging
job.create <- function(job.id, desc = "", params = list()) {
  # Set up log files in new subdirectory
  dir.create(job.id, showWarnings = FALSE)
  log.file <- file.path(job.id, "job.log")
  flog.appender(appender.file(log.file), name = job.id)
  flog.threshold(TRACE, name = job.id)
  flog.layout(layout.job(job.id), name = job.id)

  # Create the S3 object
  new.job <- list(id = job.id,
    active = TRUE,
    pid = 0,
    desc = desc,
    params = params,
    started_at = Sys.time(),
    status = "setup"
    )
  class(new.job) <- append(class(new.job), "pdtjob")

  # And write the json file and start logging, returning the job object
  job.status(new.job, "starting")
}
