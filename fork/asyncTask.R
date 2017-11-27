

library(R6)
library(parallel)
library(stringr)
library(tools)
library(futile.logger)

log.format <- function(level, msg, ...) {
  args <- list(...)
  the.level <- names(level)
  the.time <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  if (!is.null(args[["phase"]]) && args$phase != "") {
    return(paste0(the.level, " [", the.time, " ", args$phase, "] ", msg, "\n"))
  } else {
    return(paste0(the.level, " [", the.time, "] ", msg, "\n"))
  }
}

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


# Create a long-running task, executed in a forked process. (Doesn't work on Windows)
#
# The return value is a promise-like object with three
# methods:
# - completed(): FALSE initially, then TRUE if the task succeeds,
#   fails, or is cancelled. Reactive, so when the state changes
#   any reactive readers will invalidate.
# - result(): Use this to get the return value. While execution is
#   in progress, performs a req(FALSE). If task succeeded, returns
#   the return value. If failed, throws error. Reactive, so when
#   the state changes any reactive readers will invalidate.
# - cancel(): Call this to prematurely terminate the task.
AsyncTask <- R6Class("AsyncTask",
  private = list(
    state = factor("new",
      levels = c("new", "running", "success", "error", "cancel"),
      ordered = TRUE
    ),
    name = NULL,
    log = NULL,
    phase = "",
    handle = NULL,
    res = NULL,
    check = function() {
      if (!is.null(private$handle)) {
        res <- parallel::mccollect(private$handle, wait = FALSE)
        if (!is.null(res)) {
          if (!is.list(res) || length(res) != 1 || !inherits(res[[1]], "try-error")) {
            self$log.debug(paste0("task ", private$name, " succeeded"))
            private$handle <- NULL
            private$state <- "success"
            private$res <- res[[1]]
          } else {
            self$log.debug(paste0("task ", private$name, " failed"))
            private$handle <- NULL
            private$state <- "error"
            private$res <- attr(res[[1]], "condition", exact = TRUE)
          }
        }
      }
    }
  ),

  public = list(
    initialize = function(name) {
      private$name <- name
      private$log <- paste0(name, ".log")
      flog.logger(private$name, TRACE, appender = appender.file(private$log))
      flog.layout(log.format, name = private$name)
    },
    finalize = function() {
      if (private$state == "running") {
        self$cancel()
      }
    },
    # Launch the task in a forked process. This always returns
    # immediately, and we get back a handle we can use to monitor
    # or kill the job.
    start = function(expr) {
      if (private$state == "new") {
        self$log.debug(paste0("task ", private$name, " starting"))
        private$state <- "running"
        private$handle <- parallel::mcparallel({
          force(expr)
        })
        private$check()
      }
      invisible(private$state)
    },
    set.phase = function(ph) {
      private$phase <- ph
      invisible()
    },
    get.phase = function() {
      private$phase
    },
    log.trace = function(msg) {
      flog.trace(msg, phase = private$phase, name = private$name)
    },
    log.debug = function(msg) {
      flog.debug(msg, phase = private$phase, name = private$name)
    },
    log.info = function(msg) {
      flog.info(msg, phase = private$phase, name = private$name)
    },
    log.error = function(msg) {
      flog.error(msg, phase = private$phase, name = private$name)
    },
    # Read the last TRACE line in the job log
    last.progress = function() {
      con <- file(private$log)
      line <- last.level.msg(con, "TRACE")
      close(con)
      line
    },
    status = function() {
      if (private$state == "running") {
        private$check()
      }
      private$state
    },
    completed = function() {
      if (private$state == "running") {
        private$check()
      }
      private$state == "success" || private$state == "error" || private$state == "cancel"
    },
    failed = function() {
      if (private$state == "running") {
        private$check()
      }
      private$state == "error" || private$state == "cancel"
    }
    result = function() {
      if (private$state == "running") {
        private$check()
        if (private$state == "running") {
          return(NULL)
        }
      }
      if (private$state == "success") {
        return(private$res)
      } else if (private$state == "error") {
        stop(private$res)
      } else if (private$state == "cancel") {
        stop("The operation was cancelled")
      }
    },
    cancel = function() {
      if (private$state == "running") {
        private$check()
        if (private$state == "running" && !is.null(private$handle)) {
          private$state <- "cancel"
          self$log.debug(paste0("task ", private$name, " canceling"))
          tools::pskill(private$handle$pid, tools::SIGTERM)
          tools::pskill(-private$handle$pid, tools::SIGTERM)
          parallel::mccollect(private$handle, wait = FALSE)
          private$handle <- NULL
          self$log.debug(paste0("task ", private$name, " canceled"))
        }
      }
      invisible(private$state)
    }
  )
)
