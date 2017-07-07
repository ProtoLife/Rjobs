# requires sys package from CRAN

# Example usage - start a job
# source('jobmgr.R')
# pid <- backgroundJob("jobs", "X.31415", "./AutoSampleNextGen.sh", "60")

# Kill running job by jobid:
# killJob("jobs", "X.31415")

# Also can kill job by pid returned by backgroundJob:
# killPid(pid)

# Get job status - example when script has error (not executable, etc)
# pid <- backgroundJob("jobs", "X.31415", "./AutoSampleNextG.sh", "60")
# job <- pollJob("jobs", "X.31415")
# job
# $pid
# [1] 34855
#
# $stat
# [1] "done"
#
# $log
# [1] "starting"
# [2] "jobs/X.9 ./AutoSampleNextG.sh 50"
# [3] "done"
#
# $err
# character(0)
#
# $exit
# [1] 127
#
# $elapsed
# [1] 0

# ---------------------------- PUBLIC FUNCTIONS


backgroundJob <- function(resd, jobid, cmd, args = character()) {
    # Run a command script that conforms to PDT job specification
    # Arguments:
    #   resd: directory used to store all job stats
    #   jobid: unique name for job. A directory with this name will be created
    #     under resd.
    #   cmd: full path to script to run in the background
    #   args: vector of character arguments to be passed to the script.
    #     Note that the jobdir will be passed to the script as the first argument.
    # Returns:
    #   pid of the job wrapper script "execjob.sh"
    jobdir <- file.path(path.expand(resd), jobid)
    dir.create(jobdir, recursive = TRUE, showWarnings = FALSE)
    logfile <- file.path(jobdir, "log")
    errfile <- file.path(jobdir, "err")
    args <- c(jobdir, cmd, args)
    pid <- sys::exec_background("./execjob.sh", args = args,
        std_out = logfile, std_err = errfile)
    lastjobfile <- file.path(resd, "last")
    cat(jobid, file = lastjobfile)
    pid
}

pollJob <- function(resd, jobid) {
    # Get the current status of a previously submitted job
    # Arguments:
    #   resd: directory used to store all job stats
    #   jobid: unique name for job.
    # Returns an R list with these components:
    #   $pid: pid of the wrapper script
    #   $stat: 'running' or 'done'
    #   $log: character vector of last 10 lines of script's STDOUT
    #   $err: character vector of last 10 lines of script's STDERR
    #   $elapsed: time since job started, in seconds
    #   $exit: exit value of subscript (0, 1, etc) or 129 if killed
    jobdir <- file.path(path.expand(resd), jobid)

    pidfile <- file.path(jobdir, "pid")
    pid <- readInt(pidfile)

    statfile <- file.path(jobdir, "stat")
    stat <- readText(statfile)

    logfile <- file.path(jobdir, "log")
    log <- readTail(logfile, 10L)

    errfile <- file.path(jobdir, "err")
    err <- readTail(errfile, 10L)

    l <- list(pid = pid, stat = stat, log = log, err = err)

    exitfile <- file.path(jobdir, "exit")
    if (file.exists(exitfile)) {
        l$exit <- readInt(exitfile)
    }

    startfile <- file.path(jobdir, "start")
    if (file.exists(startfile)) {
        start <- readInt(startfile)
        endfile <- file.path(jobdir, "end")
        if (file.exists(endfile)) {
            end <- readInt(endfile)
        } else {
            end <- as.integer(Sys.time())
        }
        l$elapsed <- end - start
    }
    l
}

killJob <- function(resd, jobid) {
    # Kill a previously submitted job
    # Arguments:
    #   resd: directory used to store all job stats
    #   jobid: unique name for job.
    # Returns nothing
    jobdir <- file.path(path.expand(resd), jobid)
    pidfile <- file.path(jobdir, "pid")
    pid <- readInt(pidfile)
    killPid(pid)
}

removeJob <- function(resd, jobid) {
    # Delete all the files created by a previously submitted job
    # Arguments:
    #   resd: directory used to store all job stats
    #   jobid: unique name for job.
    # Returns nothing
    jobdir <- file.path(path.expand(resd), jobid)
    unlink(jobdir, recursive = TRUE, force = TRUE)
}

removeCompletedJobs <- function(resd) {
    # Delete all the files for jobs that have completed or killed
    # Arguments:
    #   resd: directory used to store all job stats
    # Returns nothing
    jobs <- list.files(resd, full.names = FALSE, no.. = TRUE)
    for (jobid in jobs) {
        jobdir <- file.path(path.expand(resd), jobid)
        if (file.info(jobdir)$isdir) {
            statfile <- file.path(jobdir, "stat")
            stat <- readText(statfile)
            cat(paste(statfile, "is", stat, "\n"))
            if (stat == "done") {
                unlink(jobdir, recursive = TRUE, force = TRUE)
            }
        }
    }
}

# ---------------------------- PRIVATE FUNCTIONS

readInt <- function(fname) {
    if (file.exists(fname)) {
        lines <- readLines(fname, n = 1, ok = TRUE)
        if (length(lines) > 0) {
            return(as.integer(lines[[1]]))
        }
    }
    NULL
}

readText <- function(fname) {
    if (file.exists(fname)) {
        lines <- readLines(fname, n = 1, ok = TRUE)
        if (length(lines) > 0) {
            return(as.character(lines[[1]]))
        }
    }
    NULL
}

readTail <- function(fname, max) {
    if (file.exists(fname)) {
        return(tail(readLines(fname, ok = TRUE), max))
    }
    rv <- character()
    rv
}

getDescendantPids <- function(pid) {
    descendants <- character()
    if (!is.null(pid)) {
        descendants <- system(paste("./descendantpids.sh ", pid), intern = TRUE)
        descendants <- rev(descendants)
    }
    descendants
}

killPid <- function(pid) {
    descendants <- getDescendantPids(pid)
    if (length(descendants) > 0) {
        tools::pskill(descendants) # kill in reverse order
    }
    if (!is.null(pid)) {
        tools::pskill(pid)
    }
}
