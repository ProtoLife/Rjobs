source("errorTypes.R")
source("asyncTask.R")
source("InitFunctions.R")
source("BoundExperimentDefinition.R")


pdtStateVars <<- c("demoflag"
  , "gen"
  , "pdtState"
  , "curExp"
  , "curDef"
  , "curExtra"
  , "cost"
  , "popSize"
  , "repeats"
  , "Nexp"
  , "totalVolume"
  , "curTask"
  , "curInput"
)


createPDTstate <- function(isdemo = FALSE) {
  stateFile <<- "PDTstate.RData"
  demoflag <<- isdemo
  gen <<- 0
  pdtState <<- 0
  curExp <<- NULL
  curDef <<- NULL
  cost <<- NULL
  curExtra <<- NULL
  if (isdemo) {
    popSize <<- 20
    repeats <<- 1
  } else {
    popSize <<- 30
    repeats <<- 2
  }
  Nexp <<- popSize * (1 + repeats)
  totalVolume <<-NULL
  curTask <<- NULL
  curInput <<- NULL
  savePDTstate()
  cat(file = stderr(), paste(stateFile, "created.\n"))
}


savePDTstate <- function() {
  save(file = stateFile, list = pdtStateVars, envir = .GlobalEnv, ascii = TRUE)
}


setPDTstate <- function(s, val) {
  if (!exists("stateFile"))
    createPDTstate()
  stopifnot(s %in% pdtStateVars)
  eval(parse(text = paste(s, " <<- val")))
  if (s != "curInput" && s != "curTask")
    cat(file = stderr(), paste("setPDTstate:", s, "set to", paste(val, collapse = " "), "\n"))
  savePDTstate()
}


loadPDTstate <- function() {
  if (!exists("stateFile"))
    createPDTstate()
  load(stateFile, envir = .GlobalEnv)
}


# ----- SaveDefinition
SetupSaveExperimentDefinitionTask <- function(requestId) {
  task <- AsyncTask$new(requestId)
  setPDTstate("curTask", task)

  task$set.phase("save_esd:setup")
  task$log.info("SetupSaveExperimentDefinitionTask")
  task
}


SaveExperimentDefinition <- function(requestId, expDef, popSize, numRepeat, totalVolume) {
  task <- SetupSaveExperimentDefinitionTask(requestId)
  task$start({
    task$set.phase("save_esd:validate")
    task$log.info("BoundExperimentDefinition")
    BoundExperimentDefinition(experimentDefinition = expDef,
      popSize = popSize, numRepeat = numRepeat, totalVolume = totalVolume)
  })
  task
}


# ----- SaveInitialExperiments
SetupSaveInitialExperimentsTask <- function(requestId, df) {
  task <- AsyncTask$new(requestId)
  setPDTstate("curTask", task)

  task$set.phase("gen_one:setup")
  task$log.info("SetupSaveInitialExperimentsTask")
  extracsv = paste0("ExtraExperiments", 0, ".csv")
  write.csv(df, file = extracsv, na = "", row.names = FALSE)
  task
}


AutoIncorporateExperiments <- function() {
  curTask$log.info("AutoIncorporateExperiments")
}


AutoSnapshot <- function() {
  curTask$log.info("AutoSnapshot")
}


AutoUpdateCurrentGen <- function() {
  curTask$log.info("AutoUpdateCurrentGen")
}


AutoSampleNextGen <- function() {
  curTask$log.info("AutoSampleNextGen")
  for (i in 1:10) {
    curTask$log.trace(sprintf("Sampling %d%%", i * 10))
  }
  mtcars
}


SaveInitialExperiments <- function(requestId, df) {
  task <- SetupSaveInitialExperimentsTask(requestId, df)
  task$start({
    task$set.phase("gen_one:auto_incorporate")
    AutoIncorporateExperiments()
    task$set.phase("gen_one:first_snapshot")
    AutoSnapshot()
    task$set.phase("gen_one:update_current_gen")
    AutoUpdateCurrentGen()
    task$set.phase("gen_one:second_snapshot")
    AutoSnapshot()
    task$set.phase("gen_one:sample_next_gen")
    AutoSampleNextGen()
  })
  task
}


# Read CSV file (no header), then create column header
LoadESD <- function(file) {
  esd <- read.csv(file = file)
  colnames(esd) <- c("Name", lapply(1:(NCOL(esd)-1), function(i) { paste0("Value.", i) }))
  esd
}


# Testing code. Load ESD and set up two validation tasks
esd <- LoadESD("esd-9x5.csv")
t1 <- SaveExperimentDefinition("D.1001", esd, 32, 2, NULL)
t2 <- SaveExperimentDefinition("D.1002", esd, 5, 1, NULL)


# After some time has passed call this:
# what <- tryCatch(t2$result(), experiment_definition_validation_error = function(e) e)
# <experiment_definition_validation_error in experiment_definition_validation_error(errorMessage):
# Invalid experiment definition: The population size is less than the minimum allowed of 10
# for this experimental space.>
