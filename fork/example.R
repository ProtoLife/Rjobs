

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
  save(file = stateFile, list = pdtStateVars, envir = .GlobalEnv) # ascii = FALSE
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

# -----
SetupSaveInitialExperimentsTask <- function(df, requestId) {
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

SaveInitialExperiments <- function(df, requestId) {
  task <- SetupSaveInitialExperimentsTask(df, requestId)
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

t <- SaveInitialExperiments(mtcars, "X.1000")
t$result()
t$last.progress()
