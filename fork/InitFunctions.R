###-------- Environment-handling functions -----------###

AddKeyValuePairToEnv = function(env, key, value=1) {
		# Adds a key (variable) to an environment along with a value (object)
		# 	If the key is already present in the environment, then it only
		# 	updates its value
		# env is an environment
		# key is a character string, typically a the result of MakeHashKey(experiment),
		#	with experiment being a numerical vector
		# value can be any non-NULL object (including a named list of multiple objects)
		# usually, it's 'experiment'

		env[[key]] = value
		return(env)
}

AddElemToKeyValuePair = function(env, key, elemName, elemValue){
	# Adds an element to a list-type value for a given key in a given environment
	# env is an environment
	# key is a character string,  typically a the result of MakeHashKey(experiment),
	#	with experiment being a numerical vector
	# elemName is a character string, containing the name of the element to add
	# elemValue is any object corresponding to the value of elemName
	env[[key]][[elemName]]=elemValue
	return(env)
}

NewEnv = function(parent = emptyenv(), size = 0L, ...) {
	#Defines a hashed environment
	#parent is the parent environment
	#size is the starting size of the environment
	out = new.env(parent = parent, size = size, hash=T, ...)
	return(out)
}

EnvOrListToMat = function(env) {
	# converts an environment or list, each element of each is a numerical vector
	# 	of the same length, into a matrix, by row-binding all elements
	# env is an environment or a list
	mat = matrix(unlist(as.list(env), recursive=F, use.names=F), nrow=length(env), byrow=T)
	return(mat)
}

EnvOrListToVect = function(env) {
	# converts an environment or list whose elements are all scalars
	# 	into a vector
	# env is an environment or a list
	vect = unlist(as.list(env),recursive=F, use.names=F)
	return(vect)
}

EnvUnion = function(envList) {
	# Returns the union of a list of environments
	# envList: a list such that envList[[1]] = environment1, envList[[2]] = environment2, ...
	# This function assumes that if environmentI and environmentJ have a key in common,
	#	its value will be the same in both environments (otherwise, the last
	# 	environment in ... will be the one that provides the key to out)

	stopifnot(is.list(envList))
	out = list()
	for(i in envList){
		stopifnot(is.environment(i))
		out = c(out, as.list(i))
	}
	out = List2HashedEnv(out)
}


MakeHashTable = function(hashVect, saveAsList = F){
	# Creates a hash-table environment with keys taken from a vector of experiment hash representations
	# and fixed value 1
	# hashVect: a character vector, each element of which corresponding to the hash
	#	representation of a different experiment
	elementNum = length(hashVect)
	hashTable = as.list(rep("x",elementNum))
	names(hashTable) = hashVect
	if(!saveAsList)
		hashTable = List2HashedEnv(hashTable)
	return(hashTable)
}

List2HashedEnv = function(list){
	# Converts a list into a hashed environment
	return(list2env(list, hash=T, parent = emptyenv()))
}

SaveExpAndSpaceEnv = function(envSavedAsList, experimentsEnvFile, spaceEnvFile){
	# Saves expEnv and, if existing in the workspace, spaceEnv
	# envSavedAsList: logical, set to T if expEnv/spaceEnv should be converted to lists
	#	prior to saving (saving lists is faster than saving environments)
	# experimentsEnvFile: character, corresponding to the name of the file to save expEnv to
	# spaceEnvFile: character, corresponding to the name of the file to save spaceEnv to

	if(envSavedAsList)
		expEnv$hashTable = as.list(expEnv$hashTable)
	save(expEnv, file=experimentsEnvFile, compress=F)
	if(exists("spaceEnv")) {
            if(expEnv$exhaustive)
                stopifnot(NROW(spaceEnv$space)== expEnv$numRemainingExperiments)

            if(envSavedAsList)
                spaceEnv$hashTable = as.list(spaceEnv$hashTable)

            save(spaceEnv, file=spaceEnvFile,  compress=F)
	}
}


CreateTempExpEnvFromSpaceDef = function(spaceDef, paramFilesPathsFile= "ParamsFilesPaths.R"){
	# Creates a 'fake' expEnv environment with essential variables based on a given space definition
	# spaceDef: a (possibly named) list, corresponding to an experimental space definition (one element
	#	for each experimental variable, corresponding to the vector of possible values for that variable)
	stopifnot(is.list(spaceDef))
	stopifnot(all(sapply(spaceDef, is.numeric)))
	stopifnot(file.exists(paramFilesPathsFile))
	expEnv=new.env() # for some reason, it doesn't work with NewEnv...
	source(paramFilesPathsFile, expEnv)
	theoMin = as.numeric(sapply(spaceDef, min))
	theoMax = as.numeric(sapply(spaceDef, max))
	varNum = length(spaceDef)
	expEnv$theoMin = theoMin
	expEnv$theoMax = theoMax
	expEnv$varNum = varNum
	expEnv$spaceDef = spaceDef
	return(expEnv)
}

#-----------------------------------------------------###


###------------Misc functions-----------------------###


LargeVariablesSize = function(numTotalPoints, varNum, meanNcharHash, integerX = FALSE){
	# Returns an estimate of the overall size in MB of a matrix of experiments, its response vector,
	# its hash-representation vector, and its hash table
	# numTotalPoints: integer, the number of experiments in the matrix
	# varNum: integer, the number of experimental variables
	# meanNcharHash: the average number of characters making up experiment hash representations
	# integerX: set to TRUE if all experiments have integer values (e.g. mixtures) and FALSE otherwise
	requiredRamSpaceX = numTotalPoints*varNum*ifelse(integerX, 4, 8) #about 8 bytes per float and 4 per integer
	requiredRamSpaceY = numTotalPoints*8
	requiredRamSpaceXY = requiredRamSpaceX + requiredRamSpaceY
	requiredRamHash = numTotalPoints*(meanNcharHash+varNum-1)*1 # about 1 byte per individual character # the extra colNum characters are separators
	requiredRamHashTable = requiredRamHash*1 + numTotalPoints*1 # for each element, about 1 byte per individual name character and 1 byte for the value "x"
	requiredRamMB = (requiredRamSpaceXY+requiredRamHash+requiredRamHashTable)/(1024^2)
	return(requiredRamMB)
}

MaxNumTotalPoints = function(varNum, meanNcharHash, maxRamMB, integerX = FALSE){
	# Inverse of LargeVariablesSize to calculate the number of points that will yield
	# 	experiment matrix, response vector, hash representation vector and hash table with a certain
	# 	overall size
	# varNum: integer, the number of experimental variables
	# meanNcharHash: numeric, the average number of characters making up experiment hash representations
	# maxRamMB: numeric, the targed overall variable size in MB
	# integerX: set to TRUE if all experiments have integer values (e.g. mixtures) and FALSE otherwise

	maxTotPoints = round(maxRamMB*1024^2/(ifelse(integerX, 4, 8)*varNum+8+2*meanNcharHash+1))
	return(maxTotPoints)
}

FixedCoveragePopSize = function(oneDCov, oneDSpaceSize, numD){
	# Assuming a space with numD dimensions, each of which with
	# 	oneDSpaceSize possible values, for a given targed coverage
	# 	of oneDCov/oneDSpaceSize of the space, returns the number of
	# 	points that should be (randomly) sampled in that space to
	# 	hit that target
	# Note 1: instead of providing oneDCov/oneDSpaceSize directly,
	# 	we provide oneDCov and oneDSpaceSize, where oneDCov
	# 	should be an integer between 1 and oneDSpaceSize. oneDCov
	#	can be seen as the target number of distinct points, projected onto
	#	any given individual dimension; the constraint we inpose on
	#	oneDCov allows us to calculate an exact (integer) number of
	#	points to sample.
	# Note 2: q is obtained by solving for q:
	#	oneDCov^q / oneDSpaceSize^numD = oneDCov/oneDSpaceSize
	#	where oneDCov^q is target number of distinct points in numD dimensions
	# 	(formulated as a power of the target number of distinct points in 1
	#	dimension), oneDSpaceSize^numD is the space size in numD dimensions and
	#	oneDCov/oneDSpaceSize is the target coverage

	if(!oneDCov%in%(1:oneDSpaceSize))
		stop("oneDCov should be an integer between 1 and oneDSpaceSize\n")

	q = (numD-1)*log(oneDSpaceSize)/log(oneDCov)+1
	res = round(oneDCov^q) # numerical errors could make this non integer
							# (with symbolic calculations it will always be an integer)
	return(res)
}

SpaceSize = function(spaceDef, totUnitNum=NULL, ...) {
	# Calculates space size based on spaceDef list
	# totUnitNum: total number of units if mixture space, else NULL

	if(is.null(totUnitNum)){
		spaceSize = prod(mapply(length, spaceDef))
	} else{
		spaceSize = MixtureSpaceSize(mapply(min, spaceDef), mapply(max, spaceDef), totUnitNum)
	}
	return(spaceSize)
}

MixtureSpaceSize = function(lowerBoundVect, upperBoundVect, totUnitNum){
	# Calculates the size of a mixture space with total number of units
	# equal to totUnitNum, and parameters bounded below by integer
	# vector lowerBoundVect and above by integer vector upperBoundVect

	# Basic idea:
	# a) Let N(p, m) be the number of possible experiments along parameters
	#	X_1, X_2, ..., X_p such that X_1 + X_2 + ... + X_p = m, with m equal to the
	#	total number of units
	# b) Let N(1, m'), m' = 1, 2, ..., m, is obvious: set equal to 1 if is m' a
	#	possible value of X_1, and 0 otherwise
	# c) Assuming N(p', m'), m' = 1, 2, ..., m have been calculated for some p'>=1,
	# 	let N(p'+1, m') = sum( { N(p', m'- v): l_{p'+1} <= v <= u_{p'+1} } )
	# 	where l_{p'+1} and u_{p'+1} are the lower and upper bound of X_{p'+1}
	# 	-- We are summing, over all values of m', the number of experiment projections
	#	in dimension p' that allow us to fulfill the mixture constraint in dimension p'+1
	# Note that N(., ..)=0 if (..) <0

	stopifnot(all(lowerBoundVect%%1==0))
	stopifnot(all(upperBoundVect%%1==0))
	stopifnot(totUnitNum%%1==0)
	stopifnot(all(lowerBoundVect<=upperBoundVect))
	varNum = length(lowerBoundVect)
	stopifnot(varNum==length(upperBoundVect))
	minLowerBound = min(lowerBoundVect)
	valVect = 1:totUnitNum
	valNum = length(valVect)
	expNumMat = matrix(0, varNum, valNum*2+1)
	valZeroIdx = valNum+1
	highestValIdx = ncol(expNumMat)
	colnames(expNumMat) = -totUnitNum:totUnitNum

	valVectWithZero = 0:totUnitNum
	expNumMat[1, valZeroIdx-1+which(valVectWithZero>=lowerBoundVect[1] & valVectWithZero<=upperBoundVect[1])] = 1

	for(i in 2:varNum){
		currFeasValIdx = which(valVectWithZero>=lowerBoundVect[i] & valVectWithZero<=upperBoundVect[i])
		if(length(currFeasValIdx)>0){
			for(j in valZeroIdx:highestValIdx)
				expNumMat[i, j] = sum(expNumMat[i-1, j-(currFeasValIdx-1)])
		}
	}
	return(as.numeric(expNumMat[varNum, highestValIdx]))
}

AssignIfNotAssigned = function(variableName, variableValue){
	# assigns a value to a variable if that variable does not exist
	# in the global environment
	# variableName:  a character string
	# variableValue: anything

	if(!exists(variableName, where = .GlobalEnv)){
		cat("Default value assigned to", variableName, "\n")
		assign(variableName, variableValue, pos = .GlobalEnv)
	}
}

IsInMixture = function(experiment, expEnv){
	# Checks if an experiment respects the mixture constraint induced by expEnv$totalVolume or not
	# and returns TRUE or FALSE, respectively
	# experiment: a numeric vector
	# expEnv: the experiment environment, which is expected to contain a spaceDef element
	#	whose element [[i]] is expected to correspond to element [i] of experiment
	# totUnitNum: an integer, corresponding to the sum of mixture units in a valid experiment

	stopifnot(is.numeric(experiment))
	stopifnot(!is.null(expEnv$spaceDef))
	stopifnot(is.numeric(expEnv$totalVolume))
	stopifnot(expEnv$totalVolume%%1==0 & expEnv$totalVolume>0)

	if(any(experiment<0) | sum(experiment)!=expEnv$totalVolume | any(experiment%%1!=0)){
		inMixture = F
	} else{
		inMixture = T
	}
	return(inMixture)
}

IsInSpace = function(experiment, expEnv){
	# Checks if an experiment belongs to the experimental space or not
	# 	and returns TRUE or FALSE, respectively
	# experiment: a vector
	# expEnv: the experiment environment, which is expected to contain a spaceDef element
	#	whose element [[i]] is expected to correspond to element [i] of experiment

	stopifnot(!is.null(expEnv$spaceDef))
        stopifnot(length(experiment)==length(expEnv$spaceDef))

        inSpace = T
	for(i in 1:length(experiment)){
		if(!experiment[i]%in%expEnv$spaceDef[[i]]){
			inSpace = F
			break
		}
	}

	if(!is.null(expEnv$totalVolume))
		inSpace = IsInMixture(experiment, expEnv)

	return(inSpace)
}

Shuffle = function(data, orderIdx){
	# Rearranges the order of data elements according to a (typically randomly generated) index vector
	# Returns the rearranged data
	# data: a vector, matrix, data frame, or list
	# orderIdx: a vector of integers with size matching that of data
	stopifnot(is.numeric(orderIdx) & round(orderIdx)==orderIdx)

	numIdx = length(orderIdx)
	if(is.matrix(data) | is.data.frame(data)) {
		if(NROW(data)!=numIdx)
			stop("Number of rows in data does not match number of shuffled indices", call.=F)
		data = data[orderIdx,]
	} else if(is.vector(data)){
		if(length(data)!=numIdx)
			stop("Number of elements in data does not match number of shuffled indices", call.=F)
		data = data[orderIdx]
	} else if(is.list(data)){
		if(length(data)!=numIdx)
			stop("Number of elements in data does not match number of shuffled indices", call.=F)
		data = relist(unlist(data)[orderIdx], skeleton = data)
	} else if(is.null(data)){
		data = data
	} else{
		stop("Don't know how to shuffle data object", call.=F)
	}
	return(data)
}

UpdateWorkingVariables = function(expEnv, popSize, numRepeat){
	# Updates several variables of the experiment environment 'expEnv' with their most recent
	# values
    # expEnv: an experiment environment
    #popSize: an integer corresponding to the generation size
    #numRepeat: an integer corresponding to the number of repeats per generation
    stopifnot(is.environment(expEnv))
    stopifnot(is.numeric(popSize))
    stopifnot(popSize>0)
    stopifnot(is.numeric(numRepeat))
    stopifnot(numRepeat>=0)

	#if(NROW(expEnv$experiment)>0) {
		expEnv$individualExperimentsNum = NROW(expEnv$experiment)			#total number of individual experiments
		expEnv$individualResponsesNum = length(expEnv$response) # number of individual response measurements
		if(NROW(expEnv$experiment)>0){
			expEnv$lastAttemptedGen = max(expEnv$generation) # index of latest generation (that possibly contains experiments w/o response measurements)
			expEnv$lastAttemptedGenLogicalVect = expEnv$generation==expEnv$lastAttemptedGen  # logical vector with T iif experiment is from latest generation
			expEnv$notRepeat = !grepl(expEnv$repeatString, expEnv$type) # boolean vector, s.t. notRepeat[i] = T iif the i-th experiment is not the repeat of
																						# any of experiments 1, 2, ..., i-1 (calculating this AFTER shuffling)
			expEnv$inSpace = apply(expEnv$experiment, 1, IsInSpace, expEnv)
			expEnv$lastAttemptedGenHash = expEnv$hash[expEnv$lastAttemptedGenLogicalVect] # vector of experiment hash keys from latest attempted generation
			expEnv$lastAttemptedGenType = expEnv$type[expEnv$lastAttemptedGenLogicalVect] # vector of experiment types from latest attempted generation
			expEnv$individualNonRepeatExperimentsNum = sum(expEnv$notRepeat) # same as individualExperimentsNum, after removing repeats
			expEnv$lastAttemptedGenExperimentNum = sum(expEnv$lastAttemptedGenLogicalVect) # number of experiments from latest generation
			expEnv$numRemainingExperiments = expEnv$spaceSize - sum(expEnv$notRepeat & expEnv$inSpace) #number of still-untried experiments
																								# remaining in the space
			expEnv$lastAttemptedGenNotRepeat = expEnv$notRepeat[expEnv$lastAttemptedGenLogicalVect] # vector indicating not-repeats in latest attempted generation

		} else{
			expEnv$lastAttemptedGenLogicalVect = logical(0)
			expEnv$notRepeat = logical(0)
			expEnv$inSpace = logical(0)
			expEnv$lastAttemptedGenHash = character(0)
			expEnv$lastAttemptedGenType = character(0)
			expEnv$individualNonRepeatExperimentsNum = 0
			expEnv$lastAttemptedGenExperimentNum = 0
			expEnv$numRemainingExperiments = expEnv$spaceSize
			expEnv$lastAttemptedGenNotRepeat = logical(0)
		}
		expEnv$lastAttemptedGenExperiment = VectToMat(expEnv$experiment[expEnv$lastAttemptedGenLogicalVect, ]) # matrix of latest attempted generation's experiments (used by measures)
											# this stays out of the if statements above, because even if there are no experiments, we still want an empty array with varNum columns

		if(expEnv$individualResponsesNum>0){
			expEnv$lastCompletedGen = expEnv$generation[length(expEnv$response)] #index of latest completed generation (whose all experiments have
			 																# response measurements contains experiments w/ response measurements)
			expEnv$lastAttemptedGenRespNum = sum(!is.na(expEnv$lastAttemptedGenResponse))	 # number of latest attempted generation's response measurements that are not NAs
			expEnv$lastAttemptedGenResponse = expEnv$response[expEnv$lastAttemptedGenLogicalVect] # vector of latest  attempted generation's response measurements

		}	else{
			### expEnv$lastCompletedGen = NULL # not assigning this to NULL, because it's already assigned to -1; same for expEnv$lastAttemptedGen
			expEnv$lastAttemptedGenRespNum = 0
			expEnv$lastAttemptedGenResponse = numeric(0)
		}

		lastAttemptedGenString = paste("Gen", expEnv$lastAttemptedGen, sep="")
		if(is.null(expEnv$parameterByGen[[lastAttemptedGenString]]))
			expEnv$parameterByGen[[lastAttemptedGenString]] = list() #generation-indexed parameter list
		if(!is.null(expEnv$parameter))
			expEnv$parameterByGen[[lastAttemptedGenString]] = UpdateCoreInfo(expEnv$parameterByGen[[lastAttemptedGenString]], expEnv$parameter) # list of latest generation's parameter values

		#expEnv$hashTableNotCompletelyMissing = as.environment(as.list(expEnv$hashTable)) 		# hash table of experiments that have
		#rm(list=expEnv$hash[!notCompletelyMissing], pos=expEnv$hashTableNotCompletelyMissing)	# only missing response values
	#}
    expEnv$maxNumRemainingGen = BoundRemainingGenerations(expEnv, popSize, numRepeat)

	return(expEnv)
}

AnalyzeResponse = function(expEnv, RepeatAnalyzerFunction, genToAnalyze="last"){
	# Runs repeat analysis and updates $averageResponse and $scatterResponse elements of
	#	an experiment environment
	stopifnot(is.function(RepeatAnalyzerFunction))
	if(length(genToAnalyze)>1){
		stopifnot(is.numeric(genToAnalyze))
		stopifnot(genToAnalyze==round(genToAnalyze))
		stopifnot(expEnv$lastCompletedGen>=max(genToAnalyze)) # can't analyze generations that don't include response measurements
	} else {
		if(is.character(genToAnalyze)){
			stopifnot(genToAnalyze=="last")
			genToAnalyze = expEnv$lastCompletedGen
		} else{
			stopifnot(is.numeric(genToAnalyze))
			stopifnot(genToAnalyze==round(genToAnalyze))
			stopifnot(expEnv$lastCompletedGen>=genToAnalyze)
		}
	}

	#if(expEnv$lastCompletedGen<=max(genToAnalyze))
	#	warning("Generation ", expEnv$lastCompletedGen, " was previously analyzed.\n", call.=F)

	repeatHashTable = RepeatHashTable(expEnv, genToAnalyze)	# hashTable with info about
															# about repeats in generation 'genToAnalyze'
	updateRepeatAnalysisOut = UpdateRepeatAnalysis(expEnv, RepeatAnalyzerFunction, repeatHashTable)	# setup / updating of
	expEnv$averageResponse = updateRepeatAnalysisOut$averageResponse									# response average and
	expEnv$scatterResponse = updateRepeatAnalysisOut$scatterResponse									# scatter vectors
	expEnv$notCompletelyMissing = expEnv$averageResponse!=expEnv$acknowledgedMissingValue
				# boolean vector, s.t. notCompletelyMissing[i] = T iif the i-th experiment, or any one of its replicates,
				# has non-missing response measurement value

	return(expEnv)
}

ShuffleExperiments = function(expEnv) {
	stopifnot(is.character(expEnv$hash))
	stopifnot(is.character(expEnv$type))
	stopifnot(is.character(expEnv$extraExperimentTypeString))
	stopifnot(is.matrix(expEnv$experiment) | is.data.frame(expEnv$experiment))
	stopifnot(length(expEnv$type)==length(expEnv$hash))
	stopifnot(length(expEnv$type) == NROW(expEnv$experiment))
	stopifnot(is.numeric(expEnv$lastAttemptedGen))
	stopifnot(is.numeric(expEnv$lastCompletedGen))
	stopifnot(expEnv$lastCompletedGen<expEnv$lastAttemptedGen)
	stopifnot(is.numeric(expEnv$lastAttemptedGenExperimentNum))
	stopifnot(is.logical(expEnv$lastAttemptedGenLogicalVect))
	stopifnot(sum(expEnv$lastAttemptedGenLogicalVect)==expEnv$lastAttemptedGenExperimentNum)

	if(any(!is.na(expEnv$response[expEnv$lastAttemptedGenLogicalVect]))) {
		stop("Response measurements have already been updated in generation", expEnv$lastAttemptedGen,
			"so experiments should not be shuffled.\n")
	}

	shuffleIdx = sample.int(expEnv$lastAttemptedGenExperimentNum, expEnv$lastAttemptedGenExperimentNum) # experiment order shuffle index vector
	expEnv$hash[expEnv$lastAttemptedGenLogicalVect] = Shuffle(expEnv$hash[expEnv$lastAttemptedGenLogicalVect], shuffleIdx)
	expEnv$type[expEnv$lastAttemptedGenLogicalVect] = Shuffle(expEnv$type[expEnv$lastAttemptedGenLogicalVect], shuffleIdx)
	expEnv$experiment[expEnv$lastAttemptedGenLogicalVect, ] = Shuffle(expEnv$experiment[expEnv$lastAttemptedGenLogicalVect, ], shuffleIdx)

	return(expEnv)
}

SetUpAverageScatterResponse = function(expEnv){
	# Sets up response average and scatter vectors within expEnv. If already present
	# it adjusts their length in such a way that it matches the total number
	# of experiments present in expEnv, leaving their already present elements untouched

	stopifnot(length(expEnv$averageResponse) == length(expEnv$scatterResponse))
	stopifnot(is.numeric(expEnv$response))
	stopifnot(length(expEnv$response)>=length(expEnv$averageResponse))
	stopifnot(is.numeric(expEnv$individualResponsesNum))
	stopifnot(expEnv$individualResponsesNum==length(expEnv$response))

	if(is.null(expEnv$averageResponse)) {
		# defining response average and scatter vectors if not present
		expEnv$averageResponse = expEnv$response
		expEnv$scatterResponse = rep(0, length(expEnv$averageResponse))
	} else{
		# filling in undefined missing entries, corresponding to last attempted generation's
		# experiments, at the end of response average and scatter vectors
		stopifnot(is.numeric(expEnv$averageResponse))
		stopifnot(is.numeric(expEnv$scatterResponse))
		numAvgResp = length(expEnv$averageResponse)
		if(numAvgResp<expEnv$individualResponsesNum){
			expEnv$averageResponse = c(expEnv$averageResponse,
				expEnv$response[(numAvgResp+1):expEnv$individualResponsesNum])
			expEnv$scatterResponse = c(expEnv$scatterResponse,
				rep(0, expEnv$individualResponsesNum - numAvgResp))
		}
	}
	return(expEnv)
}

UpdateRepeatAnalysis = function(expEnv, Analyzer, repeatHashTable) {
	# Updates average and scatter vectors within experiment environment
	# by incorporating info from repeats whose hash representation is a key
	# in repeatHashTable
	# expEnv: an experiment environment
	# Analyzer: a function that takes expEnv and returns a list
	#	containing vectors $averageResponse and $scatterResponse as elements
	# repeatHashTable: a hash table of repeats, typically as returned by RepeatHashTable
	# It returns such list and does not assign $averageResponse and
	# 	$scatterResponse in expEnv directly, only to allow this assignment
	#	to be explictly visible within UpdateWorkingVariables, which calls
	#	this function
	stopifnot(is.function(Analyzer))
	stopifnot(is.environment(repeatHashTable))
	expEnv = SetUpAverageScatterResponse(expEnv)
	repeatAnalysis = Analyzer(expEnv, repeatHashTable)
	return(repeatAnalysis)
}

RepeatHashTable = function(expEnv, generationVect) {
	# Returns a hash table each key of which is the hash representation
	# of a different repeat of a given generation, or set of generations, and each
	# corresponding value is a list containing information on such repeat:
	#	- $isRepeat, a vector of logicals, with as many elements as the number
	# 	 of individual experiments in expEnv, each element being TRUE iif
	#	 the corresponding experiment is identical to the current repeat
	#	 (that includes the repeat itself)
	#	- $response: a vector of response values for all the experiments
	#	 for which $isRepeat is TRUE

	# expEnv: the experiment environment
	# generationVect: a numeric vector, each element of which is the index of a different
	# 	generation to apply the function to

	stopifnot(is.environment(expEnv))
	stopifnot(is.numeric(generationVect))
	stopifnot(round(generationVect)==generationVect)
	stopifnot(is.numeric(expEnv$generation))
	stopifnot(is.numeric(expEnv$response))
	stopifnot(is.character(expEnv$hash))
	stopifnot(is.numeric(expEnv$individualExperimentsNum))
	stopifnot(is.numeric(expEnv$lastCompletedGen))
	stopifnot(is.numeric(expEnv$lastAttemptedGen))
	stopifnot(expEnv$lastCompletedGen<=expEnv$lastAttemptedGen)
	numExpCorrectlyWithoutResponse = ifelse(expEnv$lastCompletedGen<expEnv$lastAttemptedGen,
		sum(expEnv$generation==expEnv$lastAttemptedGen), 0) #number of experiments without response value

	stopifnot(length(expEnv$response)+numExpCorrectlyWithoutResponse == length(expEnv$hash))
	stopifnot(length(expEnv$response)+numExpCorrectlyWithoutResponse == length(expEnv$generation))
	stopifnot(length(expEnv$response)+numExpCorrectlyWithoutResponse == expEnv$individualExperimentsNum)

	stopifnot(expEnv$lastCompletedGen>=max(generationVect))

	whichSelectedGen = expEnv$generation%in%generationVect
	selectedGenHashVect = expEnv$hash[whichSelectedGen]
	selectedGenTypeVect = expEnv$type[whichSelectedGen]

	selectedGenWhichIsRepeat = grep(expEnv$repeatString, selectedGenTypeVect)	# not excluding repeats with acknowledged-missing response values, because
																				# there may be experiments identical to them that do have a
																				# response value, which we want to consider when computing
																				# average response and response scatter
																				# Note that below repeats and experiments with repeats are
																				# treated the same way (we define this variable based on
																				# $repeatString, just to single out experiments
																				# that have at least one repeat)

	selectedGenRepeatExpHashVect = unique(selectedGenHashVect[selectedGenWhichIsRepeat]) # vector of *unique* hash keys of repeats in selected generation

	numUniqueCurrRepeats = length(selectedGenRepeatExpHashVect)
	selectedGenRepeatHashTable = NewEnv(size = as.integer(numUniqueCurrRepeats))

	hashVect = expEnv$hash
	if(expEnv$lastAttemptedGen>expEnv$lastCompletedGen)
		hashVect = hashVect[expEnv$generation<=expEnv$lastCompletedGen] # otherwise the loop below will add NA's to $response
	for(i in selectedGenRepeatExpHashVect){
		selectedGenRepeatHashTable[[i]]$isRepeat = hashVect==i
		selectedGenRepeatHashTable[[i]]$response = expEnv$response[selectedGenRepeatHashTable[[i]]$isRepeat] #this will include NA's if the
																										# current experiment happens to have a
																										# repeat at latestAttemptedGen and
																										# latestAttemptedGen is > latestCompletedGen
		#selectedGenRepeatHashTable[[i]]$num = sum(selectedGenRepeatHashTable[[i]]$isRepeat)
	}

	return(selectedGenRepeatHashTable)
}


GetParamList = function(PDTaction, ...){
	# Returns a list with one list element named <PDTaction>, with subelements corresponding
	# to the name and values of a set of parameters that are supposed to be pertinent to <PDTaction>,
	# which are assumed to exist in the Global environment
	# PDTaction: a character string, describing a certain PDT 'action' (e.g. "GenerationOne", "SmartRandomSampler")
	# ...: a string or strings corresponding to variable (typically, PDT parameter) names

	stopifnot(is.character(PDTaction))

	parList = list()
	parList[[PDTaction]] = list()

	for(i in c(...)){
		stopifnot(is.character(i))
		parList[[PDTaction]][[i]] =  eval(parse(text=i), envir=.GlobalEnv)
	}

	return(parList)
}

DataList = function(datX, datY) {
	# Returns a list of info on a matrix of input variable observations
	# and a vector of corresponding observations of an output variable
	# including mean, standard deviation of the former (standard deviation set to 1 if a
	#	variable is constant), range and zero-one normalization of the latter
	# datX: a matrix of input variable observations
	# datY: vector of output variable observations
	# note: if datX/datY is NULL, no information about x/y is added to the output

	dataList=list()

	if(!(is.null(datX))) {
		datX = VectToMat(datX)
		scaleMean = apply(datX, 2, InternalMean)
		scaleSd = apply(datX, 2, sd)
		scaleSd[scaleSd==0] = 1

		dataList$x=datX
		dataList$xMean=scaleMean
		dataList$xSd=scaleSd
		dataList$xScaled=datX
	}

	if(!(is.null(datY))){
		dataList$y=datY
		dataList$yRange=range(dataList$y)
		dataList$yNorm=NormalizeValues(datY)
	}

	return(dataList)
}

RemovePatternsFromString = function(patternVect, string) {
	# Removes character patterns from a character string
	# patternVect: a character vector of patterns to remove
	# string: the character string to remove the patterns from
	for(i in patternVect)
		string = gsub(i, "", string)
	return(string)
}

Which.max.tie = function (x) {
	# Same as which.max, but breaks ties randomly
	# Borrowed from the nnet package
	# x: vector of numeric values
	y <- seq_along(x)[x == max(x)]
	if (length(y) > 1L)
		sample(y, 1L)
	else y
}

Which.min.tie = function (x) {
	# Same as which.min, but breaks ties randomly
	# x: vector of numeric values
	y <- seq_along(x)[x == min(x)]
	if (length(y) > 1L)
		sample(y, 1L)
	else y
}

Am = function(dat) {
	dat = unname(as.matrix(dat))
	return(dat)
}  # just a shorcut

MakeHashKey = function(vect) {
	#bare-bone version of R's paste function, with sep set to "" and collapse set to ","
	#vect is a numerical vector (but the function also works if vect is a character vector)

	#vect <- list(c("R", vect))

	.Internal(paste(list(vect), "", ","))
}

LoopPercDone=function(i, totLoops, functionName, percThresh=10, percStep=10) {
	#Prints to video the percent of a "for" loop completed. i is the loop counter, totLoops is the total
	#number of loops, percThresh is the percent threshold that has to be passed to get the
	#function to print to screen. In percTresh is passed, percStep is added to it. Th output
	#should become the  percThresh input at the (i+1)th loop
	percDone=round(i/ totLoops*100)
	if(percDone>=percThresh & percDone%%percStep==0) {
		cat(paste(functionName, ":", sep=""), percDone, "% done.\n")
		percThresh=percThresh+ percStep
	}
	return(percThresh)
}

MakeHashKeyParallel= function(matOrDataFrame, parallelize, multiNode,	chunkNum, workingDir) {
	# A parallel wrapper for MakeHashKey, to be used when such function is to be applied to a
	# matrix/data-frame (e.g., containing the entire experimental space) rather than to an individual vector
	# matOrDataFrame: a matrix or data frame
	# parallelize: set to TRUE if the execution should be carried out in parallel
	# multiNode: relevant only if parallelize set to TRUE – set to TRUE if parallel
	#	computations are to involve multiple nodes
	# chunkNum: relevant only if parallelize set to TRUE – an integer corresponding to
	#	the number of chunks (row-subsets of the 'space' matrix) to divide the procedure into
	# workingDir: relevant only if parallelize set to TRUE and if multiNode set to TRUE –
	#	character string corresponding to the path to the PDT code root folder (should be
	#	the same on all nodes)


	if(!is.matrix(matOrDataFrame) & !is.data.frame(matOrDataFrame))
		stop("matOrDataFrame should be a matrix or data frame\n", call.=F)

	if(NROW(matOrDataFrame)==0)
		stop("matOrDataFrame has zero rows\n", call.=F)

	if(ncol(matOrDataFrame)==1)
		return(as.character(matOrDataFrame))

	numPts = nrow(matOrDataFrame)
	if(numPts<=chunkNum*2)
		parallelize = F # else apply would have a problem

	if(parallelize) {
		doMultiNode = expression({
			setwd(workingDir)
		    source("InitFunctions.R")
		})
		hashVect = foreach(i=isplitIndices(numPts, chunks= chunkNum), .combine=c) %dopar%  {
			 # strange how it looks like the whole matOrDataFrame is sent and received via network!
			if(multiNode)
				eval(doMultiNode)
			res = apply(matOrDataFrame[i,], 1, MakeHashKey)
			return(res)
		}

	} else {
		hashVect = apply(matOrDataFrame, 1, MakeHashKey)
	}
	names(hashVect) = NULL
	return(hashVect)
}

FindExperimentIndex = function(hashVect, spaceEnv, parallelize, multiNode,
	chunkNum, workingDir){
		# Finds row/element index of space/hash elements of space environment corresponding
		#	to experiments with a certain hash representation

		# hashVect: a vector each element of which is the hash representation of a different experiment
		# spaceEnv: a space environment, assumed to have $space, $hashTable, $hash elements
		# parallelize: set to TRUE if the execution should be carried out in parallel
		# multiNode: relevant only if parallelize set to TRUE – set to TRUE if parallel
		#	computations are to involve multiple nodes
		# chunkNum: relevant only if parallelize set to TRUE – an integer corresponding to
		#	the number of chunks (row-subsets of the 'space' matrix) to divide the procedure into
		# workingDir: relevant only if parallelize set to TRUE and if multiNode set to TRUE –
		#	character string corresponding to the path to the PDT code root folder (should be
		#	the same on all nodes)
		# Returns a vector of row/element indices, or NULL if no experiment in hashVect is contained
		# 	in spaceEnv

		stopifnot(is.character(hashVect))
		stopifnot(is.environment(spaceEnv))
		stopifnot(is.environment(spaceEnv$hashTable))
		stopifnot(is.character(spaceEnv$hash))
		stopifnot(is.matrix(spaceEnv$space) | is.data.frame(spaceEnv$space))
		stopifnot(is.logical(parallelize))
		stopifnot(is.logical(multiNode))
		stopifnot(is.numeric(chunkNum))
		stopifnot(is.character(workingDir))

		numExpToRemove = length(hashVect)
		stopifnot(numExpToRemove==length(unique(hashVect)))

		if(parallelize) {
			doMultiNode = expression({
				setwd(workingDir)
			    source("InitFunctions.R")
			})
			inSpaceIdx = foreach(i=isplitIndices(numExpToRemove, chunks= chunkNum), .combine=c) %dopar%  {
				if(multiNode)
					eval(doMultiNode)

				isInSpace = !is.null(spaceEnv$hashTable[[hashVect[i]]]) # IS THIS GONNA WORK?
				if(isInSpace){
					res = which(spaceEnv$hash == hashVect[i])
				} else {
					res = NULL
				}
				return(res)
			}
		} else {
			inSpaceIdx = NULL
			for(i in 1:numExpToRemove){
				isInSpace = !is.null(spaceEnv$hashTable[[hashVect[i]]])
				if(isInSpace)
					inSpaceIdx = c(inSpaceIdx, which(spaceEnv$hash == hashVect[i]))
			}
		}
		names(inSpaceIdx) = NULL

		return(inSpaceIdx)
}

RemoveExperimentFromSpaceEnv = function(spaceEnv, indexVect) {
	# Removes rows/elements of $space, $hashTable, $hash components of spaceEnv
	#	corresponding to experiments indexed by indexVect
	# spaceEnv: a space environment
	# indexVect: a vector of integer indices, which should be aligned with the row-indices
	#	of spaceEnv$space

	stopifnot(is.environment(spaceEnv$hashTable))
	stopifnot(is.character(spaceEnv$hash))
	stopifnot(is.matrix(spaceEnv$space) | is.data.frame(spaceEnv$space))
	stopifnot(is.numeric(indexVect))
	stopifnot(round(indexVect)==indexVect)
	stopifnot(length(indexVect)==length(unique(indexVect)))

	toRemoveHashVect = spaceEnv$hash[indexVect]
	spaceEnv$space = VectToMat(spaceEnv$space[-indexVect, ])
	spaceEnv$hash = spaceEnv$hash[-indexVect]
	rm(list=toRemoveHashVect, pos = spaceEnv$hashTable)
	return(spaceEnv)
}

put = function(...) {
	cat(paste(...))
}

VectToMat=function(dat, byRow=T) {
	if(is.vector(dat) & !is.data.frame(dat)){
		output=as.matrix(dat)
		if(byRow)
			output=t(output)
	} else {
		output=dat
	}
	return(output)
}

InternalMean = function(vect) {
	#wrapper to R's internal mean function
	.Internal(mean(vect))
}

Power = function(vect, exponent=8) {
	vect^exponent
}

NoNASd = function(vect){
	# wrapper to R's sd function, which returns 0 if vect has length 1
	if(length(vect)==1){
		return(0)
	} else{
		return(sd(vect))
	}
}

ListCleaner=function(listToClean) {
	Clean = function(x) {
		if(is.data.frame(x))
			x = as.matrix(x)

		x = unname(x)
		return(x)
	}
	listToClean = lapply(listToClean, Clean)
	return(listToClean)
}

NormalizeValues=function(values, lowBound=0, upBound=1)
{
	vectLength=length(values)

	if(vectLength>1) {
		if(length(unique(values))>1) {
			normalizedValues = (values-min(values, na.rm=T))/(max(values,na.rm=T)-min(values,na.rm=T))
			normalizedValues = normalizedValues*(upBound-lowBound)+lowBound
			return(normalizedValues)
		} else {
			cat("Impossible to normalize a costant vector. Returning repeated", lowBound, "\n")
			constantValues=rep(lowBound,vectLength)
			return(constantValues)
		}
	} else {
		put("Impossible to normalize one only number. Returning", lowBound, "\n")
		return(lowBound)
	}
}

Bootstrap=function(datx, daty, inSampleFrac) {
	numdatax=nrow(datx)
	stopifnot(!is.null(numdatax), numdatax>2)

	numdatay=length(daty)
	if(numdatax!=numdatay) {
		stop("x and y data size don't match")
	}


	numinsample=trunc(numdatax*inSampleFrac)
	numoutofsample=numdatax-numinsample
	if(numinsample < 3) {
		cat("Warning: less than three points in in-sample data set\n")
	}
	if(numoutofsample < 3) {
		cat("Warning: less than three points in out-of-sample data set\n")
	}

	insamplerows=.Internal(sample(as.integer(numdatay), as.integer(numinsample), replace = F, prob = NULL))
	insampledatax=datx[insamplerows,]
	insampledatay=daty[insamplerows]

	outofsampledatax=datx[-insamplerows,]
	outofsampledatay=daty[-insamplerows]

	output=list(inx=insampledatax, iny=insampledatay,
		outx=outofsampledatax, outy=outofsampledatay)
	return(output)

}

ScaleCenter=function(obj, scaleMean, scaleSd) {
	restore=is.vector(obj)
	obj=VectToMat(obj)
	obj=sweep(obj,2,scaleMean)
	obj=sweep(obj,2,scaleSd, "/")
	if(restore)
		obj=as.numeric(obj)

	return(obj)
}

CorToCovMat = function(corMat, stdVect){
	stopifnot(is.matrix(corMat))
	stopifnot(is.numeric(stdVect))
	stopifnot(all(stdVect>=0))
	stopifnot(ncol(corMat)==ncol(corMat))
	stopifnot(ncol(corMat)==length(stdVect))
	covMat=corMat
	for(i in 1:nrow(corMat))
		for(j in 1:ncol(corMat))
			covMat[i, j] = corMat[i, j] *stdVect[i]*stdVect[j]
	return(covMat)
}

TheoNormalize=function(obj, theoMin, theoMax) {

	stopifnot(length(theoMin)==length(theoMax))
	stopifnot(all(theoMin<theoMax))
	restore=is.vector(obj)
	obj=VectToMat(obj)
	stopifnot(ncol(obj)==length(theoMin))
	whichNonNumeric=NULL
	for(i in 1:ncol(obj))
		if(!is.numeric(obj[,i]))
			whichNonNumeric=c(whichNonNumeric, i)

	objTmp=obj

	if(!is.null(whichNonNumeric)) {
		if(restore) {
			cat("TheoNormalize: can't normalize a character vector. Returning NULL\n")
			return(NULL)
		}
		objTmp[,whichNonNumeric]=0
		objTmp=sweep(objTmp,2,theoMin)
		objTmp=sweep(objTmp,2,(theoMax-theoMin), "/")
		objTmp[,whichNonNumeric]=obj[,whichNonNumeric]
	} else{

		stopifnot(all(!apply(objTmp, 1, function(x){any(x<theoMin)})))
		stopifnot(all(!apply(objTmp, 1, function(x){any(x>theoMax)})))

		objTmp=sweep(objTmp,2,theoMin)
		objTmp=sweep(objTmp,2,(theoMax-theoMin), "/")
	}
	if(restore) objTmp=as.numeric(objTmp)

	return(objTmp)
}

QuitAndStopCluster = function(parallelize, multiNode, cl, status=0) {
	# the three arguments are defined in ParamsParallel.R
	#Stops any existing clusters, then quits R
	if(parallelize) {
		if(multiNode) {
			stopCluster(cl)
		}
		else {
			stopImplicitCluster()
		}
	}
	quit(save="default", status=status)
}

CheckFiles = function(..., QuitFunction = quit, quitArgList= list(status=10)) {
	# Checks if all files named in ... exist in the working directory
	# and if not executes a certain function
	# ... : file name strings, separated by a comma
	# QuitFunction: a function to execute in case a file named in ... does not exist
	# quitArgList: a list, each element of which corresponding to an argument
	#	for the QuitFunction call

	fileList = c(...)
	missing = fileList[!file.exists(fileList)]

	if(length(missing)>0) {
		if(length(missing)==1){
			verb = "does not"
		} else {
			verb = "do not"
		}
		cat("Error:", paste(missing, collapse=", "), verb, "exist\n")
		do.call(QuitFunction, quitArgList)
	}
}

UpdateSpaceInfo = function(spaceInfo, newList) {
	spaceInfo$space = newList$space
	spaceInfo$hash = newList$hash
	spaceInfo$hashTable = newList$hashTable
	return(spaceInfo)
}

UpdateCoreInfo = function(expEnvOrOutList, newCoreInfo, ignore = c("spaceEnv"), replace = c("predictiveModel")) {
	# Updates expEnvOrOutList environment/list based on elements of newCoreInfo list
	# Each element named "elementName" in newCoreInfo not existing in expEnvOrOutList
	#	will be added to expEnvOrOutList
	# Each element named "elementName" in newCoreInfo existing in expEnvOrOutList will
	# 	be c-bound/row-bound if that element is a vector/matrix-data.frame
	#	unless "elementName" is contained in the 'ignore' argument, in which
	#	case the element will be ignored, or it is contained in the 'replace'
	# 	argument, in which case the element will be overwritten
	# Returns the updated expEnvOrOutList

	# expEnvOrOutList, an environment or list
	# newCoreInfo: a *named* list
	# overwrite: a character vector
	# replace: a character vector

	stopifnot(is.environment(expEnvOrOutList) | is.list(expEnvOrOutList))
	stopifnot(is.list(newCoreInfo))
	if(!is.null(ignore))
		stopifnot(is.character(ignore))
	if(!is.null(replace))
		stopifnot(is.character(replace))
	stopifnot(all(!(ignore%in%replace)))


	expEnvOrOutListObjectNames = ls(expEnvOrOutList)
	objectNames = names(newCoreInfo)
	for(i in objectNames){
		currObject = newCoreInfo[[i]]
		objExistsInCoreInfo = i%in%expEnvOrOutListObjectNames
		if(!(i%in%ignore)){
			if(!objExistsInCoreInfo | i%in%replace) {
				expEnvOrOutList[[i]] = currObject
			} else{
				envObj = expEnvOrOutList[[i]]
				if(is.vector(currObject) | is.list(currObject)) {
					expEnvOrOutList[[i]] = c(envObj, currObject)
				} else if (is.matrix(currObject) | is.data.frame(currObject)){
					classEnvObj = class(envObj)
					classCurrObj = class(currObject)
					if (classEnvObj!=classCurrObj) {
						stop("Trying to rbind an object of class '", classCurrObj, "' to an object of class '", classEnvObj, "'")
					} # when rbinding data frames, there might be an issue of non-matching column names, or inconsistent row names...
					expEnvOrOutList[[i]] = rbind(envObj, currObject)
				} else if (is.environment(currObject)){
					expEnvOrOutList[[i]] = EnvUnion(list(envObj, currObject))
				} else if (is.null(currObject)){
					if(!is.null(envObj)){
						stop("A NULL currObject corresponds to a non-NULL envObj")
					}
				} else{
					stop("Don't know how to handle currObject\n")
				}
			}
		}
	}
	return(expEnvOrOutList)
}

ChangeExpEnv = function(expEnvOriginal, expChanges, RepeatAnalyzerFunction) {
	# Given a set of retroactive (typically, response-value) changes, recomputes the experiment environment,
	#	along with all its 'working variables' and its repeat analysis, for each of the generations
	#	from the first one affected by a change *through* the last attempted one ('through', since in principle
	#	changing a past experiment may affect all following generations, e.g., if that experiment gets repeated
	#	in following generations, and consequently the average response of that experiment needs to be recalculated)
	# Returns a list of recomputed experiment environments for all generations mentioned above
	# expEnvOriginal: the experiment environment as of the last completed generation
	# expChanges: a list with elements:
	#	$experiment, matrix/data frame of experiments to be changed
	# 	$generation, vector of their respective generation
	#	$response, vector of their respective original response measurement
	#	$newResponse, vector of their respective new response measurement (to replace the corresponding one in $response)
	#	$indexWithinGen, vector of their respective row-index within their respective generation
	# RepeatAnalyzerFunction: a repeat-analyzer function

	stopifnot(is.list(expChanges))
	expChanges$experiment = VectToMat(expChanges$experiment) # works also if expChanges$experiment is NULL
	numChangedExperiments=nrow(expChanges$experiment)
	if(numChangedExperiments==0)
		stop("No experiments to change in expChanges.\n")

	stopifnot(is.matrix(expChanges$experiment) | is.data.frame(expChanges$experiment))
	stopifnot(class(expChanges$experiment) == class(expEnvOriginal$experiment))
	stopifnot(is.numeric(expChanges$generation))
	stopifnot(is.numeric(expChanges$indexWithinGen))
	stopifnot(all(expChanges$indexWithinGen>0))
	stopifnot(is.numeric(expChanges$response))
	stopifnot(is.numeric(expChanges$newResponse))
	stopifnot(numChangedExperiments==length(expChanges$generation))
	stopifnot(numChangedExperiments==length(expChanges$indexWithinGen))
	stopifnot(numChangedExperiments==length(expChanges$response))
	stopifnot(numChangedExperiments==length(expChanges$newResponse))
	stopifnot(is.numeric(expEnvOriginal$lastAttemptedGen))
	stopifnot(is.numeric(expEnvOriginal$lastCompletedGen))

	if(expEnvOriginal$lastCompletedGen<max(expChanges$generation))
		stop("The latest completed generation in expEnv is ", expEnvOriginal$lastCompletedGen,
			" but the latest generation in expChanges is ", max(expChanges$generation), ".\n", call.=F)
			#changes should involve only experiments that already have a response measurement

	expEnvChangedList = list()
	expEnvChanged = as.environment(as.list(expEnvOriginal))
	analyzeLastAttemptedGen = ifelse(expEnvOriginal$lastAttemptedGen>expEnvOriginal$lastCompletedGen, F, T)
	for(i in min(expChanges$generation):expEnvOriginal$lastAttemptedGen){
		# IMPORTANT to iterate through increasing generation indices here! Adjusting expEnvChanged one
		#	generation after another
		# Note: iterating through lastAttemptedGen' even if expChanges$generation will never be as large
		#	as per the function description
		if(i%in%expChanges$generation){
			currGenExperimentOriginal = expEnvOriginal$experiment[expEnvOriginal$generation == i, ]
			currGenHashOriginal = expEnvOriginal$hash[expEnvOriginal$generation == i]
			currGenResponseOriginal = expEnvOriginal$response[expEnvOriginal$generation == i]

			currGenExperimentToChange = VectToMat(expChanges$experiment[expChanges$generation == i, ])
			currGenHashToChange = apply(currGenExperimentToChange, 1, MakeHashKey)
			currGenResponseToChange = expChanges$response[expChanges$generation == i]
			currGenNewResponseToChange = expChanges$newResponse[expChanges$generation == i]
			currGenIndexToChange = expChanges$indexWithinGen[expChanges$generation == i]

			if(!all(currGenIndexToChange<=length(currGenResponseOriginal)))
				stop("Experiment indices for generation ", i,
				" in expChanges exceed the number of experiments for that generation in the original experiment environment.\n", call.=F)
			if(!length(unique(currGenIndexToChange))==length(currGenIndexToChange))
				stop("Experiment indices for generation ", i, " in expChanges contain duplicates.\n", call.=F)

			cnt = 1
			for(j in currGenIndexToChange){
				hashOriginal = currGenHashOriginal[j]
				responseOriginal = currGenResponseOriginal[j]
				hashToChange = currGenHashToChange[cnt]
				responseToChange = currGenResponseToChange[cnt]
				newResponseToChange = currGenNewResponseToChange[cnt]
				if(hashOriginal!=hashToChange){
					stop("Experiment ", j, " of generation ", i,
					" in expChanges does not match the corresponding one in the original experiment environment.\n", call.=F)
				}
				if(responseOriginal!=responseToChange){
					stop("The original response measurement of experiment ", j, " of generation ", i,
					" in expChanges does not match the corresponding one in the original experiment environment.\n", call.=F)
				}
				if(newResponseToChange == responseOriginal){
					warning("The new response measurement of experiment ", j, " of generation ", i,
					" in expChanges coincides with the corresponding original one in the original experiment environment.\n", call.=F)
				}
				expEnvChanged$response[expEnvChanged$generation == i][currGenIndexToChange]=currGenNewResponseToChange
				cnt = cnt + 1
			}
		}
		currGenString = paste("Gen", i, sep="")
		expChangedUpToGenI = TrimExpEnvElements(expEnvChanged, i) # note that expEnvChanged
																#at this point is up-to-date with changes up to generation i

		expChangedUpToGenI = UpdateWorkingVariables(expChangedUpToGenI)
		expChangedUpToGenI	= AnalyzeResponse(expChangedUpToGenI, RepeatAnalyzerFunction,
			genToAnalyze=min(expEnvOriginal$generation):min(i, expEnvOriginal$lastCompletedGen))
			#min(expEnvOriginal$generation) is the same as min(expChangedUpToGenI$generation)
			#min(i, expEnvOriginal$lastCompletedGen) makes sure, in case i=lastAttemptedGen>lastCompletedGen,
			#  when processing i, we do repeat analysis on 1:(i-1), but not on i, since it doesn't have
			# response measurements
		expEnvChangedList[[currGenString]] = expChangedUpToGenI
	}
	return(expEnvChangedList)
}

TrimExpEnvElements = function(expEnv, latestGeneration, toRemove=c("parameter")){
	# Removes from an experiment environment any information that was not available after a certain generation
	# 	and returns the corresponding "trimmed" experiment environment,
	# expEnv: an experiment environment
	# latestGeneration: the generation after which all information is to be trimmed out.
	# RepeatAnalyzerFunction: a repeat-analyzer function
	# toRemove: a character string, each element of which corresponds to an element of expEnv to be totally
	# 	removed (not trimmed)
	# Note: if latestGeneration = expEnv$lastAttemptedGen no trimming or removal will be carried out, since it would
	#	be irrelevant

	stopifnot(is.environment(expEnv))
	stopifnot(is.character(expEnv$infoVaryingByExperiment))
	stopifnot(is.character(expEnv$infoVaryingByGeneration))
	stopifnot(is.environment(expEnv$hashTable))
	stopifnot(is.numeric(expEnv$lastAttemptedGen))

	stopifnot(is.numeric(latestGeneration))
	stopifnot(latestGeneration==round(latestGeneration))
	stopifnot(expEnv$lastAttemptedGen>=latestGeneration)
	stopifnot(is.character(toRemove))

	expEnvTrimmed = as.environment(as.list(expEnv))

	if(latestGeneration<expEnv$lastAttemptedGen){
		listOrEnvOKtoSkipVect = c("hashTable", "infoVaryingByGeneration")

		for(k in expEnvTrimmed$infoVaryingByExperiment){
			# removing vector entries/matrix rows corresponding to generations after latestGeneration
			if(is.matrix(expEnvTrimmed[[k]]) | is.data.frame(expEnvTrimmed[[k]])){
				expEnvTrimmed[[k]] = expEnvTrimmed[[k]][expEnv$generation<=latestGeneration,]
			} else if (is.vector(expEnvTrimmed[[k]])){
				expEnvTrimmed[[k]] = expEnvTrimmed[[k]][expEnv$generation<=latestGeneration]
			} else if (!(is.environment(expEnvTrimmed[[k]]) | is.list(expEnvTrimmed[[k]]) &
				k%in%listOrEnvOKtoSkipVect)) {
					# so far we don't know what to do with other elements of expEnv
				stop("Don't know how to trim ", k, ".\n")
			}
		}

		rm(hashTable, pos = expEnvTrimmed) # removing and recreating trimmed hashTable
		expEnvTrimmed$hashTable = NewEnv()
		for(k in unique(expEnvTrimmed$hash))
			expEnvTrimmed$hashTable[[k]] = 1

		for(k in expEnvTrimmed$infoVaryingByGeneration) {
			# we expect expEnvTrimmed[[k]] to be a list or environment for all k (e.g., $parameterByGen)
			# removing elements in expEnvTrimmed[[k]]corresponding to generations after latestGeneration

			stopifnot(is.list(expEnvTrimmed[[k]])| is.environment(expEnvTrimmed[[k]]))
			for(j in names(expEnvTrimmed[[k]])){
				stopifnot(substr(j, 1,3)=="Gen")
				currByGenGen = as.numeric(sub("Gen", "", j))
				stopifnot(!is.na(currByGenGen))
				if(currByGenGen>latestGeneration){
					if(is.list(expEnvTrimmed[[k]])){
						expEnvTrimmed[[k]][[j]] = NULL
					} else {
						# if environment...
						rm(j, pos=expEnvTrimmed[[k]])
					}
				}
			}
		}

		notInEnv = !toRemove%in%names(expEnvTrimmed)
		if(any(notInEnv))
			warning("Element(s) ", paste(toRemove[notInEnv], collapse=","), " not present in expEnv.\n", call.=F)
		rm(list=toRemove, pos=expEnvTrimmed)
	}
	return(expEnvTrimmed)
}
