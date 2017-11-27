# source("errorTypes.R")


BoundExperimentDefinition = function(experimentDefinition=NULL, spaceDef = NULL, popSize = NULL, numRepeat = NULL,
                                     returnBoundAndSpaceInfo = F, minSpaceSize = 100, minVarNum = 2, maxVarNum = 20, minValNum = 2, maxValNum = 20,
                                     maxEncodingSize = 8, maxRamMBForLargeVariables = 250, maxDBWithoutRepeats = 20000,
                                     maxDBWithRepeats = 2*maxDBWithoutRepeats, minPopSize = 10, maxPopSize = 2500, popSizeVarNumPower = 4,
                                     popSizeMinNumGen = 3, maxNumRepeat = 10, totalVolume=NULL){

    # Given an experiment definition (ESD):
    # if called with returnBoundAndSpaceInfo = F, specified population size (popSize), and specified number
    #of repeats (numRepeat):
    #if any of definition, population size, or number of repeats do not respect certain bounds,
    #it returns a character string vector, each element of which is an error message, and NULL otherwise
    # if called with returnBoundAndSpaceInfo = T (population size and number of repeats are optional)
    #it returns a list with all bounds, as well as misc info about the space resulting from the ESD;
    #if numRepeat is specified, the list will also include $maxPopSizeGivenRepeats, which corresponds
    ## to the largest population size allowed, given numRepeat; if popSize and numRepeat are both specified,
    #the list will also include$maxNumGenGivenPopSizeAndRepeats, which corresponds to the largest allowed
    ## number of generationsgiven popSize and numRepeat


    # experimentDefinition: ESD in data frame format, with one "Name" column, multiple "Value*" columns, and one row for
    #each experimental parameter; should be set to NULL if spaceDef is not
    # spaceDef: ESD in list format, with as many named elements as the number of experimental parameters, with each element
    #containing the possible values of the corresponding experimental parameter; should be set to NULL if experimentDefinition is not
    # popSize: an integer, corresponding to the population size; can be set to NULL if returnBoundAndSpaceInfo = T
    # numRepeat: an integer, corresponding to the number of repeats per experiment; can be set to NULL if returnBoundAndSpaceInfo = T
    # returnBoundAndSpaceInfo: logical, set = F if the function should check if the former three arguments respect
    #their bounds and return error messages accordingly; set = T if the function should only return
    #a list of such bounds, and other misc info about the experimental space
    # minSpaceSize: integer, minimum experimental space size
    # minVarNum/maxVarNum: integer, minimum/maximum number of experimental parameters
    # minValNum/maxValNum: integer, minimum/maximum number of values per experimental parameter
    # minPopSize/maxPopSize: integer, minimum/maximum population size
    # maxDBWithoutRepeats: maximum number of experiments ever allowed to be in the database, without counting repeats
    # maxDBWithRepeats: maximum number of experiments ever allowed to be in the database, counting repeats

    # maxRamMBForLargeVariables = 250 #Maximum overall size of experiment-matrix, hash-vector, and hashTable-environment variables ever allowed to
    # be handled/created. This value will determine whether a PDT run will be executed in automated mode or not, and
    # will be used to set 'bounds' on experiment-sampler parameter defaults
    # popSizeVarNumPower: integer, the number of experimental parameters will be raised to this power to define
    #another bound on maximum population size
    # popSizeMinNumGen: integer, the minimum allowed number of generations, which will be used to define
    #another bound on maximum population size
    # maxNumRepeat: integer, maximum number of repeats
    # totalVolume: integer, total number of units in an experiment, when dealing with mixtures
    #set to NULL if space is factorial


    # Note: the stopifnot calls below are not used to generate the error messages;
    # all they do is stop execution if the function is being misused.

    if(!is.null(experimentDefinition)){
        stopifnot(is.null(spaceDef))
        stopifnot(is.data.frame(experimentDefinition))
        stopifnot(NROW(experimentDefinition)>0)
        expDefNames = colnames(experimentDefinition)
        stopifnot("Name"%in%expDefNames)
        whichValCols = which(expDefNames!="Name")
        stopifnot(all(grepl("Value", expDefNames[whichValCols])))
        stopifnot(is.character(experimentDefinition$Name) | is.factor(experimentDefinition$Name))
        stopifnot(all(apply(experimentDefinition[, whichValCols], 2, is.numeric)))
    } else{
        stopifnot(is.null(experimentDefinition))
        stopifnot(is.list(spaceDef))
        stopifnot(length(spaceDef)>0)
        stopifnot(length(names(spaceDef))>0)
    }

    stopifnot(is.logical(returnBoundAndSpaceInfo))


    if(!is.null(popSize)){
        stopifnot(is.numeric(popSize))
        stopifnot(popSize>0)
    } else{
        stopifnot(returnBoundAndSpaceInfo)
    }
    if(!is.null(numRepeat)){
        stopifnot(is.numeric(numRepeat))
        stopifnot(numRepeat>=0)
    } else{
        stopifnot(returnBoundAndSpaceInfo)
    }
    stopifnot(is.numeric(minSpaceSize))
    stopifnot(is.numeric(minVarNum))
    stopifnot(is.numeric(maxVarNum))
    stopifnot(is.numeric(minValNum))
    stopifnot(is.numeric(maxValNum))
    stopifnot(is.numeric(maxEncodingSize))
    stopifnot(is.numeric(maxDBWithoutRepeats))
    stopifnot(is.numeric(minPopSize))
    stopifnot(is.numeric(maxPopSize))
    stopifnot(is.numeric(maxDBWithRepeats))
    stopifnot(minSpaceSize>0)
    stopifnot(minVarNum>0)
    stopifnot(maxVarNum>0)
    stopifnot(minValNum>0)
    stopifnot(maxValNum>0)
    stopifnot(maxEncodingSize>0)
    stopifnot(maxDBWithoutRepeats>0)
    stopifnot(minPopSize>0)
    stopifnot(maxPopSize>0)
    stopifnot(maxDBWithRepeats>=maxDBWithoutRepeats)
    if(is.numeric(totalVolume)){
        stopifnot(totalVolume>0 & totalVolume%%1==0)
    } else{
        stopifnot(is.null(totalVolume))
    }

    errorMessage = NULL

    if(!is.null(experimentDefinition)) {
        varNum = nrow(experimentDefinition)
        varName = as.character(experimentDefinition$Name)

    } else{
        varNum = length(spaceDef)
        varName = names(spaceDef)
    }

    maxSquaredTheoDist = varNum ### Squared max theoretical distance of two points in the space,
    ### after space normalization to the varNum-dimensional unit hypercube
    ### E.g, if varNum = 3, the two farthest points in the 3-dimensional hypercube
    ###are (0,0,0) and (1,1,1), with squared Euclidean distance (1-0)^2+(1-0)^2+(1-0)^2
    ### Note that this is just an upper bound, which is tight only if the space is full-factorial


    spaceDefTmp = list()
    whichNonIntegral = NULL
    whichNegative = NULL
    whichMinLargerThanMax = NULL
    whichDuplicate = NULL

    if(!is.null(experimentDefinition)) {
        for(i in 1:varNum){
            currVals = experimentDefinition[i, whichValCols]
            currVals = currVals[!is.na(currVals)]
            stopifnot(is.numeric(currVals))

            if(any(duplicated(currVals)))
                whichDuplicate = c(whichDuplicate, i)

            if(!is.null(totalVolume)){
                stopifnot(length(currVals)==2)

                if(any(currVals%%1!=0))
                    whichNonIntegral = c(whichNonIntegral,i)
                if(any(currVals<0))
                    whichNegative = c(whichNegative,i)
                if(currVals[1]>=currVals[2])
                    whichMinLargerThanMax = c(whichMinLargerThanMax,i)

                currVals = currVals[1]:currVals[2]

            }
            spaceDefTmp[[i]] = currVals
        }
    } else{
        spaceDefTmp = spaceDef
    }

    valNumVect = maxEncodingSizeVect = NULL
    meanEncodingSize = 0

    for(i in 1:varNum){
        currVals = spaceDefTmp[[i]]
        valNumVect[i] =  length(currVals)
        nCharCurrVals = nchar(as.character(currVals))
        maxEncodingSizeVect[i] = max(nCharCurrVals)
        meanEncodingSize = meanEncodingSize + mean(nCharCurrVals) # this estimate is off for mixture spaces
        #b/c each value typically appears with different frequencies in the space
    }

    largestEncodingSize = max(maxEncodingSizeVect)
    spaceSize = SpaceSize(spaceDefTmp, totalVolume)
    exhaustiveModeRequiredRamMB = round(LargeVariablesSize(spaceSize, varNum,
        meanEncodingSize, ifelse(is.null(totalVolume),F,T)),2)

    if(exhaustiveModeRequiredRamMB<=maxRamMBForLargeVariables){
        exhaustive = T
    } else {
        exhaustive = F
    }

    if(!is.null(totalVolume)){
        minValueSum = sum(mapply(min, spaceDefTmp))
        maxValueSum = sum(mapply(max, spaceDefTmp))
    }

    smallestValNum = min(valNumVect)
    largestValNum = max(valNumVect)
    minPopSize = max(minPopSize, varNum)
    maxPopSize = min(varNum^popSizeVarNumPower, floor(spaceSize/popSizeMinNumGen), floor(maxDBWithoutRepeats/popSizeMinNumGen), maxPopSize)
    maxPopSizeGivenRepeats = floor(maxDBWithRepeats/popSizeMinNumGen/(1+numRepeat))

    maxNumExperiments = round(MaxNumTotalPoints(varNum, meanEncodingSize, maxRamMBForLargeVariables)/3) # kind of arbitrary...
    maxNumExperiments = min(maxNumExperiments, spaceSize, maxDBWithoutRepeats) # note correspondence with maxDBWithoutRepeats...

    if(returnBoundAndSpaceInfo){
        boundAndSpaceList = list(
            minSpaceSize = minSpaceSize,
            minVarNum = minVarNum,
            maxVarNum = maxVarNum,
            minValNum = minValNum,
            maxValNum = maxValNum,
            maxEncodingSize = maxEncodingSize,
            maxDBWithoutRepeats = maxDBWithoutRepeats,
            maxDBWithRepeats = maxDBWithRepeats,
            maxNumExperiments = maxNumExperiments,
            minPopSize = minPopSize,
            maxPopSize = maxPopSize,
            maxNumRepeat = maxNumRepeat,
            maxRamMBForLargeVariables = maxRamMBForLargeVariables,
            spaceSize=spaceSize,
            spaceDef = spaceDefTmp,
            exhaustive=exhaustive,
            exhaustiveModeRequiredRamMB = exhaustiveModeRequiredRamMB,
            meanEncodingSize = meanEncodingSize,
            maxSquaredTheoDist = maxSquaredTheoDist
        )

        if(!is.null(numRepeat)){
            boundAndSpaceList$maxPopSizeGivenRepeats = min(maxPopSize, maxPopSizeGivenRepeats)
        } else{
            boundAndSpaceList$maxPopSizeGivenRepeats = "Cannot calculate if numRepeat is not specified"
        }

        if(!is.null(popSize) &!is.null(numRepeat)){
            maxNumGen = floor(maxNumExperiments/popSize) # this already keeps into account maxDBWithoutRepeats
            maxNumGenGivenRepeats = floor(maxDBWithRepeats/popSize/(1+numRepeat))
            boundAndSpaceList$maxNumGenGivenPopSizeAndRepeats = min(maxNumGen, maxNumGenGivenRepeats)
        }else {
            boundAndSpaceList$maxNumGenGivenPopSizeAndRepeats = "Cannot calculate if popSize and numRepeat are not both specified"
        }
        return(boundAndSpaceList)
    }


    if(varNum<minVarNum)
        errorMessage = c(errorMessage, paste0("The experimental space contains ", varNum, " parameters",
            ", which is less than the minimum allowed of ",  minVarNum, "."))

    if(varNum>maxVarNum)
        errorMessage = c(errorMessage, paste0("The experimental space contains ", varNum, " parameters",
            ", which is more than the maximum allowed of ",  maxVarNum, "."))

    if(smallestValNum<minValNum)
        errorMessage = c(errorMessage, paste0("Experimental parameter ", paste(varName[valNumVect<minValNum], sep=", "),
            " contains fewer values than the minimum allowed of ",  minValNum, "."))

    if(largestValNum>maxValNum)
        errorMessage = c(errorMessage, paste0("Experimental parameter ", paste(varName[valNumVect>maxValNum], sep=", "),
            " contains more values than the maximum allowed of ",  maxValNum, "."))

    if(length(whichDuplicate)>0)
        errorMessage = c(errorMessage, paste0("Experimental parameter ", paste(varName[whichDuplicate], sep=", "),
            " contains duplicate values."))

    if(spaceSize<minSpaceSize)
        errorMessage = c(errorMessage, paste0("The experimental space contains ", spaceSize, " distinct experiments",
            ", which is less than the minimum allowed of ",  minSpaceSize, "."))

    if(largestEncodingSize>maxEncodingSize)
        errorMessage = c(errorMessage, paste0("Some of the values of experimental parameter ",
            paste(varName[maxEncodingSizeVect>maxEncodingSize], sep=", "),
            " contain more digits than the maximum allowed of ",  maxEncodingSize, "."))


    if(!is.null(totalVolume)){
        if(length(whichNegative)>0)
            errorMessage = c(errorMessage, paste0("Experimental parameter ", paste(varName[whichNegative], sep=", "),
                " contains negative mixture unit values."))

        if(length(whichNonIntegral)>0)
            errorMessage = c(errorMessage, paste0("Experimental parameter ", paste(varName[whichNonIntegral], sep=", "),
                " contains non-integer mixture unit values."))

        if(length(whichMinLargerThanMax)>0)
            errorMessage = c(errorMessage, paste0("In experimental parameter ", paste(varName[whichMinLargerThanMax], sep=", "),
                " the minimum number of mixture units is larger than the maximum."))

    }


    if(is.null(errorMessage)) {
        # these validation steps are run only after the experimental space definition is flawless

        if(popSize<minPopSize)
            errorMessage = c(errorMessage, paste0("The population size is less than the minimum allowed of ",
                minPopSize, " for this experimental space."))

        if(popSize>maxPopSizeGivenRepeats & numRepeat<=maxNumRepeat & maxPopSizeGivenRepeats<maxPopSize){ # & popSize<=maxPopSize
            errorMessage = c(errorMessage, paste0("The population size is more than the maximum allowed of ",
                maxPopSizeGivenRepeats, " for this experimental space, given the selected number of repeats."))
        } else if (popSize>maxPopSize){
            errorMessage = c(errorMessage, paste0("The population size is more than the maximum allowed of ",
                maxPopSize, " for this experimental space."))
        }

        if(numRepeat>maxNumRepeat)
            errorMessage = c(errorMessage, paste0("The number of repeats is larger than the maximum allowed of ", maxNumRepeat, "."))

        if(!is.null(totalVolume)){
            if(totalVolume<minValueSum)
                errorMessage = c(errorMessage, paste0("The total number of mixture units",
                    "is smaller than the sum of the minimum number of units for all experimental parameters."))

            if(totalVolume>maxValueSum)
                errorMessage = c(errorMessage, paste0("The total number of mixture units",
                    "exceeds the sum of the maximum number of units for all experimental parameters."))

        }

    }

    if(is.null(errorMessage)) {
        # these validation steps are run only after the experimental space definition is flawless
        if(!is.null(totalVolume)){
            if(!exhaustive)
                errorMessage = c(errorMessage, paste0("The experimental space resulting from this mixture ESD is too large. ",
                    "Try increasing the minimum/decreasing the maximum number of mixture units for one or more experimental parameters, "),
                    "or decreasing the total number of mixture units, or decreasing the number of experimental parameters.")
        }
    }

    # Validation failed
    if (!is.null(errorMessage)) {
      e <- experiment_definition_validation_error(errorMessage)
      stop(e)
    }

    # Validation succeeded
    TRUE
}
