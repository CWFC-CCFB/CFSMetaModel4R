#############################################################
# This file is part of the Capsis4R library located at :
# https://sourceforge.net/projects/capsis4r/
#
# Copyright (C) 2022 Jean-François Lavoie and Mathieu Fortin
# for Canadian Forest Service.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 3 of the License, or (at your option) any later version.
#
# This library is distributed with the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied
# warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE. See the GNU Lesser General Public
# License for more details.
#
# Please see the license at http://www.gnu.org/copyleft/lesser.html.
#############################################################

#'
#' The cache environment of this package
#'
#' This environment contains the objects that enable the connection to
#' the gateway server.
#'
#'@export
C4RCacheEnv <- new.env()

.welcomeMessage <- function() {
  packageStartupMessage("Welcome to Capsis4R !")
  packageStartupMessage("Please, make sure that the Java J4R server is running and accessible from your computer.")
  packageStartupMessage("For more information, visit https://sourceforge.net/p/ .")
}

.onAttach <- function(libname, pkgname) {
  .welcomeMessage()
}

.onDetach <- function(libname) {

  print("J4R Shutting down connection to server")
  CBRelease();
}

#' Gets the CAPSIS script API object through J4R
#' @export
getScript <- function() {
  if(exists("script", envir=C4RCacheEnv, inherits=FALSE)){
    return(get("script", envir=C4RCacheEnv, inherits=FALSE))
  }
  else {
    stop("Connection to server not established.  Please call CBInitialize() first.")
  }
}

#' Gets the MetaModelManager API object through J4R
#' @export
getMetaModelMgr <- function() {
  if(exists("metaModelMgr", envir=C4RCacheEnv, inherits=FALSE)){
    return(get("metaModelMgr", envir=C4RCacheEnv, inherits=FALSE))
  }
  else {
    stop("Connection to server not established.  Please call CBInitialize() first.")
  }
}

#' Releases all resources from the communication channel with the CAPSIS java server
#' @export
CBRelease <- function() {

  if (!is.null(getScript()))
    rm(list = "script", envir = C4RCacheEnv)

  if (!is.null(getMetaModelMgr()))
    rm(list = "metaModelMgr", envir = C4RCacheEnv)

  J4R::shutdownClient()
}

#' Initializes the communication channel with the J4R java CAPSIS server
#' at the specified address
#' @param address The address of the J4R server to use
#' @param port The port number to use to connect to the J4R server
#' @param internalPort The internal ports to use to connect to the J4R server (ex : 50000:50001)
#' @param key A pre-shared key integer used by the server to accept the client requests
#' @seealso CBRelease
#' @export
CBInitialize <- function(address, port, internalPort, key) {
  if (exists("script", envir=C4RCacheEnv, inherits=FALSE) || exists("metaModelMgr", envir=C4RCacheEnv, inherits=FALSE))
    CBRelease()

  result <- J4R::connectToJava(host=address, port = port, internalPort = internalPort, public=T, key=key)
  if (result == FALSE)
    stop(paste("Could not initialize server at ", address, rep=""))

  assign("script", J4R::createJavaObject("artemis.script.ArtScript"), envir = C4RCacheEnv, inherits = FALSE)

  assign("metaModelMgr", J4R::createJavaObject("repicea.simulation.metamodel.MetaModelManager"), envir = C4RCacheEnv, inherits = FALSE)
}

#' Return Capsis4R version
#' @return List containing version numbers
#' @export
CBGetVersion <- function() {
  script <- getScript()

  ver = list("Capsis4R" = toString(packageVersion("Capsis4R")), "Capsis" = script$getCapsisVersion())

  return (ver)
}

#' Enumerates the model data fields expected by the server.  Use this to create the index array send to CBSetFieldMatches.
#' @return A vector of strings containing all the fields as triplets separated by ";" ex : "PLOT (mandatory); String; Match: 0       1"
#' @seealso CBSetFieldMatches
#' @export
CBGetModelDataFields<- function() {
  script <- getScript()

  fields <- script$getFieldDescriptions()

  values <- J4R::getAllValuesFromListObject(fields)

  return (values$toString())
}

#' Sets initial model parameters.  Must be called prior to calling CBRunSimulation
#' @param initialDateYear The initial year to be used for simulation
#' @param stochasticMode Enable stochastic mode simulation (0 = disable)
#' @param numberOfRealizations The number of realizations to be used for simulation
#' @param applicationScale Application scale, possible values are ["FMU", "Stand"]
#' @param climateChangeOption Climate change option, possible values are ["NoChange", "Plus2Degrees", "Plus4Degrees", "Plus6Degrees"]
#' @seealso CBRunSimulation
#' @export
CBSetInitialParameters <- function(initialDateYear, stochasticMode, numberOfRealizations, applicationScale, climateChangeOption) {
  script <- getScript()

  applicationScaleEnum <- createJavaObject("repicea.simulation.ApplicationScaleProvider$ApplicationScale", applicationScale)

  script$setInitialParameters(as.integer(initialDateYear), stochasticMode != 0, as.integer(numberOfRealizations), applicationScaleEnum, climateChangeOption)
}

#' Sets evolution parameters.  Must be called prior to calling CBRunSimulation
#' @param finalDateYear The final year to be used for simulation
#' @seealso CBRunSimulation
#' @export
CBSetEvolutionParameters <- function(finalDateYear) {
  script <- getScript()

  script$setEvolutionParameters(as.integer(finalDateYear))
}

#' Gets the list of species of the specified type
#' @param types The types to get the list of species for (vector)
#' @return a vector containing all the names of the species for all input types
#' @export
CBGetSpeciesOfType <- function(types) {
  script <- getScript()

  speciesTypeArray <- J4R::createJavaObject("repicea.simulation.covariateproviders.treelevel.SpeciesTypeProvider$SpeciesType", length(as.vector(types)), isArray=TRUE)

  enums <- J4R::createJavaObject("repicea.simulation.covariateproviders.treelevel.SpeciesTypeProvider$SpeciesType", types)

  J4R::setValueInArray(speciesTypeArray, enums)

  species <- script$getSpeciesOfType(speciesTypeArray)

  return(species)
}

#' Register a request for a particular output of the simulation.
#' @param request The request to register
#' @param aggregationPatterns The aggregation patterns to register
#' @export
CBRegisterOutputRequest <- function(request, aggregationPatterns) {
  script <- getScript()

  script$registerOutputRequest(request, aggregationPatterns)
}

#' Sets field matches for input data.  Must be called prior to calling CBSendData.
#' @param matches An integer vector specifying the field match order to be used by the server when interpreting CBSendData calls.
#' @return TRUE if operation succeed
#' @seealso CBGetModelDataFields
#' @export
CBSetFieldMatches <- function(matches) {
  script <- getScript()

  matchesJavaArray <- J4R::as.JavaArray(as.integer(matches))

  result <- script$setFieldMatches(matchesJavaArray)

  if (result != TRUE)
    stop("CBSetFieldMatches failed")
}

#' Sends data to the CAPSIS Server.  Must be called prior to calling CBRunSimulation.
#'
#' @param data The data rows as a list of vector of strings.
#'
#' @seealso CBRunSimulation
#'
#' @export
CBSendData <- function(data) {
  script <- getScript()

  for(o in data) {
    tempArray <- J4R::createJavaObject("java.lang.Object", length(o), isArray = TRUE)
    J4R::setValueInArray(tempArray, o)
    script$addRecord(tempArray)
  }
}

#' Runs the simulation on the CAPSIS model
#'
#' @return simulation results
#'
#' @seealso CBRunSimulation
#'
#' @export
CBRunSimulation <- function() {
  script <- getScript()

  return (script$runSimulation())
}

#' Closes the active project and frees its resources
#'
#' @seealso CBRunSimulation
#'
#' @export
CBCloseProject <- function() {
  script <- getScript()

  script$closeProject()
}

#' Creates a new metamodel.  This must be called before MMAddSimulationResult calls
#' @param stratumGroupID The stratum group id to add the simulation results to
#' @param geoDomain the geographic domain of the source data
#' @param dataSource the origin of the source data (ex: fourth inventory)
#' @export
MMCreate <- function(stratumGroupID, geoDomain, dataSource) {
   metaModelMgr <- getMetaModelMgr()

   metaModelMgr$createMetaModel(stratumGroupID, geoDomain, dataSource)
}

#' Gets a metamodel
#' @param stratumGroupID The stratum group id to add the simulation results to
#' @return the metamodel instance
#' @export
MMGet <- function(stratumGroupID) {
  metaModelMgr <- getMetaModelMgr()

  return(metaModelMgr$get(stratumGroupID))
}

#' Clears the metamodel manager
#' @export
MMClear <- function() {
  metaModelMgr <- getMetaModelMgr()

  metaModelMgr$clear()
}

#' Adds a simulation result to the MetaModel.  Typically, MMCreate() is called first
#' then this method is called as many times as needed, and followed by MMFitMetaModel()
#' and finally, multiple MMGetPrediction() calls to retrieve the predictions.
#' @param stratumGroupID The stratum group id to add the simulation results to
#' @param initialAgeYr The initial age year of this stratum group
#' @param simulationResult The simulations results (given by CBGetSimulationResult)
#' @export
MMAddSimulationResult <- function(stratumGroupID, initialAgeYr, simulationResult) {
  metaModelMgr <- getMetaModelMgr()

  metaModelMgr$addDataset(stratumGroupID, as.integer(initialAgeYr), simulationResult)
}

#' Fits the metamodel using the simulation results given to the metamodel manager using AddSimulationResult beforehand.
#' Use MMGetPredictions() afterwards to retrieve metamodel predictions.
#' @param outputType The outputType to fit the metamodel for
#' @export
MMFitMetaModel <- function(outputType) {
  metaModelMgr <- getMetaModelMgr()

  metaModelMgr$fitMetaModels(outputType)
}

#' Gets predictions from the metamodel using fitted parameters
#' MMFitMetaModel() must have been called prior to calling this method.
#' @param stratumGroupID The stratum group id for which we want the prediction
#' @param ageYr A vector of all ages for which we want the predictions
#' @param timeSinceInitialDateYr The number of years elapsed since initialDateYear for ageYr
#' @param varianceOutputType A string containing the enum representing the desired variance output type
#' @return a map containing the prediction values and optionally the variance values as well
#' @seealso MMAddSimulationResult, MMFitMetaModel
#' @export
MMGetPredictions <- function(stratumGroupID, ageYr, timeSinceInitialDateYr, varianceOutputType) {
  metaModelMgr <- getMetaModelMgr()

  ageYrArray <- J4R::as.JavaArray(as.integer(ageYr))
  varianceOutputEnum <- J4R::createJavaObject("repicea.simulation.metamodel.MetaModel$PredictionVarianceOutputType", varianceOutputType)

  return (metaModelMgr$get(stratumGroupID)$getPredictions(ageYrArray, as.integer(timeSinceInitialDateYr), varianceOutputEnum))
}

#' Gets predictions from the metamodel using MonteCarlo generation of parameters
#' MMFitMetaModel() must have been called prior to calling this method.
#' @param stratumGroupID The stratum group id for which we want the prediction
#' @param ageYr A vector of all ages for which we want the predictions
#' @param timeSinceInitialDateYr The number of years elapsed since initialDateYear for ageYr
#' @param nbSubjects The number of subjects to generate random parameters for  (use 0 to disable MC simulation for subjects)
#' @param nRealizations The number of realizations to generate random parameters for (use 0 to disable MC simulation for realizations)
#' @return the prediction values (number)
#' @seealso MMAddSimulationResult, MMFitMetaModel
#' @export
MMGetMonteCarloPredictions <- function(stratumGroupID, ageYr, timeSinceInitialDateYr, nbSubjects, nbRealizations) {
  metaModelMgr <- getMetaModelMgr()

  return (metaModelMgr$get(stratumGroupID)$getMonteCarloPredictions(as.integer(ageYr), as.integer(timeSinceInitialDateYr), as.integer(nbSubjects), as.integer(nbRealizations)))
}

#' Gets the possible output types for the specified stratumGroupID in the metamodel.
#' @param stratumGroupID the stratum group for which we want the output types
#' @return the possible output types as a list of strings
#' @export
MMGetPossibleOutputTypes <- function(stratumGroupID) {
  metaModelMgr <- getMetaModelMgr()

  return (J4R::getAllValuesFromListObject(metaModelMgr$getPossibleOutputTypes(stratumGroupID)))
}

#' Gets the selected output type for the specified stratumGroupID that was fitted using the metamodel.
#' MMFitMetaModel must have been called before this function can be used.
#' @param stratumGroupID the stratum group for which we want the output types
#' @return the selected output type as a string
#' @export
MMGetSelectedOutputType <- function(stratumGroupID) {
  metaModelMgr <- getMetaModelMgr()

  return(metaModelMgr$getSelectedOutputType(stratumGroupID))
}

#' Gets stratum groups available in the metamodel manager
#' @return stratum groups as a list of strings
#' @export
MMGetStratumGroups <- function() {
  metaModelMgr <- getMetaModelMgr()

  return (J4R::getAllValuesFromListObject(metaModelMgr$getStratumGroups()))
}

#' Gets the final fitted metamodel results for the specified statum group id.  MMFitMetaModels must have been called before calling this function.
#' @param stratumGroupID the stratum group for which we want the output types
#' @return The resulting dataSet
#' @export
MMGetMetaModelResult <- function(stratumGroupID) {
  metaModelMgr <- getMetaModelMgr()

  return(metaModelMgr$getMetaModelResult(stratumGroupID))
}

#' Saves the metamodel manager to a file on the server
#' MMFitMetaModel() must have been called prior to calling this method.
#' @param fileName The filename we want to save the metamodel to
#' @export
MMSave <- function(fileName) {
  metaModelMgr <- getMetaModelMgr()

  metaModelMgr$save(fileName)
}

#' Loads the metamodel manager from a file on the server

#' @param fileName The filename we want to save the metamodel to
#' @export
MMLoad <- function(fileName) {
  metaModelMgr <- getMetaModelMgr()

  metaModelMgr$save(fileName)
}

#' Saves a single metamodel to a file on the server
#' MMFitMetaModel() must have been called prior to calling this method.
#' @param stratumGroupID the stratum group for which we want the output types
#' @param fileName The filename we want to save the metamodel to
#' @export
MMSaveMetaModel <- function(stratumGroupID, fileName) {
  metaModelMgr <- getMetaModelMgr()

  metaModelMgr$saveMetaModel(stratumGroupID, fileName)
}

#' Loads a single metamodel from a file on the server
#' @param stratumGroupID the stratum group for which we want the output types
#' @param fileName The filename we want to save the metamodel to
#' @export
MMLoadMetaModel <- function(stratumGroupID, fileName) {
  metaModelMgr <- getMetaModelMgr()

  metaModelMgr$loadMetaModel(stratumGroupID, fileName)
}
