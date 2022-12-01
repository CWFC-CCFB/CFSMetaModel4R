#'
#' Constructor for the MetaModelManager class.
#'
#' @description A MetaModelManager instance contains a pointer to a Java instance. It
#' also implements many methods which are described below.
#'
#' @return an S3 MetaModelManager instance
#'
#'  TODO update documentation here
#'
#' @details
#'
#' The class contains the following methods: \cr
#' \itemize{
#'
#' \item \bold{getVersion()} \cr
#' Provide the version numbers CAPSIS and Capsis4R \cr
#' Return a List
#'
#' \item \bold{getModelDataFields()} \cr
#' Enumerate the model data fields expected by the server.  Use this to create the index array send through the setFieldMatches method. \cr
#' Return A vector of strings containing all the fields as triplets separated by ";" ex : "PLOT (mandatory); String; Match: 0       1"
#'
#' \item \bold{setInitialParameters(initialDateYear, stochasticMode, numberOfRealizations, applicationScale, climateChangeOption)} \cr
#' Set initial model parameters.  Must be called prior to calling runSimulation. \cr
#' Parameters are \cr
#' \itemize{
#' \item initialDateYear - The initial year to be used for simulation (numeric)
#' \item stochasticMode - Enable stochastic mode simulation (1 = enabled, 0 = disable)
#' \item numberOfRealizations - The number of realizations to be used for simulation (numeric)
#' \item applicationScale - The application scale, possible values are ["FMU", "Stand"]
#' \item climateChangeOption - A climate change option, possible values are ["NoChange", "Plus2Degrees", "Plus4Degrees", "Plus6Degrees"]
#' }
#' Return nothing
#'
#' \item \bold{setEvolutionParameters(finalDateYear)} \cr
#' Set evolution parameters.  Must be called prior to calling runSimulation \cr
#' Parameters are \cr
#' \itemize{
#' \item finalDateYear - The final year to be used for simulation
#' }
#' Return nothing
#'
#' \item \bold{getSpeciesOfType(types)} \cr
#' Get the list of species of the specified type. \cr
#' Parameters are \cr
#' \itemize{
#' \item types - The types to get the list of species for (vector)
#' }
#' Return a vector containing all the names of the species for all input types
#'
#' \item \bold{registerOutputRequest(request, aggregationPatterns)} \cr
#' Register a request for a particular output of the simulation. \cr
#' Parameters are \cr
#' \itemize{
#' \item request - The request to register
#' \item aggregationPatterns - The aggregation patterns to register
#' }
#' Return nothing
#'
#' \item \bold{setFieldMatches(matches)} \cr
#' Set field matches for input data.  Must be called prior to calling sendData. \cr
#' Parameters are \cr
#' \itemize{
#' \item matches - A vector of integere specifying the field match order to be used by the server when interpreting CBSendData calls.
#' }
#' Return TRUE if operation succeed
#'
#' \item \bold{sendData(data)} \cr
#' Send data to the CAPSIS Server.  Must be called prior to calling runSimulation. \cr
#' Parameters are \cr
#' \itemize{
#' \item data - A data.frame object whose rows are to be read by CAPSIS
#' }
#' Return nothing
#'
#' \item \bold{runSimulation()} \cr
#' Run the simulation on the CAPSIS model \cr
#' Return the simulation results
#'
#' \item \bold{closeProject()} \cr
#' Close the active project and free its resources \cr
#' Return nothing
#' }
#'
#' @export
new_MetaModelManager <- function() {
  me <- new.env(parent = emptyenv())
  class(me) <- c("MetaModelManager")
  me$.metaModelManager <- J4R::createJavaObject("repicea.simulation.metamodel.MetaModelManager")
  delayedAssign("createMetaModel",
                function(stratumGroupID, geoDomain, dataSource) {
                  me$.metaModelManager$createMetaModel(stratumGroupID, geoDomain, dataSource)
                  return(invisible(NULL))
                },
                assign.env = me)

  delayedAssign("get",
                function(stratumGroupID) {
                  return (me$.metaModelManager$get(stratumGroupID))
                },
                assign.env = me)

  delayedAssign("clear",
                function() {
                  me$.metaModelManager$clear()
                  return(invisible(NULL))
                },
                assign.env = me)

  delayedAssign("addSimulationResult",
                function(stratumGroupID, initialAgeYr, simulationResult) {
                  me$.metaModelManager$addDataset(stratumGroupID, as.integer(initialAgeYr), simulationResult)
                  return(invisible(NULL))
                },
                assign.env = me)

  delayedAssign("fitMetaModel",
                function(outputType) {
                  me$.metaModelManager$fitMetaModels(outputType)
                  return(invisible(NULL))
                },
                assign.env = me)

  delayedAssign("getPredictions",
                function(stratumGroupID, ageYr, timeSinceInitialDateYr, varianceOutputType) {
                  ageYrArray <- J4R::as.JavaArray(as.integer(ageYr))
                  varianceOutputEnum <- J4R::createJavaObject("repicea.simulation.metamodel.MetaModel$PredictionVarianceOutputType", varianceOutputType)
                  return (me$.metaModelManager$get(stratumGroupID)$getPredictions(ageYrArray, as.integer(timeSinceInitialDateYr), varianceOutputEnum))
                },
                assign.env = me)

  delayedAssign("getMonteCarloPredictions",
                function(stratumGroupID, ageYr, timeSinceInitialDateYr, nbSubjects, nbRealizations) {
                  return (me$.metaModelManager$get(stratumGroupID)$getMonteCarloPredictions(as.integer(ageYr), as.integer(timeSinceInitialDateYr), as.integer(nbSubjects), as.integer(nbRealizations)))
                },
                assign.env = me)

  delayedAssign("getPossibleOutputTypes",
                function(stratumGroupID) {
                  return (J4R::getAllValuesFromListObject(me$.metaModelManager$getPossibleOutputTypes(stratumGroupID)))
                },
                assign.env = me)

  delayedAssign("getSelectedOutputType",
                function(stratumGroupID) {
                  return(me$.metaModelManager$getSelectedOutputType(stratumGroupID))
                },
                assign.env = me)

  delayedAssign("getStratumGroups",
                function() {

                  return (J4R::getAllValuesFromListObject(me$.metaModelManager$getStratumGroups()))
                },
                assign.env = me)

  delayedAssign("getMetaModelResult",
                function(stratumGroupID) {
                  return(me$.metaModelManager$getMetaModelResult(stratumGroupID))
                },
                assign.env = me)

  delayedAssign("save",
                function(fileName) {
                  me$.metaModelManager$save(fileName)
                },
                assign.env = me)

  delayedAssign("load",
                function(fileName) {
                  me$.metaModelManager$save(fileName)
                },
                assign.env = me)

  delayedAssign("saveMetaModel",
                function(stratumGroupID, fileName) {
                  me$.metaModelManager$saveMetaModel(stratumGroupID, fileName)
                },
                assign.env = me)

  delayedAssign("loadMetaModel",
                function(stratumGroupID, fileName) {
                  me$.metaModelManager$loadMetaModel(stratumGroupID, fileName)
                },
                assign.env = me)
  return(me)
}


