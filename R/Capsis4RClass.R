#'
#' Constructor for the CapsisScript class.
#'
#' @description A CapsisScript instance contains a pointer to a Java instance. It
#' also implements many methods which are described below.
#'
#' @return an S3 CapsisScript instance
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
new_CapsisScript <- function(className) {
  me <- new.env(parent = emptyenv())
  class(me) <- c("CapsisScript")
  me$.capsisScript <- J4R::createJavaObject(className)

  delayedAssign("getVersion",
                #### getVersion ####
                function() {
                  ver = list("Capsis4R" = toString(packageVersion("Capsis4R")), "Capsis" = me$.capsisScript$getCapsisVersion())
                  return (ver)
                },
                assign.env = me)

  delayedAssign("getModelDataFields",
                #### getModelDataFields ####
                function() {
                  fields <- me$.capsisScript$getFieldDescriptions()
                  values <- J4R::getAllValuesFromListObject(fields)
                  return (values$toString())
                },
                assign.env = me)

  delayedAssign("setInitialParameters",
                #### setInitialParameters ####
                function(initialDateYear, stochasticMode, numberOfRealizations, applicationScale, climateChangeOption) {
                  applicationScaleEnum <- J4R::createJavaObject("repicea.simulation.ApplicationScaleProvider$ApplicationScale", applicationScale)
                  me$.capsisScript$setInitialParameters(as.integer(initialDateYear), stochasticMode != 0, as.integer(numberOfRealizations), applicationScaleEnum, climateChangeOption)
                },
                assign.env = me)

  delayedAssign("setEvolutionParameters",
                #### setEvolutionParameters ####
                function(finalDateYear) {
                  me$.capsisScript$setEvolutionParameters(as.integer(finalDateYear))
                },
                assign.env = me)

  delayedAssign("getSpeciesOfType",
                #### getSpeciesOfType ####
                function(types) {
                  speciesTypeArray <- J4R::createJavaObject("repicea.simulation.covariateproviders.treelevel.SpeciesTypeProvider$SpeciesType", length(as.vector(types)), isArray=TRUE)
                  enums <- J4R::createJavaObject("repicea.simulation.covariateproviders.treelevel.SpeciesTypeProvider$SpeciesType", types)
                  J4R::setValueInArray(speciesTypeArray, enums)
                  species <- me$.capsisScript$getSpeciesOfType(speciesTypeArray)
                  return(species)
                },
                assign.env = me)

  delayedAssign("registerOutputRequest",
                #### registerOutputRequest ####
                function(request, aggregationPatterns) {
                  me$.capsisScript$registerOutputRequest(request, aggregationPatterns)
                },
                assign.env = me)

  delayedAssign("setFieldMatches",
                #### setFieldMatches ####
                function(matches) {
                  matchesJavaArray <- J4R::as.JavaArray(as.integer(matches))
                  result <- me$.capsisScript$setFieldMatches(matchesJavaArray)
                  if (result != TRUE)
                    stop("CBSetFieldMatches failed")
                },
                assign.env = me)

  delayedAssign("sendData",
                #### sendData ####
                function(data) {
                  for(o in data) {
                    tempArray <- J4R::createJavaObject("java.lang.Object", length(o), isArray = TRUE)
                    J4R::setValueInArray(tempArray, o)
                    me$.capsisScript$addRecord(tempArray)
                  }
                },
                assign.env = me)

  delayedAssign("runSimulation",
                #### runSimulation ####
                function() {
                  return(me$.capsisScript$runSimulation())
                },
                assign.env = me)

  delayedAssign("closeProject",
                #### closeProject ####
                function() {
                  me$.capsisScript$closeProject()
                },
                assign.env = me)

  return(me)
}


