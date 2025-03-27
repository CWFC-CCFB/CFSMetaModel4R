#' #'
#' #' A plot with two trees that can be passed to FVS Web API
#' #'
#' #' @docType data
#' #'
#' #' @usage data(FVSTreeListTwoTreesOnePlot)
#' #'
#' #' @keywords datasets
#' #'
#' #' @examples
#' #' data(FVSTreeListTwoTreesOnePlot)
#' "FVSTreeListTwoTreesOnePlot"
#'
#'
#'
#' #'
#' #' Constructor for the FVSClass class.
#' #'
#' #' @description This class is the interface to the OSM http server.
#' #' @param host the url of FVS Web API (http://repicea.dynu.net by default)
#' #' @return an S3 FVSClass instance
#' #'
#' #'  TODO update documentation here
#' #'
#' #' @details
#' #'
#' #' The class contains the following methods: \cr
#' #' \itemize{
#' #'
#' #' \item \bold{ConvertDataFrameToCSVString(dataFrameInstance)} \cr
#' #' Utility method to convert a dataframe to CSV string to be sent as input data to the server \cr
#' #' \item dataFrameInstance - The status class to be requested (character, typically one of "Alive", "Dead" )
#' #' \item variable - The variable to be requested (character, typically one of "Volume", "Biomass")
#' #' \item aggregrationPatterns - A list of aggregation patterns to be used for the request (named List where the names are the aggregation groups, and the list data are the species)
#' #'
#' #'
#' #' \item \bold{Simulate(data, outputRequestList, variant, years, ypc)} \cr
#' #' Converts the current OSMOutputRequestList to a json string \cr
#' #' \item data - a string in CSV format that represents the input data to run the simulation on
#' #' \item outputRequestList - An object of type OSMOutputRequestList that contains the output data requested
#' #' \item variant - A string containing the variant name to use for simulation
#' #' \item years - An int containing the number of years the simulation should use
#' #' \item ypc - An int containing the number of years per cycle the simulation should use
#'
#' #' Return character vector
#' #' }
#' #'
#' #' @export
#' new_FVSClass <- function(host = "https://repicea.dynu.net") {
#'   me <- new.env(parent = emptyenv())
#'   class(me) <- c("FVSClass")
#'   me$host <- host
#'   me$orList <- list()
#'   me$endpoint <- c("FVSSimulation")
#'   delayedAssign("ConvertDataFrameToCSVString",
#'                 function(dataFrameInstance) {
#'                   outputVector <- sapply(1:nrow(dataFrameInstance), function(i) {paste(dataFrameInstance[i,], collapse= ",")})
#'                   outputVector <- c(paste(colnames(dataFrameInstance), collapse= ","), outputVector)
#'                   outputString <- paste(outputVector, collapse = "\r\n")
#'                   return(outputString)
#'                 },
#'                 assign.env = me)
#'
#'   delayedAssign("VariantList",
#'                 function() {
#'                   url <- paste(me$host, me$endpoint, "VariantList", sep="/")
#'
#'                   r <- httr::GET( url, query = list());
#'
#'                   if (r$status_code != 200)
#'                   {
#'                     stop(httr::content(r, "text"))
#'                   }
#'
#'                   result <- httr::content(r, "text")
#'
#'                   resultJSON <- jsonlite::fromJSON(result)
#'
#'                   return (resultJSON)
#'                 },
#'                 assign.env = me)
#'
#'   delayedAssign("VariantSpecies",
#'                 function(variant, outputAsVector, speciesType) {
#'                   url <- paste(me$host, me$endpoint, "VariantSpecies", sep="/")
#'
#'                   r <- httr::GET( url, query = list(variant = variant, type = speciesType));
#'
#'                   if (r$status_code != 200)
#'                   {
#'                     stop(httr::content(r, "text"))
#'                   }
#'
#'                   result <- httr::content(r, "text")
#'
#'                   resultJSON <- jsonlite::fromJSON(result)
#'
#'                   if (outputAsVector)
#'                   {
#'                     resultVector <- vector(mode="character", length=length(resultJSON))
#'                     for (i in 1:length(resultJSON))
#'                     {
#'                       resultVector[i] = resultJSON[[i]]$key
#'                     }
#'
#'                     return (resultVector)
#'                   }
#'                   else
#'                   {
#'                     return(resultJSON)
#'                   }
#'                 },
#'                 assign.env = me)
#'
#'   delayedAssign("OutputRequestTypes",
#'                 function() {
#'                   url <- paste(me$host, me$endpoint, "OutputRequestTypes", sep="/")
#'
#'                   r <- httr::GET( url, query = list());
#'
#'                   if (r$status_code != 200)
#'                   {
#'                     stop(httr::content(r, "text"))
#'                   }
#'
#'                   result <- httr::content(r, "text")
#'
#'                   resultJSON <- jsonlite::fromJSON(result)
#'
#'                   return (resultJSON)
#'                 },
#'                 assign.env = me)
#'
#'
#'   delayedAssign("VariantFields",
#'                 function(variant) {
#'                   url <- paste(me$host, me$endpoint, "VariantFields", sep="/")
#'
#'                   r <- httr::GET( url, query = list(variant = variant));
#'
#'                   if (r$status_code != 200)
#'                   {
#'                     stop(httr::content(r, "text"))
#'                   }
#'
#'                   result <- httr::content(r, "text")
#'
#'                   resultJSON <- jsonlite::fromJSON(result)
#'
#'                   return (resultJSON)
#'                 },
#'                 assign.env = me)
#'
#'   delayedAssign("Simulate",
#'                 function(data, outputRequestList, variant, standInfoDict, years, ypc, initialYear) {
#'                   outputRequestListJSON <- outputRequestList$toJSONString()
#'                   standInfoDictJSON <- standInfoDict$toJSONString()
#'                   csvData <- me$ConvertDataFrameToCSVString(data)
#'                   url <- paste(me$host, me$endpoint, "Simulate", sep="/")
#'                   r <- httr::POST( url, query = list(years = as.character(years), variant = variant, ypc = as.character(ypc), initialYear = as.character(initialYear)), body = list(data=csvData, output=outputRequestListJSON, standInfoDict=standInfoDictJSON), encode = "multipart" );
#'
#'                   if (r$status_code != 200)
#'                   {
#'                     stop(httr::content(r, "text"))
#'                   }
#'
#'                   result <- httr::content(r, "text")
#'
#'                   resultJSON <- jsonlite::fromJSON(result)
#'
#'                   osmResult <- new_SimulationResult(resultJSON)
#'
#'                   return(osmResult)
#'                 },
#'                 assign.env = me)
#'
#'   delayedAssign("PrepareScriptResult",
#'                 function(fvsSimResults) {
#'                   .connectToJ4R()
#'                   dfAggregated <- fvsSimResults$AggregateResults()
#'
#'                   dataSet <- J4R::callJavaMethod("repicea.simulation.scriptapi.ScriptResult", "createEmptyReducedDataSet")
#'                   for (i in 1:nrow(dfAggregated))
#'                   {
#'                     jarray <- J4R::createJavaObject("java.lang.Object", ncol(dfAggregated), isArray = TRUE)
#'                     J4R::setValueInArray(jarray, as.character(dfAggregated[i,]))
#'                     dataSet$addObservation(jarray)
#'                   }
#'
#'                   climateChangeScenario <- J4R::callJavaMethod("repicea.simulation.climate.REpiceaClimateGenerator$ClimateChangeScenarioHelper", "getClimateChangeScenarioFromString", fvsSimResults$climateChangeScenario)
#'
#'                   scriptResult <- J4R::createJavaObject("repicea.simulation.scriptapi.ScriptResult", fvsSimResults$nbRealizations, fvsSimResults$nbPlots, climateChangeScenario, fvsSimResults$growthModel, dataSet)
#'
#'                   return(scriptResult)
#'                 },
#'                 assign.env = me)
#'
#'   return(me)
#' }
#'
