#'
#' A list of three plots that can be passed to OSM Web API
#'
#' @docType data
#'
#' @usage data(OSMThreeStandList)
#'
#' @keywords datasets
#'
#' @examples
#' data(OSMThreeStandList)
"OSMThreeStandList"


#'
#' Constructor for the OSMClass class.
#'
#' @description This class is the interface to the OSM http server.
#'
#' @return an S3 OSMClass instance
#'
#'  TODO update documentation here
#'
#' @details
#'
#' The class contains the following methods: \cr
#' \itemize{
#'
#' \item \bold{ConvertDataFrameToCSVString(dataFrameInstance)} \cr
#' Utility method to convert a dataframe to CSV string to be sent as input data to the server \cr
#' \item dataFrameInstance - The status class to be requested (character, typically one of "Alive", "Dead" )
#' \item variable - The variable to be requested (character, typically one of "Volume", "Biomass")
#' \item aggregrationPatterns - A list of aggregation patterns to be used for the request (named List where the names are the aggregation groups, and the list data are the species)
#'
#'
#' \item \bold{Simulate(data, outputRequestList, variant, years, ypc)} \cr
#' Converts the current OSMOutputRequestList to a json string \cr
#' \item data - a string in CSV format that represents the input data to run the simulation on
#' \item outputRequestList - An object of type OSMOutputRequestList that contains the output data requested
#' \item variant - A string containing the variant name to use for simulation
#' \item years - An int containing the number of years the simulation should use
#' \item ypc - An int containing the number of years per cycle the simulation should use

#' Return character vector
#' }
#'
#' @export
new_OSMClass <- function(host) {
  me <- new.env(parent = emptyenv())
  class(me) <- c("OSMClass")
  me$host <- host
  me$orList <- list()
  me$endpoint <- c("OSMSimulation")
  delayedAssign("ConvertDataFrameToCSVString",
                function(dataFrameInstance) {
                  outputVector <- sapply(1:nrow(dataFrameInstance), function(i) {paste(dataFrameInstance[i,], collapse= ",")})
                  outputVector <- c(paste(colnames(dataFrameInstance), collapse= ","), outputVector)
                  outputString <- paste(outputVector, collapse = "\r\n")
                  return(outputString)
                },
                assign.env = me)

  delayedAssign("VariantList",
                function() {
                  url <- paste(me$host, me$endpoint, "VariantList", sep="/")

                  r <- GET( url, query = list());

                  if (r$status_code != 200)
                  {
                    stop(content(r, "text"))
                  }

                  result <- content(r, "text")

                  resultJSON <- fromJSON(result)

                  return (resultJSON)
                },
                assign.env = me)

  delayedAssign("VariantSpecies",
                function(variant, outputAsVector, speciesType) {
                  url <- paste(me$host, me$endpoint, "VariantSpecies", sep="/")

                  r <- GET( url, query = list(variant = variant, type = speciesType));

                  if (r$status_code != 200)
                  {
                    stop(content(r, "text"))
                  }

                  result <- content(r, "text")

                  resultJSON <- fromJSON(result)

                  if (outputAsVector)
                  {
                    resultVector <- vector(mode="character", length=length(resultJSON))
                    for (i in 1:length(resultJSON))
                    {
                      resultVector[i] = resultJSON[[i]]$key
                    }

                    return (resultVector)
                  }
                  else
                  {
                    return(resultJSON)
                  }
                },
                assign.env = me)

  delayedAssign("OutputRequestTypes",
                function() {
                  url <- paste(me$host, me$endpoint, "OutputRequestTypes", sep="/")

                  r <- GET( url, query = list());

                  if (r$status_code != 200)
                  {
                    stop(content(r, "text"))
                  }

                  result <- content(r, "text")

                  resultJSON <- fromJSON(result)

                  return (resultJSON)
                },
                assign.env = me)


  delayedAssign("VariantFields",
                function(variant) {
                  url <- paste(me$host, me$endpoint, "VariantFields", sep="/")

                  r <- GET( url, query = list(variant = variant));

                  if (r$status_code != 200)
                  {
                    stop(content(r, "text"))
                  }

                  result <- content(r, "text")

                  resultJSON <- fromJSON(result)

                  return (resultJSON)
                },
                assign.env = me)

  delayedAssign("Simulate",
                function(data, outputRequestList, variant, years, ypc) {
                  outputRequestListJSON <- outputRequestList$toJSONString()
                  csvData <- me$ConvertDataFrameToCSVString(data)
                  url <- paste(me$host, me$endpoint, "Simulate", sep="/")
                  r <- POST( url, query = list(years = as.character(years), variant = variant, ypc = as.character(ypc)), body = list(data=csvData, output=outputRequestListJSON), encode = "multipart" );

                  if (r$status_code != 200)
                  {
                    stop(content(r, "text"))
                  }

                  result <- content(r, "text")

                  resultJSON <- fromJSON(result)

                  osmResult <- new_OSMResult(resultJSON)

                  return(osmResult)
                },
                assign.env = me)

  delayedAssign("PrepareScriptResult",
                function(osmSimResults) {
                  .connectToJ4R() # must be connected to J4R to create a ScriptResult instance

                  dfAggregated <- osmSimResults$AggregateResults()

                  dataSet <- J4R::callJavaMethod("repicea.simulation.scriptapi.ScriptResult", "createEmptyReducedDataSet")
                  for (i in 1:nrow(dfAggregated))
                  {
                    jarray <- J4R::createJavaObject("java.lang.Object", ncol(dfAggregated), isArray = TRUE)
                    J4R::setValueInArray(jarray, as.character(dfAggregated[i,]))
                    dataSet$addObservation(jarray)
                  }

                  climateChangeScenario <- J4R::callJavaMethod("repicea.simulation.climate.REpiceaClimateGenerator$ClimateChangeScenarioHelper", "getClimateChangeScenarioFromString", osmSimResults$climateChangeScenario)

                  scriptResult <- J4R::createJavaObject("repicea.simulation.scriptapi.ScriptResult", osmSimResults$nbRealizations, osmSimResults$nbPlots, climateChangeScenario, osmSimResults$growthModel, dataSet)

                  return(scriptResult)
                },
                assign.env = me)

  return(me)
}

