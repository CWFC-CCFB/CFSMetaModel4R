#'
#' Constructor for the CapsisClass class.
#'
#' @description This class is the interface to the CAPSIS WEB API.
#'
#' @return an S3 CapsisClass instance
#'
#' @details
#'
#' The class contains the following methods: \cr
#' \itemize{
#'
#' \item \bold{Simulate(data, outputRequestList, variant, years, initialYear, isStochastic, nbRealizations, climateChange, applicationScale)} \cr
#' Converts the current OutputRequestList to a json string \cr
#' \itemize{
#' \item data - a string in CSV format that represents the input data to run the simulation on
#' \item outputRequestList - An object of type OutputRequestList that contains the output data requested
#' \item variant - A string containing the variant name to use for simulation
#' \item years - An int containing the number of years the simulation should use
#' \item initialYear - The initial date of the simulation
#' \item isStochastic - A logical (true to enable stochastic simulation)
#' \item nbRealizations - An int containing the number of realizations in case of stochastic simulation [1-100]
#' \item climateChange - A string indicating the climate change scenarion [NO_CHANGE, RCP2_6, RCP4_5, RCP6_0, RCP8_5]
#' \item applicationScale - A string indicating the scale of the simulation [Stand, FMU]
#' }
#'
#' }
#'
#' @export
new_CapsisClass <- function(host) {
  me <- new.env(parent = emptyenv())
  class(me) <- c("CapsisClass")
  me$host <- host
  me$orList <- list()
  me$endpoint <- c("CapsisSimulation")
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
                      resultVector[i] = resultJSON[i]
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
                function(data, outputRequestList, variant, years, initialYear, isStochastic, nbRealizations, climateChange, applicationScale) {
                  outputRequestListJSON <- outputRequestList$toJSONString()
                  fieldMatches <- .GetFieldMatches(data, me$VariantFields(variant))
                  fieldMatchesJSON <- toJSON(fieldMatches, auto_unbox=TRUE)
                  csvData <- .ConvertDataFrameToCSVString(data)
                  url <- paste(me$host, me$endpoint, "Simulate", sep="/")
                  r <- POST( url, query = list(years = as.character(years), variant = variant, initialYear = as.character(initialYear), isStochastic=as.logical(isStochastic), nbRealizations = as.integer(nbRealizations), climateChange = as.character(climateChange), applicationScale = as.character(applicationScale), fieldMatches=as.character(fieldMatchesJSON)), body = list(data=csvData, output=outputRequestListJSON), encode = "multipart" )

                  # if the CapsisWebAPI cannot launch this request, it informs us by returning code HTTP error 429 "too many requests".  in this case, return NULL
                  if (r$status_code == 429)
                    return (NULL)

                  if (r$status_code != 200)
                  {
                    stop(content(r, "text"))
                  }

                  result <- content(r, "text")

                  taskId <- fromJSON(result)

                  status <- capsis$TaskStatus(taskId)
                  firstTime <- T
                  while (status$code == "IN_PROGRESS")
                  {
                    if (firstTime) {
                      messageStr <- status$code
                      firstTime <- F
                    } else {
                      messageStr <- "."
                    }
                    message(messageStr, appendLF = F)
                    Sys.sleep(2)
                    status <- capsis$TaskStatus(taskId)
                  }

                  message(paste(status$code))

                  if (status$code != "COMPLETED") {
                    stop(paste("Error while processing ", row$Filename, " with error code : ", status$code, sep=""))
                  }
                  return(status$result)
                },
                assign.env = me)

  delayedAssign("TaskStatus",
                function(taskID) {
                  outputRequestListJSON <- outputRequestList$toJSONString()
                  url <- paste(me$host, me$endpoint, "TaskStatus", sep="/")
                  r <- GET( url, query = list(taskID = as.character(taskID)))

                  if (r$status_code != 200)
                  {
                    stop(content(r, "text"))
                  }

                  result <- content(r, "text")

                  resultJSON <- fromJSON(result)

                  osmResult <- NULL

                  if (resultJSON$status == "COMPLETED")
                  {
                    osmResult <- new_SimulationResult(resultJSON)
                  }

                  return(list(code=resultJSON$status, result = osmResult))
                },
                assign.env = me)

  delayedAssign("PrepareScriptResult",
                function(fvsSimResults) {
                  .connectToJ4R()
                  dfAggregated <- fvsSimResults$AggregateResults()

                  dataSet <- J4R::callJavaMethod("repicea.simulation.scriptapi.ScriptResult", "createEmptyReducedDataSet")
                  for (i in 1:nrow(dfAggregated))
                  {
                    jarray <- J4R::createJavaObject("java.lang.Object", ncol(dfAggregated), isArray = TRUE)
                    J4R::setValueInArray(jarray, as.character(dfAggregated[i,]))
                    dataSet$addObservation(jarray)
                  }

                  climateChangeScenario <- J4R::callJavaMethod("repicea.simulation.climate.REpiceaClimateGenerator$ClimateChangeScenarioHelper", "getClimateChangeScenarioFromString", fvsSimResults$climateChangeScenario)

                  scriptResult <- J4R::createJavaObject("repicea.simulation.scriptapi.ScriptResult", fvsSimResults$nbRealizations, fvsSimResults$nbPlots, climateChangeScenario, fvsSimResults$growthModel, dataSet)

                  return(scriptResult)
                },
                assign.env = me)

  return(me)
}


.ConvertDataFrameToCSVString <- function(dataFrameInstance) {
  outputVector <- sapply(1:nrow(dataFrameInstance), function(i) {paste(dataFrameInstance[i,], collapse= ",")})
  outputVector <- c(paste(colnames(dataFrameInstance), collapse= ","), outputVector)
  outputString <- paste(outputVector, collapse = "\r\n")
  return(outputString)
}


.GetFieldMatches <- function(dataFrameObj, fieldList) {
  availableFields <- colnames(dataFrameObj)
  matches <- c()
  for (i in 1:nrow(fieldList)) {
    fieldName <- fieldList[i,"name"]
    index <- which(availableFields == fieldName)
    if (length(index) == 0) {
      if (fieldList[i, "isOptional"]) {
        matches <- c(matches, -1)
      } else { # field is mandatory
        stop(paste("Field", fieldName, "cannot be found! Check the required field using capsis$VariantFields(variant) function."))
      }
    } else {
      matches <- c(matches, index - 1)
    }
  }
  return(matches)
}




#'
#' Constructor for the OSMResult class.
#'
#' @description This class extracts data received from JSON OSM simulate() calls to allow later scriptResult conversion
#'
#' @return an S3 OSMResult instance
#'
#'  TODO update documentation here
#'
#' @details
#'
#' The class contains the following methods: \cr
#' \itemize{
#'
#' \item \bold{AggregateResults()} \cr
#' Aggregates results to return only the average value for all plots \cr
#'
#' }
#'
#' @export
new_OSMResult <- function(resultJSON)
{
  me <- new.env(parent = emptyenv())
  class(me) <- c("OSMResult")
  me$dataSet <- read.csv(text = resultJSON$csvReport)
  me$nbRealizations <- resultJSON$nbRealizations
  me$nbPlots <- resultJSON$nbPlots
  me$climateChangeScenario <- resultJSON$climateChangeScenario
  me$growthModel <- resultJSON$growthModel

  delayedAssign("AggregateResults",
                function() {
                  return (aggregate(Estimate~DateYr+timeSinceInitialDateYear+OutputType, me$dataSet, FUN="mean"))
                },
                assign.env = me)

  return (me)
}
