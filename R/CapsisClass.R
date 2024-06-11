#'
#' Constructor for the CapsisClass class.
#'
#' @description This class is the interface to the CAPSIS WEB API.
#'
#' @param host the url of CAPSIS Web API (http://repicea.dynu.net by default)
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
new_CapsisClass <- function(host = "http://repicea.dynu.net") {
  me <- new.env(parent = emptyenv())
  class(me) <- c("CapsisClass")
  me$host <- host
  me$orList <- list()
  me$endpoint <- c("CapsisSimulation")
  delayedAssign("VariantList",
                function() {
                  url <- paste(me$host, me$endpoint, "VariantList", sep="/")

                  r <- httr::GET( url, query = list());

                  if (r$status_code != 200)
                  {
                    stop(httr::content(r, "text"))
                  }

                  result <- httr::content(r, "text")

                  resultJSON <- jsonlite::fromJSON(result)

                  return (resultJSON)
                },
                assign.env = me)

  delayedAssign("CapsisStatus",
                function() {
                  url <- paste(me$host, me$endpoint, "CapsisStatus", sep="/")

                  r <- httr::GET( url, query = list());

                  if (r$status_code != 200)
                  {
                    stop(httr::content(r, "text"))
                  }

                  result <- httr::content(r, "text")

                  resultJSON <- jsonlite::fromJSON(result)

                  return (resultJSON)
                },
                assign.env = me)

  delayedAssign("VariantScope",
                function(variant) {
                  url <- paste(me$host, me$endpoint, "VariantScope", sep="/")

                  r <- httr::GET( url, query = list(variant = variant));

                  if (r$status_code != 200)
                  {
                    stop(httr::content(r, "text"))
                  }

                  result <- httr::content(r, "text")

                  resultJSON <- jsonlite::fromJSON(result)

                  return (resultJSON)
                },
                assign.env = me)

  delayedAssign("VariantSpecies",
                function(variant, outputAsVector, speciesType) {
                  url <- paste(me$host, me$endpoint, "VariantSpecies", sep="/")

                  r <- httr::GET( url, query = list(variant = variant, type = speciesType));

                  if (r$status_code != 200)
                  {
                    stop(httr::content(r, "text"))
                  }

                  result <- httr::content(r, "text")

                  resultJSON <- jsonlite::fromJSON(result)

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
                function(variant) {
                  url <- paste(me$host, me$endpoint, "OutputRequestTypes", sep="/")

                  r <- httr::GET( url, query = list(variant = variant));

                  if (r$status_code != 200)
                  {
                    stop(httr::content(r, "text"))
                  }

                  result <- httr::content(r, "text")

                  resultJSON <- jsonlite::fromJSON(result)

                  return (resultJSON)
                },
                assign.env = me)


  delayedAssign("VariantFields",
                function(variant) {
                  url <- paste(me$host, me$endpoint, "VariantFields", sep="/")

                  r <- httr::GET( url, query = list(variant = variant));

                  if (r$status_code != 200)
                  {
                    stop(httr::content(r, "text"))
                  }

                  result <- httr::content(r, "text")

                  resultJSON <- jsonlite::fromJSON(result)

                  return (resultJSON)
                },
                assign.env = me)

  delayedAssign("Simulate",
                function(data, outputRequestList, variant, years, initialYear, isStochastic, nbRealizations, climateChange, applicationScale) {
                  outputRequestListJSON <- outputRequestList$toJSONString()
                  fieldMatches <- .GetFieldMatches(data, me$VariantFields(variant))
                  fieldMatchesJSON <- jsonlite::toJSON(fieldMatches, auto_unbox=TRUE)
                  csvData <- .ConvertDataFrameToCSVString(data)
                  url <- paste(me$host, me$endpoint, "Simulate", sep="/")
                  r <- httr::POST( url, query = list(years = as.character(years), variant = variant, initialYear = as.character(initialYear), isStochastic=as.logical(isStochastic), nbRealizations = as.integer(nbRealizations), climateChange = as.character(climateChange), applicationScale = as.character(applicationScale), fieldMatches=as.character(fieldMatchesJSON)), body = list(data=csvData, output=outputRequestListJSON), encode = "multipart" )

                  # if the CapsisWebAPI cannot launch this request, it informs us by returning code HTTP error 429 "too many requests".  in this case, return NULL
                  if (r$status_code == 429)
                    return (NULL)

                  if (r$status_code != 200)
                  {
                    stop(httr::content(r, "text"))
                  }

                  result <- httr::content(r, "text")

                  taskId <- jsonlite::fromJSON(result)

                  status <- me$TaskStatus(taskId)
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
                    status <- me$TaskStatus(taskId)
                  }

                  message(paste(status$code))

                  if (status$code != "COMPLETED") {
                    stop(paste("Error while processing with error code:", status$result))
                  }
                  return(status$result)
                },
                assign.env = me)

  delayedAssign("TaskStatus",
                function(taskID) {
                  url <- paste(me$host, me$endpoint, "TaskStatus", sep="/")
                  r <- httr::GET( url, query = list(taskID = as.character(taskID)))

                  if (r$status_code != 200)
                  {
                    stop(httr::content(r, "text"))
                  }

                  result <- httr::content(r, "text")

                  resultJSON <- jsonlite::fromJSON(result)

                  simResult <- NULL

                  if (resultJSON$status == "COMPLETED")
                  {
                    simResult <- new_SimulationResult(resultJSON)
                  } else if (resultJSON$status == "ERROR") {
                    return(list(code=resultJSON$status, result = resultJSON$errorMessage))
                  }

                  return(list(code=resultJSON$status, result = simResult))
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



