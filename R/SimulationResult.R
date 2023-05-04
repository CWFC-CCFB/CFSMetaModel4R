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
new_SimulationResult <- function(resultJSON)
{
  me <- new.env(parent = emptyenv())
  class(me) <- c("SimulationResult")

  me$mustAggregate <- FALSE
  if (!is.null(resultJSON[["csvReport"]]))
  {# OSM output
    me$mustAggregate <- TRUE
    me$dataSet <- read.csv(text = resultJSON$csvReport)

    me$nbRealizations <- resultJSON$nbRealizations
    me$nbPlots <- resultJSON$nbPlots
    me$climateChangeScenario <- resultJSON$climateChangeScenario
    me$growthModel <- resultJSON$growthModel
    me$outputTypes <- resultJSON$outputTypes
  }
  else if (!is.null(resultJSON[["status"]]))
  {# Capsis output
    if (resultJSON$status == "COMPLETED")
    {
      me$dataSet <- data.frame(Reduce(rbind,resultJSON$result$dataset$observations$values))
      colnames(me$dataSet) <- resultJSON$result$dataset$fieldNames
      for (i in 1:length(resultJSON$result$dataset$fieldNames))
      {
        fieldType <- resultJSON$result$dataset$fieldTypes[[i]]
        fieldName <-resultJSON$result$dataset$fieldNames[[i]]
        if (fieldType == "java.lang.Integer")
        {
          me$dataSet[[fieldName]] = as.integer(me$dataSet[[fieldName]])
        }
        else if (fieldType == "java.lang.Double")
        {
          me$dataSet[[fieldName]] = as.numeric(me$dataSet[[fieldName]])
        }
      }
    }

    me$nbRealizations <- resultJSON$result$nbRealizations
    me$nbPlots <- resultJSON$result$nbPlots
    me$climateChangeScenario <- resultJSON$result$climateChangeScenario
    me$growthModel <- resultJSON$result$growthModel
    me$outputTypes <- resultJSON$result$outputTypes
  }

  #aggregate results if needed
  if (me$mustAggregate)
  {
    me$dataSet <- aggregate(Estimate~DateYr+timeSinceInitialDateYear+OutputType, me$dataSet, FUN="mean")
  }

  return (me)
}
