#'
#' Constructor for the SimulationResult class.
#'
#' @description This class converts the JSON result of a simulate call
#' into a SimulationResult instance
#'
#' @param resultJSON a JSON result from a simulate call
#'
#' @return an S3 SimulationResult instance
#'
#' @details
#' The constructor automatically detects whether the
#' simulation results need to be aggregated.
#'
#' @export
new_SimulationResult <- function(resultJSON) {
  me <- new.env(parent = emptyenv())
  class(me) <- c("SimulationResult")

  if (resultJSON$growthModel == "Natura2014") { # Natura2014 output
    me$dataSet <- resultJSON$dataSet
  } else if (resultJSON$growthModel == "OSM") { # OSM output
    me$dataSet <- utils::read.csv(text = resultJSON$csvReport)
    me$dataSet <- stats::aggregate(Estimate~DateYr+timeSinceInitialDateYear+OutputType, me$dataSet, FUN="mean")
  } else if (resultJSON$growthModel %in% c("Artemis2009", "Artemis2014")) { # A Capsis output
    me$dataSet <- data.frame(Reduce(rbind,resultJSON$dataset$observations$values))
    colnames(me$dataSet) <- resultJSON$dataset$fieldNames
    for (i in 1:length(resultJSON$dataset$fieldNames)) {
      fieldType <- resultJSON$dataset$fieldTypes[[i]]
      fieldName <-resultJSON$dataset$fieldNames[[i]]
      if (fieldType == "java.lang.Integer") {
        me$dataSet[[fieldName]] = as.integer(me$dataSet[[fieldName]])
      } else if (fieldType == "java.lang.Double") {
        me$dataSet[[fieldName]] = as.numeric(me$dataSet[[fieldName]])
      }
    }
  } else {
    stop("The model ", resultJSON$growthModel, " is not recognized!")
  }

  me$nbRealizations <- as.integer(resultJSON$nbRealizations)
  me$nbPlots <- as.integer(resultJSON$nbPlots)
  me$climateChangeScenario <- resultJSON$climateChangeScenario
  me$growthModel <- resultJSON$growthModel
  me$outputTypes <- resultJSON$outputTypes

  return (me)
}
