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
