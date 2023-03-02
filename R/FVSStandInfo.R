#'
#' Constructor for the OSMOutputRequestList class.
#'
#' @description This class holds data related to output requests and is used to prepare the ORList to be sent to the Simulate() call
#'
#' @return an S3 OSMOutputRequestList instance
#'
#'  TODO update documentation here
#'
#' @details
#'
#' The class contains the following methods: \cr
#' \itemize{
#'
#' \item \bold{addOutputRequest(statusClass, variable, aggregrationPatterns)} \cr
#' Allows adding an output request to the list. \cr
#' \item statusClass - The status class to be requested (character, typically one of "Alive", "Dead" )
#' \item variable - The variable to be requested (character, typically one of "Volume", "Biomass")
#' \item aggregrationPatterns - A list of aggregation patterns to be used for the request (named List where the names are the aggregation groups, and the list data are the species)
#'
#'
#' \item \bold{toJSONString()} \cr
#' Converts the current OSMOutputRequestList to a json string \cr
#' Return character vector
#' }
#'
#' @export
new_FVStandInfo <- function(ecoRegion, standAge, aspectDegrees, slopePct, elevationMeters) {
  me <- new.env(parent = emptyenv())
  class(me) <- c("FVStandInfo")
  me$SIList <- list(ecoRegion = as.character(ecoRegion), standAge = as.integer(standAge), aspectDegrees = as.numeric(aspectDegrees), slopePct = as.numeric(slopePct), elevationMeters = as.numeric(elevationMeters))

  delayedAssign("toJSONString",
                function() {
                  return (toJSON(me$SIList, auto_unbox=TRUE))
                },
                assign.env = me)

  return(me)
}


