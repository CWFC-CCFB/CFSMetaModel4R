#'
#' Constructor for the OutputRequestList class.
#'
#' @description This class holds data related to output
#' requests and is used to prepare the list to be sent
#' to the Simulate() call.
#'
#' @return an S3 OutputRequestList instance
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
new_OutputRequestList <- function() {
  me <- new.env(parent = emptyenv())
  class(me) <- c("OutputRequestList")
  me$orList <- list()
  delayedAssign("addOutputRequest",
                function(requestType, aggregrationPatterns) {

                  or <- list()
                  or[["requestType"]] <- requestType
                  or[["aggregationPatterns"]] <- aggregrationPatterns

                  me$orList[[length(me$orList) + 1]] <- or

                },
                assign.env = me)

  delayedAssign("toJSONString",
                function() {
                  return (toJSON(me$orList, auto_unbox=TRUE))
                },
                assign.env = me)

  return(me)
}


