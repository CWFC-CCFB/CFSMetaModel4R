#'
#' Input data for OSM
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


#' @export
convertDataFrameToCSVString <- function(dataFrameInstance) {
  outputVector <- sapply(1:nrow(dataFrameInstance), function(i) {paste(dataFrameInstance[i,], collapse= ",")})
  outputVector <- c(paste(colnames(dataFrameInstance), collapse= ","), outputVector)
  outputString <- paste(outputVector, collapse = "\r\n")
  return(outputString)
}
