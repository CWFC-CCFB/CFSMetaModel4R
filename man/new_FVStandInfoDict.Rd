% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FVSStandInfo.R
\name{new_FVStandInfoDict}
\alias{new_FVStandInfoDict}
\title{Constructor for the OSMOutputRequestList class.}
\usage{
new_FVStandInfoDict()
}
\value{
an S3 OSMOutputRequestList instance

 TODO update documentation here
}
\description{
This class holds data related to output requests and is used to prepare the ORList to be sent to the Simulate() call
}
\details{
The class contains the following methods: \cr
\itemize{

\item \bold{addOutputRequest(statusClass, variable, aggregrationPatterns)} \cr
Allows adding an output request to the list. \cr
\item statusClass - The status class to be requested (character, typically one of "Alive", "Dead" )
\item variable - The variable to be requested (character, typically one of "Volume", "Biomass")
\item aggregrationPatterns - A list of aggregation patterns to be used for the request (named List where the names are the aggregation groups, and the list data are the species)


\item \bold{toJSONString()} \cr
Converts the current OSMOutputRequestList to a json string \cr
Return character vector
}
}
