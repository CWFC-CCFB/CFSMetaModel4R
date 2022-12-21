rm(list=ls())
#
# df <- read.csv("./tests/OSM_ThreeStandList.csv")
#
# SurveyIDList <- unique(df$SurveyID)
#
#
# myCSVString <- convertDataFrameToCSVString(df, SurveyIDList[1])
#
#
# (v <- structure(10*(5:8), names = LETTERS[1:4]))
# f2 <- function(x, y) outer(rep(x, length.out = 3), y)
# (a2 <- sapply(v, f2, y = 2*(1:5), simplify = "array"))

#OSMThreeStandList <- read.csv("./tests/OSM_ThreeStandList.csv")
#save(OSMThreeStandList, file = "./data/OSM_ThreeStandList.RData")

#csvDataFileName <- "./tests/OSM_ThreeStandList.csv"
#csvData <- readChar(csvDataFileName, file.info(csvDataFileName)$size)

library(Capsis4R)
library(httr)
library(jsonlite)

data("OSMThreeStandList")
csvData <- convertDataFrameToCSVString(OSMThreeStandList)

outputRequestFileName <- "./tests/outputRequestList.json"
outputRequest <- readChar(outputRequestFileName, file.info(outputRequestFileName)$size)

jsonTest <- fromJSON(outputRequest)

or <- list()
or[["requestType"]] <- list(statusClass = "Alive", variable = "Volume")
or[["aggregationPatterns"]] <- list(Coniferous = c("BF", "LX"), Broadleaved = c("BE", "ST"))

orList <- list()
orList[[1]] <- or

orListString <- toJSON(orList)

r <- POST( "https://localhost:7032/OSMSimulation/Simulate", query = list(years = "10", variant = "Acadian", ypc = "2"), body = list(data=csvData, output=orListString), encode = "multipart" );
#r <- POST( "https://localhost:7032/OSMSimulation/Simulate", query = list(years = "10", variant = "Acadian", ypc = "2"), body = list(data=csvData), encode = "multipart" );

if (r$status_code == 200)
{
  result <- content(r, "text")

  resultJSON <- fromJSON(result)

  df <- read.csv(text = resultJSON$csvReport)

  print(df)
}
