rm(list=ls())
convertDataFrameToCSVString <- function(dataFrameInstance, SurveyID) {
  d <- dataFrameInstance[which(dataFrameInstance$SurveyID == SurveyID),]
  outputVector <- sapply(1:nrow(d), function(i) {paste(d[i,], collapse= ",")})
  outputVector <- c(paste(colnames(d), collapse= ","), outputVector)
  outputString <- paste(outputVector, collapse = "\r\n")
  return(outputString)
}

df <- read.csv("./tests/OSM_FullList.csv")

SurveyIDList <- unique(df$SurveyID)


myCSVString <- convertDataFrameToCSVString(df, SurveyIDList[1])


(v <- structure(10*(5:8), names = LETTERS[1:4]))
f2 <- function(x, y) outer(rep(x, length.out = 3), y)
(a2 <- sapply(v, f2, y = 2*(1:5), simplify = "array"))
