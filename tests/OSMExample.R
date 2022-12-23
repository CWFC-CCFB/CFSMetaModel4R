rm(list=ls())

library(Capsis4R)
#library(jsonlite)

data("OSMThreeStandList")

osm <- new_OSMClass("https://localhost:7032")
outputRequestList <- new_OSMOutputRequestList()

outputRequestList$addOutputRequest("Alive", "Volume", list(Coniferous = c("BF", "LX"), Broadleaved = c("BE", "ST")))

df <- osm$Simulate(OSMThreeStandList, outputRequestList, "Acadian", 10, 2);

print(df)

