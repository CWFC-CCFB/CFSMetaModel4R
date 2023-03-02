rm(list=ls())

library(Capsis4R)
#library(jsonlite)

#data("OSMThreeStandList")
data <- read.csv("./tests/FVS_TreeList.csv")

CBInitialize("localhost", 18000, 50001:50002, 212)

osm <- new_OSMClass("https://localhost:7135")

variantList <- osm$VariantList()
variant <- variantList[[1]]

speciesConiferous <- osm$VariantSpecies(variant, TRUE, "Coniferous")
speciesBroadleaved <- osm$VariantSpecies(variant, TRUE, "Broadleaved")

#variantFields <- osm$VariantFields(variant)

outputRequestTypes <- osm$OutputRequestTypes()

outputRequestList <- new_OSMOutputRequestList()

outputRequestList$addOutputRequest(outputRequestTypes$statusClass[[1]], outputRequestTypes$variable[[1]], list(Coniferous = speciesConiferous, Broadleaved = speciesBroadleaved))

df <- osm$Simulate(data, outputRequestList, variant, 10, 2)

scriptResult <- osm$PrepareScriptResult(df)

