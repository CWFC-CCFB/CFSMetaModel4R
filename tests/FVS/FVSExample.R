rm(list=ls())

library(Capsis4R)
#library(jsonlite)

#data("OSMThreeStandList")
data <- read.csv("./tests/FVS/FVS_TreeListTwoTreesOnePlot.csv", colClasses="character")

CBInitialize("localhost", 18000, 50001:50002, 212)

fvs <- new_FVSClass("https://localhost:7135")

variantList <- fvs$VariantList()
variant <- variantList[[1]]

speciesConiferous <- fvs$VariantSpecies(variant, TRUE, "Coniferous")
speciesBroadleaved <- fvs$VariantSpecies(variant, TRUE, "Broadleaved")

#variantFields <- fvs$VariantFields(variant)

outputRequestTypes <- fvs$OutputRequestTypes()

outputRequestList <- new_OSMOutputRequestList()

outputRequestList$addOutputRequest(outputRequestTypes$statusClass[[1]], outputRequestTypes$variable[[1]], list(Coniferous = speciesConiferous, Broadleaved = speciesBroadleaved))

standinfoDict <- new_FVStandInfoDict()
standinfoDict$addStandInfo("0101","KAM-IDFxh2/01", 21, 0, 0, 1150)

df <- fvs$Simulate(data, outputRequestList, variant, standinfoDict, 10, 2, 1990)

scriptResult <- fvs$PrepareScriptResult(df)

