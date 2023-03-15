#'
#' A test fir FVS with real-world data
#' Mathieu Fortin - March 2023
#'

rm(list=ls())

library(CFSMetaModel4R)
#library(jsonlite)

metaModel <- new_MetaModel("IDFdk3/01", "geoDomain", "dataSource")

#data("OSMThreeStandList")
treeData <- read.csv("./tests/FVS/BC-treeFile.csv")
plotData <- read.csv("./tests/FVS/BC-standInfo.csv")

fvs <- new_FVSClass("https://localhost:7135")

variantList <- fvs$VariantList()
variant <- variantList[[1]]

speciesConiferous <- fvs$VariantSpecies(variant, TRUE, "Coniferous")
speciesBroadleaved <- fvs$VariantSpecies(variant, TRUE, "Broadleaved")

outputRequestTypes <- fvs$OutputRequestTypes()

outputRequestList <- new_OSMOutputRequestList()

outputRequestList$addOutputRequest(outputRequestTypes$statusClass[[1]], outputRequestTypes$variable[[1]], list(Coniferous = speciesConiferous, Broadleaved = speciesBroadleaved))

for (ageCl in unique(plotData$ageClass)) {
  message(paste("Processing stratum", ageCl))
  plotData.AgeCl <- plotData[which(plotData$ageClass == ageCl),]
#  treeData.AgeCl <- merge(plotData.AgeCl, treeData, by = c("site_identifier", "CLSTR_ID"))
  standInfo <- new_FVStandInfoDict()
  for (plotID in unique(plotData.AgeCl$PlotID)) {
    myPlot <- plotData.AgeCl[which(plotData.AgeCl$PlotID == plotID),]
    standInfo$addStandInfo(myPlot$PlotID,
                           myPlot$FVSEcoregionCode,
                           myPlot$ageClass,
                           myPlot$aspect,
                           myPlot$slope,
                           myPlot$elevation)
  }
  data <- treeData[which(treeData$PlotID %in%unique(plotData.AgeCl$PlotID)),]
  df <- fvs$Simulate(data, outputRequestList, variant, standInfo, 80, 5, 2023)
  scriptResult <- fvs$PrepareScriptResult(df)
  metaModel$addScriptResult(ageCl, scriptResult)
}

possibleOutputTypes <- metaModel$getPossibleOutputTypes()
metaModel$fitModel(possibleOutputTypes[2], TRUE)

cat(metaModel$getSummary())
