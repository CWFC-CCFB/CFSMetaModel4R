#'
#' Happy path test up to addScriptResult
#'
#' IMPORTANT FVS Web API must be online
#'

rm(list=ls())

library(CFSMetaModel4R)

data("FVSTreeListTwoTreesOnePlot")

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

df <- fvs$Simulate(FVSTreeListTwoTreesOnePlot, outputRequestList, variant, standinfoDict, 10, 2, 1990)

scriptResult <- fvs$PrepareScriptResult(df)

metaModel <- new_MetaModel("stratumGroupID", "geoDomain", "dataSource")

metaModel$addScriptResult(as.integer(30), scriptResult)
dataSet <- scriptResult$getDataSet()

test_that("Number of observations in the dataset", {
  expect_equal(dataSet$getNumberOfObservations(), 14)
  expect_equal(dataSet$getValueAt(as.integer(13),"Estimate"), 205.840832765212, tolerance=1E-8)
})

J4R::shutdownClient()
