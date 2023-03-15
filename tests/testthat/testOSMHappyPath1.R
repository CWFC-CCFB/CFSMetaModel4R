#'
#' Happy path test up to addScriptResult
#'
#'  IMPORTANT OSM Web API must be online
#'

rm(list=ls())

library(CFSMetaModel4R)

data("OSMThreeStandList")

osm <- new_OSMClass("https://localhost:7032")

variantList <- osm$VariantList()
variant <- variantList[[1]]

speciesConiferous <- osm$VariantSpecies(variant, TRUE, "Coniferous")
speciesBroadleaved <- osm$VariantSpecies(variant, TRUE, "Broadleaved")

variantFields <- osm$VariantFields(variant)

outputRequestTypes <- osm$OutputRequestTypes()

outputRequestList <- new_OSMOutputRequestList()

outputRequestList$addOutputRequest(outputRequestTypes$statusClass[[1]], outputRequestTypes$variable[[1]], list(Coniferous = speciesConiferous, Broadleaved = speciesBroadleaved))

df <- osm$Simulate(OSMThreeStandList, outputRequestList, variant, 10, 2)

scriptResult <- osm$PrepareScriptResult(df)

initialAgeYr <- as.integer(40)

metaModel <- new_MetaModel("stratumGroupID", "geoDomain", "dataSource")

metaModel$addScriptResult(initialAgeYr, scriptResult)
dataSet <- scriptResult$getDataSet()

test_that("Number of observations in the dataset", {
  expect_equal(dataSet$getNumberOfObservations(), 12)
  expect_equal(dataSet$getValueAt(as.integer(11),"Estimate"), 248.458891933529, tolerance=1E-8)
})

J4R::shutdownClient()
