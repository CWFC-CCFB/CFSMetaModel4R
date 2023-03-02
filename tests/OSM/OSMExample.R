rm(list=ls())

library(Capsis4R)
#library(jsonlite)

data("OSMThreeStandList")

CBInitialize("localhost", 18000, 50001:50002, 212)

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

metaModelManager <- new_MetaModelManager()

metaModelManager$createMetaModel("stratumGroupID", "geoDomain", "dataSource")

metaModelManager$addSimulationResult("stratumGroupID", initialAgeYr, scriptResult)
