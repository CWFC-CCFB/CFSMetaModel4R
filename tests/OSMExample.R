rm(list=ls())

library(Capsis4R)
#library(jsonlite)

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

df <- osm$Simulate(OSMThreeStandList, outputRequestList, variant, 10, 2);

dfAggregated <- df$AggregateResults()
print(dfAggregated)

CBInitialize("localhost", 18000, 50001:50002, 212)

metaModelManager <- new_MetaModelManager()

dataSet <- J4R::callJavaMethod("repicea.simulation.metamodel.ScriptResult", "createEmptyReducedDataSet")
for (i in 1:nrow(dfAggregated))
{
  jarray <- J4R::createJavaObject("java.lang.Object", ncol(dfAggregated), isArray = TRUE)
  J4R::setValueInArray(jarray, as.character(dfAggregated[i,]))
  dataSet$addObservation(jarray)
}

print(cat(dataSet$toString()))

#metaModelManager$addSimulationResult("stratumGroupID", initialAgeYr, simulationResult)
