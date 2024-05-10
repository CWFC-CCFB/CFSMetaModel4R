#'
#' Example of meta-model for OSM and an FMU in Bas-Saint-Laurent
#'

rm(list=ls())

library(CFSMetaModel4R)
#library(jsonlite)

# uncomment the following line to force J4R to be connected to an existing J4R server instead of starting a new one
#J4R::connectToJava(port = 18000, internalPort = 50000:50001, public=T, key=212)

stratumGroupID <- "FMU01152"
metaModel <- new_MetaModel(stratumGroupID, "geoDomain", "dataSource")
#metaModel <- J4R::createJavaObject("repicea.simulation.metamodel.MetaModel", stratumGroupID, "geoDomain", "dataSource")

MMFileName <- paste(getwd(), "tests", "OSM", "FittedMetamodel_Coniferous_AllAlive_FMU01152.zml", sep = "/")

if (!file.exists(MMFileName))
{
  osm <- new_OSMClass()

  variantList <- osm$VariantList()
  variant <- variantList[[1]]

  speciesConiferous <- osm$VariantSpecies(variant, TRUE, "Coniferous")
  speciesBroadleaved <- osm$VariantSpecies(variant, TRUE, "Broadleaved")

  variantFields <- osm$VariantFields(variant)

  outputRequestTypes <- osm$OutputRequestTypes()

  outputRequestList <- new_OutputRequestList()

  outputRequestList$addOutputRequest(outputRequestTypes$statusClass[[1]], outputRequestTypes$variable[[1]], list(Coniferous = speciesConiferous, Broadleaved = speciesBroadleaved))

  fmuList <- read.csv("./tests/OSM/FMU01152.csv")

  for (i in 1:nrow(fmuList))
  {
    row <- fmuList[i,]
    fmuData <- read.csv(paste("./tests/OSM/", row$Filename, sep=""))
    df <- osm$Simulate(fmuData, outputRequestList, variant, 80, 5)

    metaModel$addScriptResult(row$initialAgeYr, df)
  }

  possibleOutputTypes <- metaModel$getPossibleOutputTypes()
  metaModel$fitModel(possibleOutputTypes[2], TRUE)

  metaModel$save(MMFileName)
} else
{
  metaModel$load(MMFileName)
}

metaModel$getSummary()

print(metaModel$plot(ymax = 350))
