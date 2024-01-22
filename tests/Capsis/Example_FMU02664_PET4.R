#################################################
# Meta-model fitting
# Mathieu Fortin & Jean-François Lavoie - February 2022
#################################################

rm(list=ls())
library(CFSMetaModel4R)

capsis <- new_CapsisClass("http://repicea.dynu.net")

variantList <- capsis$VariantList()
variant <- variantList[1]

fieldsList <- capsis$VariantFields(variant)

geoRegion <- "QC"
geoDomain <- "FMU02664"
inventory <- "PET4"

stratumGroupID <- "FMU01152"

# uncomment the following line to force J4R to be connected to an existing J4R server instead of starting a new one
#J4R::connectToJava(port = 18000, internalPort = 50000:50001, public=T, key=212)

metaModel <- new_MetaModel(stratumGroupID, "geoDomain", "dataSource")

MMFileName <- paste(getwd(), "tests", "Capsis", "FittedMetamodel_Coniferous_AllAlive_FMU02664.zml", sep = "/")

if (!file.exists(MMFileName)) {
  speciesConiferous <- capsis$VariantSpecies(variant, FALSE, "Coniferous")
  speciesBroadleaved <- capsis$VariantSpecies(variant, FALSE, "Broadleaved")

  # create output requests
  outputRequestTypes <- capsis$OutputRequestTypes(variant)
  print(outputRequestTypes)
  outputRequestList <- new_OutputRequestList()
  outputRequestList$addOutputRequest("AliveVolume", list(Coniferous = speciesConiferous, Broadleaved = speciesBroadleaved, EPX = I(c("EPX"))))
  fmuList <- read.csv("./tests/Capsis/FMU02664.csv")

  for (i in 1:nrow(fmuList)) {
    row <- fmuList[i,]
    age <- row$initialAgeYr
    csvFilename <- paste("./tests/Capsis/", row$Filename, sep="")
    fmuData <- read.csv(csvFilename)
    message(paste("Simulating age", age))
#    str(fmuData)
    colnames(fmuData) <- c("ManagStr", "PLOT", "LATITUDE", "LONGITUDE", "ALTITUDE", "ECOREGION", "TYPEECO", "SLOPE_CLASS",
                           "DRAINAGE_CLASS", "NO_ARBRE", "SPECIES", "TREESTATUS", "TREEDHPCM", "TREEHEIGHT", "TREEFREQ", "ANNEE_SOND")
    initYear <- fmuData[i,"ANNEE_SOND"]
    simResult <- capsis$Simulate(fmuData, outputRequestList, variant, 80, initYear, TRUE, 100, "RCP4_5", "Stand")
    metaModel$addScriptResult(age, simResult)
  }

  possibleOutputTypes <- metaModel$getPossibleOutputTypes()
  metaModel$fitModel(possibleOutputTypes[3], TRUE)

  metaModel$save(MMFileName)
} else {
  metaModel$load(MMFileName)
}

metaModel$getSummary()

print(metaModel$plot(ymax = 350))
