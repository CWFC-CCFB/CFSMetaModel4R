#################################################
# Meta-model fitting
# Mathieu Fortin & Jean-François Lavoie - February 2022
#################################################

rm (list=ls())
library(CFSMetaModel4R)

createFieldIndex <- function(fieldMatches, dataset) {
  output <- c()
  for (o in fieldMatches) {
    if (o == "") {
      output <- c(output, -1)
    } else {
      output <- c(output, which(colnames(dataset) ==  o) - 1)
    }
  }
  return(output)
}

getFieldMatches <- function(modelDataFields, csvFilename, matchingFieldNames = NULL) {
  # Determine input field matches
  dataSample <- read.csv(csvFilename)
  initialYear <- unique(dataSample$ANNEE_SOND)
  data.frame(index = 0:(length(colnames(dataSample)) -1), fieldNames = colnames(dataSample))
  if (is.null(matchingFieldNames)) {
    matchingFieldNames <- c("ID_PE", "LATITUDE", "LONGITUDE", "ALTITUDE", "GUIDE_ECO",
                            "TYPE_ECO", "CL_DRAI", "ESSENCE", "ETAT", "dbhCm",
                            "freq", "", "CL_PENT", "", "",
                            "", "", "heightM", "")
  }
  matches <- createFieldIndex(matchingFieldNames, dataSample)
  return(matches)
}

capsis <- new_CapsisClass("https://localhost:7217")

variantList <- capsis$VariantList()
variant <- variantList[[1]]

fieldsList <- capsis$VariantFields(variant)

geoRegion <- "QC"
geoDomain <- "FMU02664"
inventory <- "PET4"

stratumGroupID <- "FMU01152"

# uncomment the following line to force J4R to be connected to an existing J4R server instead of starting a new one
#J4R::connectToJava(port = 18000, internalPort = 50000:50001, public=T, key=212)

metaModel <- new_MetaModel(stratumGroupID, "geoDomain", "dataSource")

MMFileName <- paste(getwd(), "tests", "Capsis", "FittedMetamodel_Coniferous_AllAlive_FMU02664.zml", sep = "/")

if (!file.exists(MMFileName))
{

  speciesConiferous <- capsis$VariantSpecies(variant, FALSE, "Coniferous")
  speciesBroadleaved <- capsis$VariantSpecies(variant, FALSE, "Broadleaved")

  # create output requests
  outputRequestTypes <- capsis$OutputRequestTypes()

  outputRequestList <- new_OutputRequestList()

  outputRequestList$addOutputRequest(outputRequestTypes$statusClass[[1]], outputRequestTypes$variable[[1]], list(Coniferous = speciesConiferous, Broadleaved = speciesBroadleaved))

  fmuList <- read.csv("./tests/Capsis/FMU02664.csv")

  taskIDList <- character(nrow(fmuList))

  for (i in 1:nrow(fmuList))
  {
    row <- fmuList[i,]
    csvFilename <- paste("./tests/Capsis/", row$Filename, sep="")
    fieldMatches <- getFieldMatches(fieldsList, csvFilename)
    fmuData <- read.csv(csvFilename)

    taskID <- capsis$Simulate(fmuData, outputRequestList, variant, 80, 5, TRUE, 500, "NoChange", "Stand", fieldMatches)
    while (is.null(taskID)) # a null taskID means that the server cannot process a new simulation yet, so let's way until it can
    {
      Sys.sleep(0.1)

      taskID <- capsis$Simulate(fmuData, outputRequestList, variant, 80, 5, TRUE, 500, "NoChange", "Stand", fieldMatches)
    }

    print(paste("Started processing for ", row$Filename))

    taskIDList[i] <- taskID
  }

  # all tasks have been started, now let's wait for them to finish
  for (i in 1:nrow(fmuList))
  {
    row <- fmuList[i,]

    status <- capsis$TaskStatus(taskIDList[i])
    while (status$code == "IN_PROGRESS")
    {
      Sys.sleep(0.1)
      status <- capsis$TaskStatus(taskIDList[i])
    }

    if (status$code != "COMPLETED")
      stop(paste("Error while processing ", row$Filename, " with error code : ", status$code, sep=""))

    print(paste("Adding results to metamodel from ", row$Filename))

    metaModel$addScriptResult(row$initialAgeYr, status$result)
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
