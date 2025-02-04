#'
#' Testing CAPSIS WebAPI
#' @author Mathieu Fortin - February 2025
#'

rm(list=ls())
library(CFSMetaModel4R)

capsis <- new_CapsisClass()

capsis$CapsisStatus()
variantList <- capsis$VariantList()

test_that("Testing nb of variants", {
  expect_equal(length(variantList), 2)
})

variant <- variantList[1]
vegPotScope <- capsis$VariantScope(variant)

test_that("Testing nb of vegpot", {
  expect_equal(length(vegPotScope$VEG_POT), 32)
})

fieldsList <- capsis$VariantFields(variant)

test_that("Testing nb of fields", {
  expect_equal(nrow(fieldsList), 20)
})

geoRegion <- "QC"
geoDomain <- "FMU02664"
inventory <- "PET4"

stratumGroupID <- "FMU01152"

# uncomment the following line to force J4R to be connected to an existing J4R server instead of starting a new one
#J4R::connectToJava(port = 18000, internalPort = 50000:50001, public=T, key=212)

speciesConiferous <- capsis$VariantSpecies(variant, FALSE, "Coniferous")
speciesBroadleaved <- capsis$VariantSpecies(variant, FALSE, "Broadleaved")

# create output requests
outputRequestTypes <- capsis$OutputRequestTypes(variant)
print(outputRequestTypes)

outputRequestList <- new_OutputRequestList()
outputRequestList$addOutputRequest("AliveVolume", list(Coniferous = speciesConiferous, Broadleaved = speciesBroadleaved, EPX = I(c("EPX"))))
outputRequestList$addOutputRequest("AliveBasalArea", list(Coniferous = speciesConiferous, Broadleaved = speciesBroadleaved, EPX = I(c("EPX"))))
outputRequestList$addOutputRequest("AliveStemDensity", list(Coniferous = speciesConiferous, Broadleaved = speciesBroadleaved, EPX = I(c("EPX"))))

#treeList <- read.csv("./tests/testthat/STR_RE2_50.csv")
treeList <- read.csv("STR_RE2_50.csv")
colnames(treeList) <- c("ManagStr", "PLOT", "LATITUDE", "LONGITUDE", "ALTITUDE", "ECOREGION", "TYPEECO", "SLOPE_CLASS",
                        "DRAINAGE_CLASS", "NO_ARBRE", "SPECIES", "TREESTATUS", "TREEDHPCM", "TREEHEIGHT", "TREEFREQ", "ANNEE_SOND")
initYear <- 2006
simResult <- capsis$Simulate(treeList, outputRequestList, variant, 80, initYear, TRUE, 100, "RCP4_5", "Stand")
result <- simResult$dataSet

test_that("Testing nb of realizations in result", {
  expect_equal(simResult$nbRealizations, 100)
  expect_equal(simResult$nbPlots, length(unique(treeList$PLOT)))
  expect_equal(nrow(simResult$dataSet), 81)
})

