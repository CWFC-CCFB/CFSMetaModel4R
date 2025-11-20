#'
#' Happy path test for MetaModelClass
#'

rm(list=ls())

library(CFSMetaModel4R)

metaModel <- new_MetaModel("","","")
#metaModelFilename <- "./tests/testthat/QC_3EST_RS38_NoChange_AliveVolume_AllSpecies.zml"
metaModelFilename <- "QC_3EST_RS38_NoChange_AliveVolume_AllSpecies.zml"
#metaModel$convertToLightVersion(metaModelFilename)
metaModelLightFilename <- J4R::callJavaMethod("repicea.simulation.metamodel.MetaModel", "getLightVersionFilename", metaModelFilename)

metaModel$load(metaModelFilename)
hasConverged <- metaModel$hasConverged()
test_that("MetaModel has converged", {
  expect_equal(hasConverged, TRUE)
})

p <- metaModel$plotOutputType("AliveVolume_AllSpecies")
test_that("Produced the plot of output type", {
  expect_equal(inherits(p, "ggplot"), TRUE)
})

comparison <- metaModel$getModelComparison()
test_that("Comparison yields something", {
  expect_equal(comparison$ModelImplementation[1], "ChapmanRichardsDerivative")
})

implementationList <- metaModel$getImplementationList()
test_that("Testing nb of implementations", {
  expect_equal(length(implementationList), 10)
})

outputTypes <- metaModel$getPossibleOutputTypes()
test_that("Testing nb of output types", {
  expect_equal(length(outputTypes), 49)
})

p <- metaModel$plotParameterEstimates()
test_that("Produced the MCMC llk plot", {
  expect_equal("list" %in% class(p), TRUE)
})

p <- metaModel$plotChain()
test_that("Produced the MCMC llk plot", {
  expect_equal(inherits(p, "ggplot"), TRUE)
})

p <- metaModel$plotFit(title = "AliveVolume_AllSpecies")
test_that("Produced the plot of fit", {
  expect_equal(inherits(p, "ggplot"), TRUE)
})

pred <- metaModel$getPredictions(1:150,"PARAMEST")
test_that("Predictions", {
  expect_equal(nrow(pred), 150)
  expect_equal(pred[100,"Pred"], 135.52098595, tolerance = 1E-6)
})

lag <- metaModel$getRegenerationLagYrIfAny()
test_that("Regeneration lag", {
  expect_equal(lag, 8.868763446, tolerance = 1E-6)
})

#### Light version ####

metaModel$load(metaModelLightFilename)
hasConverged <- metaModel$hasConverged()
test_that("MetaModel has converged", {
  expect_equal(hasConverged, TRUE)
})

p <- metaModel$plotOutputType("AliveVolume_AllSpecies")
test_that("Produced the plot of output type", {
  expect_equal(inherits(p, "ggplot"), TRUE)
})

comparison <- metaModel$getModelComparison()
test_that("Comparison yields something", {
  expect_equal(comparison$ModelImplementation[1], "ChapmanRichardsDerivative")
})

implementationList <- metaModel$getImplementationList()
test_that("Testing nb of implementations", {
  expect_equal(length(implementationList), 10)
})

outputTypes <- metaModel$getPossibleOutputTypes()
test_that("Testing nb of output types", {
  expect_equal(length(outputTypes), 49)
})

#### the next two tests should fail because this is a light version of the meta model
test_that("Expecting error on calls to plot parameter estimates with light version", {
  expect_error(metaModel$plotParameterEstimates())
})

test_that("Expecting error on calls to plot MCMC chain", {
  expect_error(metaModel$plotChain())
})

p <- metaModel$plotFit(title = "AliveVolume_AllSpecies")
test_that("Produced the plot of fit", {
  expect_equal(inherits(p, "ggplot"), TRUE)
})

pred <- metaModel$getPredictions(1:150,"PARAMEST")
test_that("Predictions", {
  expect_equal(nrow(pred), 150)
  expect_equal(pred[100,"Pred"], 135.52098595, tolerance = 1E-6)
})

lag <- metaModel$getRegenerationLagYrIfAny()
test_that("Regeneration lag", {
  expect_equal(lag, 8.868763446, tolerance = 1E-6)
})

J4R::shutdownClient()
