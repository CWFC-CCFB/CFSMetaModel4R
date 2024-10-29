#'
#' Happy path test for MetaModelClass
#'

rm(list=ls())

library(CFSMetaModel4R)

metaModel <- new_MetaModel("","","")
#metaModel$load("./tests/testthat/QC_3EST_RS38_NoChange_AliveVolume_AllSpecies.zml")
metaModel$load("QC_3EST_RS38_NoChange_AliveVolume_AllSpecies.zml")

p <- metaModel$plotOutputType("AliveVolume_AllSpecies")
test_that("Produced the plot of output type", {
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

J4R::shutdownClient()
