#'
#' Happy path for StartingValues and StartingValuesMap classes
#'


rm(list=ls())

library(CFSMetaModel4R)

map <- new_StartingValuesMap()
map$add("ChapmanRichards", new_StartingValues(c("b1","b2", "b3", "rho"), c(100, 0.02, 2, 0.95), rep("Uniform",4),
                                                c(list(c("0","600")), list(c("0.0001","0.1")), list(c("1","6")), list(c("0.8","0.995")))))
map$add("ChapmanRichardsWithRandomEffect", new_StartingValues(c("b1","b2", "b3", "rho", "sigma_u"), c(100, 0.02, 2, 0.95, 15), rep("Uniform",5),
                                                c(list(c("0","600")), list(c("0.0001","0.1")), list(c("1","6")), list(c("0.8","0.995")), list(c("0","100")))))
map$add("ChapmanRichardsDerivative", new_StartingValues(c("b1","b2", "b3", "rho"), c(1000, 0.02, 2, 0.95), rep("Uniform",4),
                                                c(list(c("0","3000")), list(c("0.00001","0.05")), list(c("0.8","6")), list(c("0.8","0.995")))))
map$add("ChapmanRichardsDerivativeWithRandomEffect", new_StartingValues(c("b1","b2", "b3", "rho", "sigma_u"), c(1000, 0.02, 2, 0.95, 50), rep("Uniform",5),
                                                c(list(c("0","3000")), list(c("0.00001","0.05")), list(c("0.8","6")), list(c("0.8","0.995")), list(c("0","500")))))

map$keepOnlyThese(c("ChapmanRichardsWithRandomEffect", "ChapmanRichardsDerivative"))

test_that("Nb of candidate models left", {
  expect_equal(length(ls(map, pattern = "Chapman")), 2)
})

map$keepOnlyThese(c())
test_that("Nb of candidate models left", {
  expect_equal(length(ls(map, pattern = "Chapman")), 2)
})

map$keepOnlyThese(NULL)
test_that("Nb of candidate models left", {
  expect_equal(length(ls(map, pattern = "Chapman")), 2)
})

out <- tryCatch(
  {
    map$keepOnlyThese("allo")
    "Failed - should have thrown an exception"
  },
  error = function(cond) {
    "Success - threw an exception"
  },
  warning = function(cond) {
    "Failed - issued a warning instead"
  }
)

test_that("Threw an exception because no models were left in the starting values map", {
  expect_equal(startsWith(out, "Success"), T)
})
