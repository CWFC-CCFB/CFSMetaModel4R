#'
#' Constructor for the MetaModel class.
#'
#' @description A MetaModel instance contains a pointer to a Java instance. It
#' also implements many methods which are described below.
#'
#' @return an S3 MetaModel instance
#'
#'  TODO update documentation here
#'
#' @details
#'
#' The class contains the following methods: \cr
#' \itemize{
#'
#' \item \bold{getVersion()} \cr
#' Provide the version numbers CAPSIS and Capsis4R \cr
#' Return a List
#'
#' \item \bold{getModelDataFields()} \cr
#' Enumerate the model data fields expected by the server.  Use this to create the index array send through the setFieldMatches method. \cr
#' Return A vector of strings containing all the fields as triplets separated by ";" ex : "PLOT (mandatory); String; Match: 0       1"
#'
#' \item \bold{setInitialParameters(initialDateYear, stochasticMode, numberOfRealizations, applicationScale, climateChangeOption)} \cr
#' Set initial model parameters.  Must be called prior to calling runSimulation. \cr
#' Parameters are \cr
#' \itemize{
#' \item initialDateYear - The initial year to be used for simulation (numeric)
#' \item stochasticMode - Enable stochastic mode simulation (1 = enabled, 0 = disable)
#' \item numberOfRealizations - The number of realizations to be used for simulation (numeric)
#' \item applicationScale - The application scale, possible values are ["FMU", "Stand"]
#' \item climateChangeOption - A climate change option, possible values are ["NoChange", "Plus2Degrees", "Plus4Degrees", "Plus6Degrees"]
#' }
#' Return nothing
#'
#' \item \bold{setEvolutionParameters(finalDateYear)} \cr
#' Set evolution parameters.  Must be called prior to calling runSimulation \cr
#' Parameters are \cr
#' \itemize{
#' \item finalDateYear - The final year to be used for simulation
#' }
#' Return nothing
#'
#' \item \bold{getSpeciesOfType(types)} \cr
#' Get the list of species of the specified type. \cr
#' Parameters are \cr
#' \itemize{
#' \item types - The types to get the list of species for (vector)
#' }
#' Return a vector containing all the names of the species for all input types
#'
#' \item \bold{registerOutputRequest(request, aggregationPatterns)} \cr
#' Register a request for a particular output of the simulation. \cr
#' Parameters are \cr
#' \itemize{
#' \item request - The request to register
#' \item aggregationPatterns - The aggregation patterns to register
#' }
#' Return nothing
#'
#' \item \bold{setFieldMatches(matches)} \cr
#' Set field matches for input data.  Must be called prior to calling sendData. \cr
#' Parameters are \cr
#' \itemize{
#' \item matches - A vector of integere specifying the field match order to be used by the server when interpreting CBSendData calls.
#' }
#' Return TRUE if operation succeed
#'
#' \item \bold{sendData(data)} \cr
#' Send data to the CAPSIS Server.  Must be called prior to calling runSimulation. \cr
#' Parameters are \cr
#' \itemize{
#' \item data - A data.frame object whose rows are to be read by CAPSIS
#' }
#' Return nothing
#'
#' \item \bold{runSimulation()} \cr
#' Run the simulation on the CAPSIS model \cr
#' Return the simulation results
#'
#' \item \bold{closeProject()} \cr
#' Close the active project and free its resources \cr
#' Return nothing
#' }
#'
#' @export
new_MetaModel <- function(stratumGroup, geoDomain, dataSource) {
  me <- new.env(parent = emptyenv())
  class(me) <- c("MetaModel")
  me$.metaModel <- J4R::createJavaObject("repicea.simulation.metamodel.MetaModel", stratumGroup, geoDomain, dataSource)
  delayedAssign("getStratumGroup",
                function() {
                  return(me$.metaModel$getStratumGroup())
                },
                assign.env = me)

  delayedAssign("hasConverged",
                function() {
                  return (me$.metaModel$hasConverged())
                },
                assign.env = me)

  delayedAssign("addScriptResult",
                function(initialAge, scriptResult) {
                  me$.metaModel$addScriptResult(as.integer(initialAge), scriptResult)
                  return(invisible(NULL))
                },
                assign.env = me)

  delayedAssign("getPossibleOutputTypes",
                function() {
                  return (J4R::getAllValuesFromListObject(me$.metaModel$getPossibleOutputTypes()))
                },
                assign.env = me)

  delayedAssign("fitModel",
                function(outputType, enableMixedModelImplementations) {
                  me$.metaModel$fitModel(outputType, as.logical(enableMixedModelImplementations))
                  return(invisible(NULL))
                },
                assign.env = me)

  delayedAssign("getPrediction",
                function(ageYr, timeSinceInitialDateYr) {
                  return (me$.metaModel$getPrediction(as.integer(ageYr), as.integer(timeSinceInitialDateYr)))
                },
                assign.env = me)

  delayedAssign("getPredictions",
                function(ageYr, timeSinceInitialDateYr, varianceOutputType) {
                  ageYrArray <- J4R::as.JavaArray(as.integer(ageYr))
                  varianceOutputEnum <- J4R::createJavaObject("repicea.simulation.metamodel.MetaModel$PredictionVarianceOutputType", varianceOutputType)
                  return (me$.metaModel$getPredictions(ageYrArray, as.integer(timeSinceInitialDateYr), varianceOutputEnum))
                },
                assign.env = me)

  delayedAssign("getMonteCarloPredictions",
                function(ageYr, timeSinceInitialDateYr, nbSubjects, nbRealizations) {
                  return (me$.metaModel$getMonteCarloPredictions(as.integer(ageYr), as.integer(timeSinceInitialDateYr), as.integer(nbSubjects), as.integer(nbRealizations)))
                },
                assign.env = me)

  delayedAssign("getPredictionVariance",
                function(ageYr, timeSinceInitialDateYr, includeRandomEffectVariance) {
                  return(me$.metaModel$getPredictionVariance(as.integer(ageYr), as.integer(timeSinceInitialDateYr), as.logical(includeRandomEffectVariance)))
                },
                assign.env = me)

  delayedAssign("exportFinalDataSet",
                function(filename) {
                  return(me$.metaModel$exportFinalDataSet(filename))
                },
                assign.env = me)

  delayedAssign("getSelectedOutputType",
                function() {
                  return(me$.metaModel$getSelectedOutputType())
                },
                assign.env = me)

  delayedAssign("getFinalDataSet",
                function() {
                  return(convertDataSet(me$.metaModel$getFinalDataSet()))
                },
                assign.env = me)

  delayedAssign("save",
                function(fileName) {
                  me$.metaModel$save(fileName)
                },
                assign.env = me)

  delayedAssign("load",
                function(fileName) {
                  me$.metaModel <- J4R::callJavaMethod("repicea.simulation.metamodel.MetaModel", "Load", fileName)
                },
                assign.env = me)

  delayedAssign("getSummary",
                function() {
                  return(me$.metaModel$getSummary())
                },
                assign.env = me)

  delayedAssign("getModelComparison",
                function() {
                  return(convertDataSet(me$.metaModel$getModelComparison()))
                },
                assign.env = me)

  delayedAssign("plot",
                function(textsize = 20, plotPred = T, title = NULL, ymax = 250) {
                  dataset <- me$getFinalDataSet()
                  predictions <- NULL
                  if (me$hasConverged())
                  {
                    #predictions <- javaMetamodel$GetPredictions()
                  }

                  isVarianceAvailable <- "TotalVariance" %in% colnames(dataset)

                  if (isVarianceAvailable)
                  {
                    dataset$lower95 <- dataset$Estimate - dataset$TotalVariance^.5 * qnorm(0.975)
                    dataset[which(dataset$lower95 < 0), "lower95"] <- 0
                    dataset$upper95 <- dataset$Estimate + dataset$TotalVariance^.5 * qnorm(0.975)
                  }

                  dataset$age <- dataset$initialAgeYr + dataset$timeSinceInitialDateYr
                  dataset$stratum <- paste(dataset$OutputType,dataset$initialAgeYr,sep="_")
                  dataset$predL95 <- dataset$pred - dataset$predVar^.5 * qnorm(0.975)
                  dataset$predU95 <- dataset$pred + dataset$predVar^.5 * qnorm(0.975)
                  dataset[which(dataset$predL95 < 0), "predL95"] <- 0

                  datasetPred <- NULL
                  uniqueAge <- c()
                  for (i in 1:length(dataset[,1])) {
                    if (!dataset[i,"age"] %in% uniqueAge) {
                      datasetPred <- rbind(datasetPred, dataset[i,])
                      uniqueAge <- c(uniqueAge, dataset[i,"age"])
                    }
                  }

                  plot <- ggplot2::ggplot()
                  if (isVarianceAvailable)
                  {
                    plot <- plot +
                      ggplot2::geom_ribbon(ggplot2::aes(ymin=lower95, ymax=upper95, x=age, group=stratum), dataset, alpha = .1)
                  }

                  plot <- plot +
                    ggplot2::geom_line(ggplot2::aes(y=Estimate, x=age, group=stratum), dataset, lty = "dashed") +
                    ggplot2::xlab("Age (yr)") +
                    ggplot2::ylab(bquote('Volume'~(m^3~ha^{-1}))) +
                    ggplot2::ylim(0,ymax) +
                    ggplot2::xlim(0, 220) +
                    ggplot2::theme_bw() +
                    ggplot2::theme(text = ggplot2::element_text(size=textsize),
                                   axis.text.x = ggplot2::element_text(size=textsize, color = "black"),
                                   axis.text.y = ggplot2::element_text(size=textsize, color = "black"),
                                   axis.line = ggplot2::element_line(color = "black"),
                                   panel.grid.major = ggplot2::element_blank(),
                                   panel.grid.minor = ggplot2::element_blank(),
                                   panel.background = ggplot2::element_blank(),
                                   axis.ticks.length = ggplot2::unit(3,"mm"),
                                   panel.border = ggplot2::element_blank())
                  if (!is.null(title)) {
                    plot <- plot + ggplot2::ggtitle(title)
                  }
                  if (plotPred) {
                    plot <- plot + ggplot2::geom_ribbon(ggplot2::aes(ymin=predL95, ymax=predU95, x=age), datasetPred, alpha = .5) +
                      ggplot2::geom_line(ggplot2::aes(y=pred, x=age), datasetPred, lty = "solid", size = 1.5)
                  }
                  if (!is.null(predictions)) {
                    predictions$predL95 <- predictions$predictions - predictions$predictionVariance^.5 * qnorm(0.975)
                    predictions$predU95 <- predictions$predictions + predictions$predictionVariance^.5 * qnorm(0.975)
                    plot <- plot + ggplot2::geom_ribbon(ggplot2::aes(ymin=predL95, ymax=predU95, x=ageYr), predictions, alpha = .5) +
                      ggplot2::geom_line(ggplot2::aes(y=predictions, x=ageYr), predictions, lty = "solid", size = 1.5)
                  }
                  return(plot)
                },
                assign.env = me)
  return(me)
}


