#'
#' Constructor for the MetaModel class.
#'
#' @description A MetaModel instance contains a pointer to a Java instance. It
#' also implements many methods which are described below.
#'
#' @param stratumGroup - a descriptor of the population (e.g. RE3 potential
#' vegetation)
#' @param geoDomain - a descriptor of the location (e.g. 3OUEST)
#' @param dataSource - adescriptor of the data source (e.g. PET4)
#' @return an S3 MetaModel instance
#'
#' @details
#'
#' The class contains the following methods: \cr
#' \itemize{
#'
#' \item \bold{getStratumGroup()} \cr
#' Provide the stratum group the meta-model was fitted to \cr
#' Return a character string
#'
#' \item \bold{hasConverged()} \cr
#' Check if the meta-model has comverged. \cr
#' Return a logical
#'
#' \item \bold{addScriptResult(initialAge, scriptResult)} \cr
#' Add a simulation script result to the meta-model. Those are the simulation result
#' particular model. \cr
#' Arguments are \cr
#' \itemize{
#' \item initialDateYear - The initial year to be used for simulation (numeric)
#' \item scriptResult - A SimulationResult object (see the SimulationResult class)
#' }
#' Return nothing
#'
#' \item \bold{getPossibleOutputTypes()} \cr
#' Provide the output type of the simulation. That is the possible dependent
#' variables of an eventual meta-model. \cr
#' Return a vector of character strings
#'
#' \item \bold{fitModel(outputType, enableMixedModelImplementations,
#' randomGridSize = 10000,
#' nbBurnIn = 10000,
#' nbAcceptedRealizations = 500000 + nbBurnIn,
#' oneEach = 50)} \cr
#' Fit a metamodel to a particular output type. \cr
#' Arguments are \cr
#' \itemize{
#' \item outputType - The dependent variable of the meta-model
#' \item enableMixedModelImplementations - A logical
#' \item randomGridSize - The number of random trial in order to find the
#' starting values for the parameters (0 to disable the grid)
#' \item nbBurnIn - The number of burn-in realizations
#' \item nbAcceptedRealizations - The number of realizations in the chain
#' before filtering for the final sample
#' \item onEach - The selection rate for the final sample
#' }
#' Provide the parameter estimates in the console
#'
#' \item \bold{getPredictions(ageYr, timeSinceInitialDateYr, varianceOutputType)} \cr
#' Provide predictions of the meta-model. \cr
#' Arguments are \cr
#' \itemize{
#' \item ageYr - a vector of positive integers
#' \item timeSinceInitialDateYr - Any value (is useless at the moment)
#' \item varianceOutputType - a string either NONE, PARAMEST (error on the mean),
#' PARAMESTRE (error on the mean + random effect)
#' }
#' Return a data.frame object
#'
#' \item \bold{getMonteCarloPredictions(ageYr, timeSinceInitialDateYr, nbSubjects,
#' nbRealizations)} \cr
#' Provide stochastic predictions of the meta-model. \cr
#' Arguments are \cr
#' \itemize{
#' \item ageYr - a vector of positive integers
#' \item timeSinceInitialDateYr - Any value (is useless at the moment)
#' \item nbSubjects - the number of subject (typically strata)
#' \item nbRealizations - the number of realizations
#' }
#' Return a data.frame object
#'
#' \item \bold{getSelectedOutputType()} \cr
#' Provide the output type of this meta-model. \cr
#' Return a character string
#'
#' \item \bold{getFinalDataSet()} \cr
#' Provide the individual stratum simulation and the meta-model predictions.
#' Return a data.frame object
#'
#' \item \bold{load(filename)} \cr
#' Load a meta-model from file. \cr
#' Arguments are \cr
#' \itemize{
#' \item filename - The name of the file that contains the meta-model instance
#' }
#' Return nothing
#'
#' \item \bold{save(filename)} \cr
#' Save a meta-model to file. \cr
#' Arguments are \cr
#' \itemize{
#' \item filename - The name of the file that will contain the meta-model instance
#' }
#' Return nothing
#'
#' \item \bold{getSummary} \cr
#' Display the parameter estimates and other information on the meta-model in
#' the console. \cr
#' Return nothing
#'
#' \item \bold{getModelComparison} \cr
#' Display the comparison between the different meta-model implementations in
#' the console. \cr
#' Return nothing
#'
#' \item \bold{plotFit} \cr
#' Provide a graph of the goodness of fit of the meta-model. \cr
#' Return a ggplot2 graph
#'
#' \item \bold{plotChain} \cr
#' Provide a graph of the loglikelihood of the different realizations of the
#' final sample. \cr
#' Return a ggplot2 graph
#'
#' \item \bold{plotParameterEstimates} \cr
#' Provide a histogram for each parameter estimate. \cr
#' Return a list of ggplot2 graph
#'
#' \item \bold{convertScriptResultsIntoDataSet} \cr
#' Provide the script results in a dataset. \cr
#' Return a data.frame object
#'
#' \item \bold{setStartingValuesForThisModelImplementation} \cr
#' Set the starting values of the parameters for a particular model implementation. \cr
#' Arguments are \cr
#' \itemize{
#' \item modelImpl - A string that stands for the model implementation (e.g. ChapmanRichardsDerivativeWithRandomEffect)
#' \item startingValues - A StartingValues instance
#' }
#' Return a data.frame object
#' }
#'
#' @export
new_MetaModel <- function(stratumGroup, geoDomain, dataSource) {
  .connectToJ4R()
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
                  scriptResultJava <- .prepareScriptResult(scriptResult)
                  me$.metaModel$addScriptResult(as.integer(initialAge), scriptResultJava)
                  return(invisible(NULL))
                },
                assign.env = me)

  delayedAssign("getPossibleOutputTypes",
                function() {
                  return (J4R::getAllValuesFromListObject(me$.metaModel$getPossibleOutputTypes()))
                },
                assign.env = me)

  delayedAssign("fitModel",
                function(outputType,
                         enableMixedModelImplementations,
                         randomGridSize = 10000,
                         nbBurnIn = 10000,
                         nbAcceptedRealizations = 500000 + nbBurnIn,
                         oneEach = 50) {
                  simParms <- me$.metaModel$getMetropolisHastingsParameters()

                  if (randomGridSize < 0) {
                    warning("Random grid size argument is inconsistent. Previous value will be used instead.")
                  } else {
                    simParms$nbInitialGrid <- as.integer(randomGridSize)
                  }

                  if (nbBurnIn < 1) {
                    warning("Number of burn in realizations is inconsistent. Previous value will be used instead.")
                  } else {
                    simParms$nbBurnIn <- as.integer(nbBurnIn)
                  }

                  if (nbAcceptedRealizations < nbBurnIn) {
                    warning("Number of accepted realizations is inconsistent. Previous value will be used instead.")
                  } else {
                    simParms$nbAcceptedRealizations <- as.integer(nbAcceptedRealizations)
                  }

                  if (oneEach < 1 | oneEach > nbAcceptedRealizations) {
                    warning("Rate of final selection (oneEach argument) is inconsistent. Previous value will be used instead.")
                  } else {
                    simParms$oneEach <- as.integer(oneEach)
                  }
                  message(simParms$toString())
                  message("Fitting candidate meta-models. This may take a while...")
                  me$.metaModel$fitModel(outputType, as.logical(enableMixedModelImplementations))
                  return(invisible(NULL))
                },
                assign.env = me)

  delayedAssign("getPredictions",
                function(ageYr, timeSinceInitialDateYr, varianceOutputType) {
                  ageYrArray <- J4R::as.JavaArray(as.integer(ageYr))
                  varianceOutputEnum <- J4R::createJavaObject("repicea.simulation.metamodel.MetaModel$PredictionVarianceOutputType", varianceOutputType)
                  dataSetInstance <- me$.metaModel$getPredictions(ageYrArray, as.integer(timeSinceInitialDateYr), varianceOutputEnum)
                  return(convertDataSet(dataSetInstance))
                },
                assign.env = me)

  delayedAssign("getMonteCarloPredictions",
                function(ageYr, timeSinceInitialDateYr, nbSubjects, nbRealizations) {
                  ageYrArray <- J4R::as.JavaArray(as.integer(ageYr))
                  dataSetInstance <- me$.metaModel$getMonteCarloPredictions(ageYrArray, as.integer(timeSinceInitialDateYr), as.integer(nbSubjects), as.integer(nbRealizations))
                  return(convertDataSet(dataSetInstance))
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
                  return(cat(me$.metaModel$getSummary()))
                },
                assign.env = me)

  delayedAssign("getModelComparison",
                function() {
                  return(convertDataSet(me$.metaModel$getModelComparison()))
                },
                assign.env = me)

  delayedAssign("plotFit",
                function(textsize = 20, plotPred = T, title = NULL) {
                  dataset <- me$getFinalDataSet()
                  predictions <- NULL

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
                  if (isVarianceAvailable) {
                    plot <- plot +
                      ggplot2::geom_ribbon(ggplot2::aes(ymin=lower95, ymax=upper95, x=age, group=stratum), dataset, alpha = .1)
                    maxY <- max(dataset$upper95)
                  } else {
                    maxY <- 0
                  }
                  maxY <- max(maxY, max(dataset$Estimate))

                  if (plotPred) {
                    plot <- plot + ggplot2::geom_ribbon(ggplot2::aes(ymin=predL95, ymax=predU95, x=age), datasetPred, alpha = .5) +
                      ggplot2::geom_line(ggplot2::aes(y=pred, x=age), datasetPred, lty = "solid", size = 1.5)
                    maxY <- max(maxY, max(dataset$predU95))
                  }

                  outputType <- me$getSelectedOutputType()
                  if (grepl("BasalArea", outputType)) {
                    yLabel <- bquote('Basal area'~(m^2~ha^{-1}))
                  } else if (grepl("Volume", outputType)) {
                    yLabel <- bquote('Volume'~(m^3~ha^{-1}))
                  } else if (grepl("Biomass", outputType)) {
                    yLabel <- bquote('Biomass'~(Mg~ha^{-1}))
                  } else if (grepl("DominantHeight", outputType)) {
                    yLabel <- bquote('Dominant height'~(m))
                  } else if (grepl("StemDensity", outputType)) {
                    yLabel <- bquote('Density'~(Trees~ha^{-1}))
                  }

                  plot <- plot +
                    ggplot2::geom_line(ggplot2::aes(y=Estimate, x=age, group=stratum), dataset, lty = "dashed") +
                    ggplot2::xlab("Age (yr)") +
                    ggplot2::ylab(yLabel) +
                    ggplot2::ylim(0, maxY + 1) +
                    ggplot2::xlim(0, max(dataset$age) + 1) +
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
                  return(plot)
                },
                assign.env = me)

  delayedAssign("plotChain",
                function() {
                  markovChain <- convertDataSet(me$.metaModel$convertMetropolisHastingsSampleToDataSet())
                  markovChain$i <- 1:nrow(markovChain)

                  chainPlot <- ggplot2::ggplot() + ggplot2::geom_point(ggplot2::aes(x=i,y=LLK), data=markovChain)
                  return(chainPlot)
                },
                assign.env = me)

  delayedAssign("plotParameterEstimates",
                function() {
                  output <- list()
                  markovChain <- convertDataSet(me$.metaModel$convertMetropolisHastingsSampleToDataSet())
                  parmNames <- colnames(markovChain)[2:length(colnames(markovChain))]
                  markovChain$i <- 1:nrow(markovChain)
                  for (parm in parmNames) {
                    data.tmp <- markovChain[,c("i", parm)]
                    colnames(data.tmp)[2] <- "parm"
                    plot <- ggplot2::ggplot() +
                      ggplot2::geom_histogram(ggplot2::aes(x=parm), data=data.tmp) +
                      ggplot2::xlab(parm)
                    output[[parm]] <- plot
                  }
                  return(output)
                },
                assign.env = me)

  delayedAssign("convertScriptResultsIntoDataSet",
                function(initialAge, scriptResult) {
                  datasetObject <- me$.metaModel$convertScriptResultsIntoDataSet()
                  return(convertDataSet(datasetObject))
                },
                assign.env = me)

  delayedAssign("getParameterFieldNames",
                function() {
                  inputParameterMapKeys <- J4R::getAllValuesFromArray(J4R::callJavaMethod("repicea.simulation.metamodel.ParametersMapUtilities$InputParametersMapKey", "values"))
                  return(inputParameterMapKeys$name())
                },
                assign.env = me)

  delayedAssign("setStartingValuesForThisModelImplementation",
                function(modelImpl, startingValues) {
                  if (!("StartingValues" %in% class(startingValues))) {
                    stop("The startingValues parameter should be an instance of the StartingValues class.")
                  }
                  modelImplEnum <- J4R::createJavaObject("repicea.simulation.metamodel.MetaModel$ModelImplEnum", modelImpl)
                  jsonStr <- startingValues$toJSONString()
                  message(paste("Setting parameters of", modelImplEnum$name()))
                  message(jsonStr)
                  me$.metaModel$setStartingValuesForThisModelImplementation(modelImplEnum, jsonStr)
                  return(invisible(NULL))
                },
                assign.env = me)

  return(me)
}

.prepareScriptResult <- function(simResults) {
  if ("TotalVariance" %in% colnames(simResults$dataSet))
  {
    dataSet <- J4R::callJavaMethod("repicea.simulation.scriptapi.ScriptResult", "createEmptyDataSet")
  }
  else
  {
    dataSet <- J4R::callJavaMethod("repicea.simulation.scriptapi.ScriptResult", "createEmptyReducedDataSet")
  }

  for (i in 1:nrow(simResults$dataSet))
  {
    jarray <- J4R::createJavaObject("java.lang.Object", ncol(simResults$dataSet), isArray = TRUE)
    J4R::setValueInArray(jarray, as.character(simResults$dataSet[i,]))
    dataSet$addObservation(jarray)
  }
  dataSet$indexFieldType()

  climateChangeScenario <- J4R::callJavaMethod("repicea.simulation.climate.REpiceaClimateGenerator$ClimateChangeScenarioHelper", "getClimateChangeScenarioFromString", simResults$climateChangeScenario)

  scriptResult <- J4R::createJavaObject("repicea.simulation.scriptapi.ScriptResult", simResults$nbRealizations, simResults$nbPlots, climateChangeScenario, simResults$growthModel, dataSet)

  return(scriptResult)
}


#'
#' Constructor for the StartingValues class.
#'
#' @description The parameter starting values for a particular model implementation.
#'
#' @param Parameter - a vector of characters that stands for the parameter names
#' @param StartingValue - a vector of numerics
#' @param Distribution - a vector of characters that stands for the prior distributions
#' @param DistParms - a vector of lists containing the parameters of the prior distributions
#' @return an S3 StartingValues instance
#'
#' @details
#'
#' The class contains the following methods: \cr
#' \itemize{
#'
#' \item \bold{toJSONString()} \cr
#' Provide a JSON representation of this object \cr
#' Return a character string
#' }
#'
#' @export
new_StartingValues <- function(Parameter, StartingValue, Distribution, DistParms) {
  me <- new.env(parent = emptyenv())
  class(me) <- c("MetaModel")
  me$.startingValues <- data.frame(Parameter = Parameter, StartingValue = StartingValue, Distribution = Distribution)
  me$.startingValues$DistParms <- DistParms
  class(me) <- c("StartingValues")
  delayedAssign("toJSONString",
                function() {
                  jsonStr <- toJSON(me$.startingValues)
                  class(jsonStr) <- "character"
                  return(jsonStr)
                },
                assign.env = me)
  return(me)
}
