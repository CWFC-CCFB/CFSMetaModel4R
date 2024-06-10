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
#' \item \bold{fitModel(outputType, startingValuesMap,
#' randomGridSize = 10000,
#' nbBurnIn = 10000,
#' nbAcceptedRealizations = 500000 + nbBurnIn,
#' oneEach = 50)} \cr
#' Fit a metamodel to a particular output type. \cr
#' Arguments are \cr
#' \itemize{
#' \item outputType - The dependent variable of the meta-model
#' \item startingValuesMap - A StartingValuesMap instance
#' \item randomGridSize - The number of random trial in order to find the
#' starting values for the parameters (0 to disable the grid)
#' \item nbBurnIn - The number of burn-in realizations
#' \item nbAcceptedRealizations - The number of realizations in the chain
#' before filtering for the final sample
#' \item onEach - The selection rate for the final sample
#' }
#' Provide the parameter estimates in the console
#'
#' \item \bold{getPredictions(ageYr, varianceOutputType, timeSinceInitialDateYr)} \cr
#' Provide predictions of the meta-model. \cr
#' Arguments are \cr
#' \itemize{
#' \item ageYr - a vector of positive integers
#' \item varianceOutputType - a string either NONE, PARAMEST (error on the mean),
#' PARAMESTRE (error on the mean + random effect)
#' \item timeSinceInitialDateYr - Any value (is useless at the moment, 0 by default)
#' }
#' Return a data.frame object
#'
#' \item \bold{getMonteCarloPredictions(ageYr, nbSubjects, nbRealizations,
#' timeSinceInitialDateYr)} \cr
#' Provide stochastic predictions of the meta-model. \cr
#' Arguments are \cr
#' \itemize{
#' \item ageYr - a vector of positive integers
#' \item nbSubjects - the number of subject (typically strata)
#' \item nbRealizations - the number of realizations
#' \item timeSinceInitialDateYr - Any value (is useless at the moment, 0 by default)
#' }
#' Return a data.frame object
#'
#' \item \bold{getSelectedOutputType()} \cr
#' Provide the output type of this meta-model. \cr
#' Return a character string
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
#' \item \bold{plotOutputType(outputType, textsize = 20)} \cr
#' Plot a graph with the simulation results of a particular output type.
#' Arguments are \cr
#' \itemize{
#' \item outputType - One of the output types (see getPossibleOutputTypes())
#' \item textsize - The font size (by default 20)
#' }
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
#' \item \bold{getImplementationList} \cr
#' Provide the list of possible implementation for the metamodels. \cr
#' Return a vector of characters
#'
#' \item \bold{getRegenerationLagYrIfAny} \cr
#' Provide the regeneration lag if there is one. \cr
#' Return the lag (yrs)
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
                         startingValuesMap,
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
                  linkedHashMapJava <- J4R::createJavaObject("java.util.LinkedHashMap")
                  for (key in ls(startingValuesMap)) {
                    o <- get(key, envir = startingValuesMap, inherits = F)
                    if ("character" %in% class(o)) {
                      linkedHashMapJava$put(key, o)
                      message(paste("Adding implementation:", key))
                    }
                  }
                  message("Fitting candidate meta-models. This may take a while...")
                  me$.metaModel$fitModel(outputType, linkedHashMapJava)
                  return(invisible(NULL))
                },
                assign.env = me)

  delayedAssign("getPredictions",
                function(ageYr, varianceOutputType = "NONE", timeSinceInitialDateYr = 0) {
                  ageYrArray <- J4R::as.JavaArray(as.integer(ageYr))
                  varianceOutputEnum <- J4R::createJavaObject("repicea.simulation.metamodel.MetaModel$PredictionVarianceOutputType", varianceOutputType)
                  dataSetInstance <- me$.metaModel$getPredictions(ageYrArray, as.integer(timeSinceInitialDateYr), varianceOutputEnum)
                  return(convertDataSet(dataSetInstance))
                },
                assign.env = me)

  delayedAssign("getMonteCarloPredictions",
                function(ageYr, nbSubjects, nbRealizations, timeSinceInitialDateYr = 0) {
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

  delayedAssign("plotOutputType",
                function(outputType, textsize = 20) {
                  if (!outputType %in% me$getPossibleOutputTypes()) {
                    stop(paste("The output type should be one of the following:", paste(me$getPossibleOutputTypes(), collapse=", ") ))
                  }

                  dataset <- .formatObservationDataSet(me, outputType)

                  plot <- ggplot2::ggplot()
                  if ("lower95" %in% colnames(dataset) & "upper95" %in% colnames(dataset)) {
                    plot <- plot +
                      ggplot2::geom_ribbon(ggplot2::aes(ymin=lower95, ymax=upper95, x=age, group=stratum), dataset, alpha = .1)
                    maxY <- max(dataset$upper95)
                  } else {
                    maxY <- 0
                  }
                  maxY <- max(maxY, max(dataset$Estimate))

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
                  } else {
                    stop(paste("The output type", outputType, "is not supported!"))
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
                  return(plot)
                },
                assign.env = me)

  delayedAssign("plotFit",
                function(textsize = 20, title = NULL) {
                  dataset <- .formatObservationDataSet(me, me$getSelectedOutputType())
                  maxX <- max(dataset$age)

                  predictions <- me$getPredictions(1:maxX, "PARAMEST")

                  predictions$predL95 <- predictions$Pred - predictions$Variance^.5 * qnorm(0.975)
                  predictions$predU95 <- predictions$Pred + predictions$Variance^.5 * qnorm(0.975)
                  predictions[which(predictions$predL95 < 0), "predL95"] <- 0

                  plot <- ggplot2::ggplot()
                  if ("lower95" %in% colnames(dataset) & "upper95" %in% colnames(dataset)) {
                    plot <- plot +
                      ggplot2::geom_ribbon(ggplot2::aes(ymin=lower95, ymax=upper95, x=age, group=stratum), dataset, alpha = .1)
                    maxY <- max(dataset$upper95)
                  } else {
                    maxY <- 0
                  }
                  maxY <- max(maxY, max(dataset$Estimate))
                  maxY <- max(maxY, max(predictions$predU95))

                  plot <- plot + ggplot2::geom_ribbon(ggplot2::aes(ymin=predL95, ymax=predU95, x=AgeYr), predictions, alpha = .5)

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
                    ggplot2::geom_line(ggplot2::aes(y=Pred, x=AgeYr), predictions, lty = "solid", size = 1.5) +
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

  delayedAssign("getImplementationList",
                function() {
                  list <- J4R::getAllValuesFromArray(J4R::callJavaMethod("repicea.simulation.metamodel.MetaModel$ModelImplEnum", "values"))
                  return(list$name())
                },
                assign.env = me)

  delayedAssign("getRegenerationLagYrIfAny",
                function() {
                  return(me$.metaModel$getRegenerationLagYrIfAny())
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


#'
#' Constructor for the StartingValuesMap class.
#'
#' @description The parameter starting values for a set of model implementations.
#'
#' @return an S3 StartingValuesMap instance
#'
#' @details
#'
#' The class contains the following methods: \cr
#' \itemize{
#'
#' \item \bold{add(implementationName, startingValues)} \cr
#' Add the starting values for a particular implementation. \cr
#' Arguments are \cr
#' \itemize{
#' \item implementationName - The implementation name (see metaModel$getImplementationList)
#' \item startingValues - A StartingValues instance
#' }
#' Return nothing
#' }
#'
#' @export
new_StartingValuesMap <- function() {
  me <- new.env(parent = emptyenv())
  class(me) <- c("StartingValuesMap")
  delayedAssign("add",
                function(implementationName, startingValues) {
                  if (class(implementationName) != "character" | !"StartingValues" %in% class(startingValues)) {
                    stop("The implementationName argument must be a character string and the startingValues must be an instance of the StartingValues S3 class!")
                  }
                  assign(implementationName, startingValues$toJSONString(), envir = me, inherits = FALSE)
                  return(invisible(NULL))
                },
                assign.env = me)
  return(me)
}

.formatObservationDataSet <- function(metaModelInstance, outputType) {
  dataset <- convertDataSet(metaModelInstance$.metaModel$convertScriptResultsIntoDataSet())

  dataset <- dataset[which(dataset$OutputType == outputType),]

  isVarianceAvailable <- "TotalVariance" %in% colnames(dataset)

  if (isVarianceAvailable)
  {
    dataset$lower95 <- dataset$Estimate - dataset$TotalVariance^.5 * qnorm(0.975)
    dataset[which(dataset$lower95 < 0), "lower95"] <- 0
    dataset$upper95 <- dataset$Estimate + dataset$TotalVariance^.5 * qnorm(0.975)
  }

  dataset$age <- dataset$StratumAgeYr + dataset$timeSinceInitialDateYr
  dataset$stratum <- paste(dataset$OutputType, dataset$StratumAgeYr, sep="_")
  return(dataset)
}

