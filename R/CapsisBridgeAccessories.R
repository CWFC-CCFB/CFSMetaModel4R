#############################################################
# This file is part of the Capsis4R library located at :
# https://sourceforge.net/projects/capsis4r/
#
# Copyright (C) 2020-2021 Jean-François Lavoie and Mathieu Fortin
# for Canadian Forest Service.
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation; either
# version 3 of the License, or (at your option) any later version.
#
# This library is distributed with the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied
# warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE. See the GNU Lesser General Public
# License for more details.
#
# Please see the license at http://www.gnu.org/copyleft/lesser.html.
#############################################################

#'
#' Plot a MetaModel object
#'
#' @param metaModel the MetaModel object
#' @param textsize the font size (set to 20 by default)
#' @param plotPred a bool (true to plot the predictions of the MetaModel instance)
#' @param title an optional string
#'
#' @export
plotMetaModel <- function(javadataset, textsize = 20, plotPred = T, title = NULL, predictions = NULL) {
  dataset <- convertDataSet(javadataset)
  dataset$lower95 <- dataset$Estimate - dataset$TotalVariance^.5 * qnorm(0.975)
  dataset[which(dataset$lower95 < 0), "lower95"] <- 0
  dataset$upper95 <- dataset$Estimate + dataset$TotalVariance^.5 * qnorm(0.975)
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

  plot <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(ggplot2::aes(ymin=lower95, ymax=upper95, x=age, group=stratum), dataset, alpha = .1) +
    ggplot2::geom_line(ggplot2::aes(y=Estimate, x=age, group=stratum), dataset, lty = "dashed") +
    ggplot2::xlab("Age (yr)") +
    ggplot2::ylab(bquote('Volume'~(m^3~ha^{-1}))) +
    ggplot2::ylim(0,250) +
    ggplot2::xlim(20, 220) +
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
}

#'
#' Convert dataset to an R equivalent
#'
#' @param dataSet the dataSetObject java object to convert
#' @return an R equivalent of the dataset
#'
#' @export
convertDataSet <- function(dataSetObject) {
  fieldNames <- J4R::getAllValuesFromListObject(dataSetObject$getFieldNames())
  for (i in 0:(length(fieldNames) - 1)) {
    values <- J4R::getAllValuesFromListObject(dataSetObject$getFieldValues(i))
    if (i == 0) {
      dataFrame <- data.frame(values)
      colnames(dataFrame) <- fieldNames[i + 1]
    } else {
      dataFrame[, fieldNames[i+1]] <- values
    }
  }

  class(dataFrame)<-c("metamodelResult", "data.frame")

  return(dataFrame)
}

#'
#' Convert predictions returned by MMGetPredictions to an R dataframe
#'
#' @param pred the predictions returned by MMGetPredictions
#' @param key string representing the key of the desired map inside (see MetaModel.PREDICTIONS)
#' @return an R equivalent of the dataset
#'
#' @export
convertPredictions <- function(pred, key) {
  jmap <- pred$get(key)
  iter <- jmap$keySet()$iterator()
  size <- jmap$size()
  ageYr <- 1:size
  data <- 1:size

  i <- 1
  while (iter$hasNext()) {
    rowkey <- J4R::callJavaMethod(iter, "next")
    ageYr[i] <- rowkey
    data[i] <- jmap$get(rowkey)
    i <- i + 1
  }

  output <- data.frame(ageYr, data)
  colnames(output)[2] <- key

  return(output)
}

