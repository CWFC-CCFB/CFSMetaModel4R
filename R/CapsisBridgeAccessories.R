#############################################################
# This file is part of the Capsis4R library located at :
# https://github.com/CWFC-CCFB/Capsis4R
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
#' Convert dataset to an R equivalent
#'
#' @param dataSetObject the dataSetObject java object to convert
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

