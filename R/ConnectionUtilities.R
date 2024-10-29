#############################################################
# This file is part of the Capsis4R library located at :
# https://github.com/CWFC-CCFB/Capsis4R
#
# Copyright (C) 2022 Jean-François Lavoie and Mathieu Fortin
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


.welcomeMessage <- function() {
  packageStartupMessage("Welcome to CFSMetaModel4R !")
  packageStartupMessage("For more information, visit https://github.com/CWFC-CCFB/CFSMetaModel4R")
}

.onAttach <- function(libname, pkgname) {
  .welcomeMessage()
}

.onDetach <- function(libname) {
  if (J4R::isConnectedToJava())
    J4R::shutdownClient()
}

#'
#' Check if the package is properly connect to J4R.
#'
#' To be properly connected, the J4R server must be running and must include the repicea-metamodels library in its classpath.
#' If so, the function returns true. If the server is running but does not include the library in its classpath, the function
#' stops. If the J4R server is not running the function returns false.
#'
.checkIfProperlyConnectedToJ4R <- function() {
  if (J4R::isConnectedToJava()) {
    if (J4R::checkIfClasspathContains("repicea-metamodels")) {
      return(TRUE)
    } else {
      stop("The J4R server is running but it does not include the repicea-metamodels library in its classpath. Please shut down the J4R server using the J4R::shutdownClient() function first.")
    }
  }
  return(FALSE)
}

#'
#' Connect to J4R Server
#'
#' Check if the server is running and includes the repicea-metamodels library. If not running,
#' the server is started.
#'
.connectToJ4R <- function() {
  if (!.checkIfProperlyConnectedToJ4R()) {
    rootPath <- system.file("java", package = "CFSMetaModel4R")
    J4R::connectToJava(extensionPath = paste(rootPath, "*", sep="/"))
    J4RLogger <- J4R::getMainLoggerInstance()
    J4R::callJavaMethod("repicea.util.REpiceaLogManager", "setMainLogger", J4RLogger)
  }
}
