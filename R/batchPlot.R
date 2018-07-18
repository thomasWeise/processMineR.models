#' @title Plot all the Curves from a List of Models
#' @description A simple utility method for visualizing the data.
#' @param models an instance or list of \code{\link[dataManageR]{dataset}}s with
#'   models in their data field
#' @param names the model names, or \code{NULL} if they should not be printed
#' @importFrom plotteR batchPlot.groups batchPlot.list
#' @importClassesFrom regressoR RegressionResult
#' @inheritDotParams plotteR::batchPlot.list -data -names -xfun -yfun -ffun
#' @export batchPlot.Models
batchPlot.Models <- function(models,
                             names=if(length(models)<10) datasets.names.get(models) else NULL,
                             ...) {
  batchPlot.groups(data=models,
                   extract=function(x) x@data,
                   xfun=function(x) x@metric@x,
                   yfun=function(x) x@metric@y,
                   ffun=function(x) x@result@f,
                   names=names,
                   ...)
}
