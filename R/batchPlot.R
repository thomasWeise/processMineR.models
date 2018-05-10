#' @title Plot all the Curves from a List of Models
#' @description A simple utility method for visualizing the data.
#' @param models an instance or list of \code{\link{Models}}
#' @param names the model names, or \code{NULL} if they should not be printed
#' @importFrom plotteR batchPlot.groups batchPlot.list
#' @importClassesFrom regressoR RegressionResult
#' @inheritDotParams plotteR::batchPlot.list -data -names -xfun -yfun -ffun
#' @include Models.R
#' @export batchPlot.Models
batchPlot.Models <- function(models,
                             names=if(length(models)<10) Models.names(models) else NULL,
                             ...) {
  batchPlot.groups(data=models,
                   extract=function(model) model@models,
                   xfun=function(result) result@metric@x,
                   yfun=function(result) result@metric@y,
                   ffun=function(result, x) result@result@f(x),
                   names=names,
                   ...)
}
