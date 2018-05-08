#' @title Plot all the Curves from a List of Models
#' @description A simple utility method for visualizing the data.
#' @param models an instance or list of \code{\link{Model}}
#' @param log the logarithmic scaling info for the axes
#' @param plotPoints should the points be plotted?
#' @param plotFun should the fitted function models be plotted?
#' @param printNames should the model names be printed as legend
#' @importFrom plotteR batchPlot.groups
#' @importClassesFrom regressoR RegressionResult
#' @include Models.R
#' @export batchPlot.Models
batchPlot.Models <- function(models, log="", plotPoints=FALSE, plotFun=TRUE, printNames=(length(models)<10)) {
  batchPlot.groups(data=models,
                   extract=function(model) model@models,
                   xfun=function(result) result@metric@x,
                   yfun=function(result) result@metric@y,
                   ffun=function(result, x) result@result@f(x),
                   plotXY=plotPoints,
                   plotXF=plotFun,
                   log=log,
                   names=if(printNames) Models.names(models) else NULL)
}
