#' @title Batch-Learn Regression Models for Process Mining
#' @description This is a wrapper around
#'   \code{\link[regressoR]{regressoR.batchLearn}} which should provide a
#'   shortcut to the recommended modeling techniques.
#' @param learners the model learners to be applied
#' @param cores the number of cores to use (\code{>1L} leads to parallel
#'   execution)
#' @export processMineR.batchModel
#' @importFrom parallel detectCores
#' @importFrom regressoR.splines regressoR.spline.protected
#' @importFrom regressoR regressoR.batchLearn
#' @inheritDotParams regressoR::regressoR.batchLearn -learners -cores
processMineR.batchModel <- function(learners=regressoR.spline.protected(),
                                    cores=detectCores(),
                                    ...) {
  return(regressoR.batchLearn(learners=learners, cores=cores, ...));
}
