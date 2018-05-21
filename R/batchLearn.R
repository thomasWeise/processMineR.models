# create the default models
#' @importFrom regressoR.functional FunctionalModel.makeLearners
#' @importFrom regressoR.functional.models FunctionalModel.linear FunctionalModel.constant
.default <- unlist(c(regressoR.spline.protected(),
                    FunctionalModel.makeLearners(
                      models=c(FunctionalModel.linear(),
                               FunctionalModel.constant()))),
                   recursive=TRUE);
.default <- force(.default);
for(f in .default) { f <- force(f); }


#' @title Batch-Learn Regression Models for Process Mining
#' @description This is a wrapper around
#'   \code{\link[regressoR]{regressoR.batchLearn}} which should provide a
#'   shortcut to the recommended modeling techniques.
#' @param learners the model learners to be applied
#' @param cores the number of cores to use (\code{>1L} leads to parallel
#'   execution)
#' @export Models.batchLearn
#' @importFrom parallel detectCores
#' @importFrom regressoR.splines regressoR.spline.protected
#' @importFrom regressoR regressoR.batchLearn
#' @inheritDotParams regressoR::regressoR.batchLearn -learners -cores
Models.batchLearn <- function(learners=.default,
                              cores=detectCores(),
                              ...) {
  pars <- list(...);
  pars$learners <- learners;
  pars$cores <- cores;
  if(is.null(pars$q)) { pars$q <- 0.05; }
  return(do.call(regressoR.batchLearn, pars));
}
