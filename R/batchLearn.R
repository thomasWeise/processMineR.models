# create the default models for lowest fitting effort
#' @importFrom regressoR.direct regressoR.direct.default
.defaultA <- regressoR.direct.default();
.defaultA <- force(.defaultA);

# create the default models for low fitting effort
#' @importFrom regressoR.splines regressoR.spline.protected
.defaultB <- unique(unlist(c(.defaultA,
                             regressoR.spline.protected()),
                           recursive=TRUE));
.defaultB <- force(.defaultB);

# create the default models for high fitting effort
#' @importFrom regressoR.functional FunctionalModel.monotonousLearners
.defaultC <- unique(unlist(c(.defaultB, FunctionalModel.monotonousLearners()),
                    recursive=TRUE));
.defaultC <- force(.defaultC);


#' @title Batch-Learn Regression Models for Process Mining
#' @description This is a wrapper around
#'   \code{\link[regressoR]{regressoR.batchLearn}} which should provide a
#'   shortcut to the recommended modeling techniques.
#' @param q the modeling effort to be applied
#' @param learners the model learners to be applied, which has direct
#'   implications on the runtime, fitting quality, and models tested
#' @param cores the number of cores to use (\code{>1L} leads to parallel
#'   execution)
#' @export Models.batchLearn
#' @importFrom parallel detectCores
#' @importFrom regressoR regressoR.batchLearn
#' @inheritDotParams regressoR::regressoR.batchLearn -learners -cores -q
Models.batchLearn <- function(q=0.2,
                              learners=if(q > 0.15) .defaultC else
                                       if(q > 0.1)  .defaultB else
                                                    .defaultA,
                              cores=detectCores(),
                              ...) {
  pars <- list(...);
  pars$learners <- learners;
  pars$cores <- cores;
  pars$q <- q;
  return(do.call(regressoR.batchLearn, pars));
}
