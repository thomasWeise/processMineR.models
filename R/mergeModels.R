#' @include Models.R
#' @include joinNames.R

#' @title Create a Combination or Selection of Models
#' @description Merge a set of models according to a given name transformation.
#' @param models the list of instances of \code{\link{RegressionResults}}
#' @param features.ignore a vector of names of features to delete, or
#'   \code{NULL} if no features should explicitly be deleted
#' @param features.keep a vector of the names of the (only) features to keep, or
#'   \code{NULL} if all non-deleted features should be kept
#' @param namer a function used to join names of model sets that now have
#'   identical features, see \code{\link{Models.joinNames}} for
#'   documentation
#' @return a list of \code{\link{Models}} where all names have been transformed
#'   according to \code{namer} and the features have been selected according to
#'   \code{features.ignore} and \code{features.keep} where the \code{models}
#'   members of instances with the same features have been merged
#' @export Models.merge
Models.merge <- function(models, features.ignore=NULL,
                                 features.keep=NULL,
                                 namer=Models.joinNames) {
  # if there are no models, we are done here
  n <- length(models);
  if(length(n) <= 0L) {
    return(list());
  }

  # check the parameters
  if(length(features.ignore) <= 0L) {
    features.ignore <- NULL;
  }
  if(length(features.keep) <= 0L) {
    features.keep <- NULL;
  }

  # compute all the feature vectors
  features.all <- lapply(X=models, FUN=function(model) {
    features <- model@features;
    if(!is.null(features.keep)) {
      features <- features[features.keep];
    }
    if(!is.null(features.ignore)) {
      features <- features[is.na(match(names(features), features.ignore))];
    }
    return(features);
    });

  # find the unique feature vectors
  features.unique <- unique(features.all);
  m               <- length(features.unique);

  # if the list length is the same, we just update the names
  if(m >= n) {
    return(unname(unlist(lapply(X=1L:n, FUN=function(i) {
      return(Models.new(name=namer(models[[i]]@name),
                        features=features.all[[i]],
                        models=models[[i]]@models));
    }), recursive = TRUE)));
  }

  # pick the matching models and create and return a new list of models
  result <- lapply(X=features.unique, FUN=function(features) {
    sel <- unlist(lapply(which(vapply(X=features.all, FUN=identical, FUN.VALUE=FALSE, features)),
                        FUN=function(i) models[[i]]), recursive=TRUE);
    sel.models <- unlist(lapply(X=sel, FUN=function(m) m@models), recursive=TRUE);
    sel.models <- force(sel.models);
    sel.name   <- namer(unlist(lapply(X=sel, FUN=function(m) m@name), recursive=TRUE));
    sel.name   <- force(sel.name);
    Models.new(name=sel.name,
               features=features,
               models=sel.models)
  });

  # create result list
  result <- unname(unlist(result, recursive = TRUE));
  result <- force(result);
  return(result);
}
