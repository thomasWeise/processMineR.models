#' @title A List of Models (\code{\link{RegressionResult}}s) for a Specific Setup
#'   Identified by a Name and a List of Features
#' @description Each instance of this  class represents list of regression
#'   models for a specific setup identified by a list of features and a name.
#' @slot name the name of this setup
#' @slot features a list of name-value pairs denoting the features of the setup
#' @slot models the list of instances of \code{\link{RegressionResult}} for
#'   this setup
#' @importFrom methods setClass representation prototype
#' @importClassesFrom regressoR RegressionResult
#' @exportClass Models
Models <- setClass(
  Class = "Models",
  representation = representation(name="character",
                                  features="list",
                                  models="list"),
  validity = function(object) {
    # check name: must be exactly one
    if(is.null(object@name) || (length(object@name) != 1L) ||
                               (nchar(object@name) <= 0L)) {
      return("Name must be defined and non-empty.");
    }

    # check the features
    if(is.null(object@features)) {
      return("Features list can be empty, but not NULL.");
    }
    l <- length(object@features);
    if(l > 0L) {
      nams <- names(object@features);
      if(length(nams) != l) {
        return("All features must be named.");
      }
      for(nam in nams) {
        if(is.null(nam) || (nchar(nam) <= 0L)) {
          return("All feature names must be valid, non-empty strings.");
        }
      }
      for(feature in object@features) {
        if(is.na(feature) || is.null(feature) || (length(feature) != 1L) ||
           (!((is.character(feature) && (nchar(feature) > 0L)) ||
              is.logical(feature) ||
             (is.numeric(feature) && is.finite(feature)) ||
             is.factor(feature)))) {
          return("All features must be non-empty character strings, logical, or finite numbers or factors.");
        }
      }
    }

    # check models
    if(is.null(object@models) || (length(object@models) <= 0L)) {
      return("List of models must be defined and not empty");
    }
    for(result in object@models) {
      if(is.null(result) || (!(is(result, "RegressionResult")))) {
        return("Each result record must be an non-NULL instance of RegressionResult.");
      }
      validObject(result);
    }

    return(TRUE);
  }
)

#' @title Create an Instance of \code{\link{Models}}
#' @description Instantiate \code{\link{Models}} using this method.
#' @param name the name of this setup
#' @param features a list of name-value pairs denoting the features of the setup
#' @param models the list of instances of \code{\link{RegressionResult}} for
#'   this setup
#' @importFrom methods validObject new
#' @export Models.new
Models.new <- function(name, features, models) {
  if(length(features) > 1L) {
    features <- features[order(names(features))];
  }
  result <- new("Models", name=name,
                features=features,
                models=unname(unlist(models, recursive=TRUE)));
  result <- force(result);
  result@name <- force(result@name);
  result@models <- force(result@models);
  result@features <- force(result@features);
  result <- force(result);
  validObject(result);
  return(result);
}
