#' @title Get the Names of All Features of All Models
#' @description Get the names of all features of all models.
#' @param models the list of models from where we want to extract the feature
#'   names
#' @return the feature names (alphabetically sorted)
#' @export Models.featureNames
#' @include Models.R
Models.featureNames <- function(models)
   sort(unique(unname(unlist(lapply(X=models,
                                    FUN=function(model) names(model@features)), recursive = TRUE))))
