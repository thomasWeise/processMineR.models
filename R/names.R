#' @title Get the Names of All Models in a List
#' @description This function extracts all the names of the models in a list of
#'   models and returns them in the same order as the models appear.
#' @param models the list of models
#' @return the vector of names of the models
#' @export Models.names
#' @include Models.R
Models.names <- function(models) vapply(X=models, FUN=function(model) model@name, FUN.VALUE="unnamed")
