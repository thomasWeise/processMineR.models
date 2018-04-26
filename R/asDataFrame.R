#' @title Convert the Model Features, Feature Names, and Model Names into a Data Frame
#' @description Create a data frame which represents all the meta-information about the models.
#' @param models the models
#' @param stringsAsFactors convert strings to factors
#' @return the data frame
#' @export Models.as.data.frame
Models.as.data.frame <- function(models, stringsAsFactors=TRUE) {
  features <- Models.featureNames(models);
  values <- lapply(X=features, FUN=function(feature) Models.featureValues(models, feature));
  values$stringsAsFactors <- stringsAsFactors;
  frame <- do.call(cbind.data.frame, values);
  names(frame) <- features;
  rownames(frame) <- Models.names(models);
  return(frame);
}
