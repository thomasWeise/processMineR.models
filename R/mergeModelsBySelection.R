#' @include Models.R
#' @include joinNames.R
#' @include makeNamesUnique.R

# the internal merge-by-selection method, which already requires having the right features
.Models.merge.by.selection <- function(models, selection, namer, features.unique) {
  # if there are no models, we are done here
  n <- length(models);
  if((length(n) <= 0L) || (length(selection) <= 0L)) {
    return(list());
  }

  # pick the matching models and create and return a new list of models
  result <- lapply(X=seq_along(selection), FUN=function(sel.index) {
    # get the Models instances belonging to the current selection
    sel <- unlist(lapply(X=selection[[sel.index]], FUN=function(i) models[[i]]), recursive=TRUE);

    # merge all the models stored in these instances
    sel.models <- unlist(lapply(X=sel, FUN=function(m) m@models), recursive=TRUE);
    sel.models <- force(sel.models);

    # obtain the features for these models
    sel.features <- features.unique[[sel.index]];
    sel.features <- force(sel.features);

    # compute the new names
    sel.name   <- namer(unlist(lapply(X=sel, FUN=function(m) m@name), recursive=TRUE),
                        sel.features);
    sel.name   <- force(sel.name);

    # create the new, merged Models instance
    Models.new(name=sel.name,
               features=sel.features,
               models=sel.models)
  });

  # create result list
  result <- unname(unlist(result, recursive = TRUE));
  # make sure that all models have unique names
  result <- Models.make.names.unique(result);
  result <- force(result);
  return(result);
}


#' @title Create a Combination or Selection of Models
#' @description Merge a set of models according to a given selection.
#' @param models the list of instances of \code{\link{Models}}
#' @param selection a list of vectors of model indexes. Models in the same index are joined
#' @param namer a function used to join names of model sets that now have
#'   identical features, see \code{\link{Models.joinNames}} for
#'   documentation
#' @return a list of  instances of \code{\link{Models}} where
#'   all names have been transformed according to \code{namer} and the features
#'   have been selected according to selection
#' @export Models.merge.by.selection
Models.merge.by.selection <- function(models, selection=seq_along(models),
                                      namer=Models.joinNames) {
  # if there are no models, we are done here
  n <- length(models);
  if((length(n) <= 0L) || (length(selection) <= 0L)) {
    return(list());
  }

  # invoke the builder based on the selected models, but first compute the
  # selected features
  return(.Models.merge.by.selection(models=models,
                                    selection=selection,
                                    namer=namer,
                                    features.unique=lapply(X=selection,
   FUN=function(sel) {
     # try to get the feature values for the i'th selection
     # these will be the values that all models have in common
     if(length(sel) == 1L) {
     # only 1 model? return its features as-is
       return(models[[sel[[1L]]]]@features);
     }

     # get the list of feature lists
     features <- lapply(X=sel, FUN=function(i) models[[i]]@features);

     features.length <- length(features);
     # only one (or zero) lists? odd. return as-is
     if(features.length <= 1L) { return(features); }

     # get the blueprint
     first <- features[[1L]];
     first.length <- length(first);
     if(first.length <= 0L) {
       # no feature? then let's quit and return an empty list
       return(list());
     }

     # iterate over all the features and keep the unique feature only
     names <- ls(first);
     keep  <- rep(TRUE, first.length);
     for(i in seq.int(from=2L, to=features.length, by=1L)) {
       keep <- keep & vapply(X=names, FUN=function(j) identical(first[j], features[[i]][j]),
                             FUN.VALUE=FALSE);
     }
     return(first[keep]);
   })));
}
