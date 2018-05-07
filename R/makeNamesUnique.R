#' @title Receive a List of \code{\link{Models}} Instances and Make their Names
#'   Unique
#' @description Receive a list of instances of class \code{\link{Models}} and
#'   make sure that all of their names are unique.
#' @param models the list of \code{\link{Models}} instances
#' @return a list of \code{\link{Models}} instances where no name appears twice
#' @include names.R
#' @include Models.R
#' @export Models.make.names.unique
Models.make.names.unique <- function(models) {
  models.length <- length(models);
  if(models.length > 1L) {
    names <- Models.names(models);
    count <- vapply(X=names, FUN=function(name) sum(names==name), FUN.VALUE=0L);
    index <- rep(x=1L, times=models.length);
    if(max(count) > 1L) {
      first <- vapply(X=names, FUN=function(name) which(names==name)[1L], FUN.VALUE=1L);
      for(i in seq_along(models)) {
        if(count[i] > 1L) {
          j <- first[i];
          models[[i]]@name <- paste(models[[i]]@name, ".", index[[j]],
                                    sep="", collapse="");
          models[[i]]@name <- force(models[[i]]@name);
          models[[i]] <- force(models[[i]]);
          index[[j]] <- index[[j]] + 1L;
        }
      }
      return(models);
    }
  }
  return(models);
}
