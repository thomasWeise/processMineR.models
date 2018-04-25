#' @title Join a Set of Names by Keeping only the Identical Parts
#' @description This function receives a vector of names and tries to keep only
#'   their common components. Name components are separated by slashes ("/").
#' @param names the vector of names
#' @return a single string of the common name components only
#' @export Models.joinNames
Models.joinNames <- function(names) {
  l <- length(names);
  if(l <= 0L) { return("unnamed"); }
  if(l <= 1L) { return(names[[1L]]); }

  # if all names are the same, we are good
  uni <- unique(names);
  if(length(uni) == 1L) {
    return(uni[[1L]]);
  }

  # split the names by slashes
  splits <- strsplit(unlist(names, recursive=TRUE), "/");
  min.len <- min(vapply(X=splits, FUN=length, FUN.VALUE = 0L));
  if(min.len > 0L) {
    # ok, each name had at least one slash inside
    nams <- unlist(lapply(X=seq_len(min.len),
                  # for each "column", i.e, slashed component
                   FUN=function(i) {
                     # pick up all values of the column
                     # and check if they are the same
                     sel <- unique(vapply(X=splits,
                                          FUN=function(s, i) s[i],
                                          FUN.VALUE = "",
                                          i));

                     # if they are the same, return them
                     if(length(sel) <= 1L) sel[[1L]]
                     else NULL; # else: delete column
                   }), recursive=TRUE);

    # if there is more than zero columns left over
    if(length(nams) > 0L) {
      # join the column
      return(paste(nams, sep="", collapse="/"));
    }
  }
  return("unnamed");
}
