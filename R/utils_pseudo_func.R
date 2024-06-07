#' Generate variables for complete/incomplete/pseudo complete factorial context(s)
#'
#' @param results The list entry 'sge' from the "SubScreenResult" object generated via function 'subscreencalc'.
#' @param endpoint The vector of target variable(s).
#' @param factors The list entry 'factors' from the "SubScreenResult" object generated via function 'subscreencalc'.
#'
#'@keywords internal

pseudo_func <- function(results, endpoint, factors) {
  possible_candidates <- names(which(apply(results[,factors],2,function(x){length(unique(x))}) > 2))
  ind <- names(which(unlist(apply(results[is.na(results[,endpoint]),factors],2,function(x){unique(x) != "Not used" & length(unique(x)) == 1}))))
  ind <- names(which.max(apply(results[,ind,drop=FALSE], 2, function(x){length(unique(x))})))
  if(!is.null(possible_candidates)) {
    if(!is.null(ind)) {
      if (!ind %in% possible_candidates) {
        ind <- NULL
      }
    }
  } else { ind <- NULL}
  results[results[,ind] != as.character(results[is.na(results[,endpoint]),ind,drop = FALSE][1,1]),]
}


