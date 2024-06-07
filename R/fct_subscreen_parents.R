#' Returns all 'parent'-subgroups of a specific subgroup
#'
#' @param data The "SubScreenResult" object generated via function 'subscreencalc'.
#' @param SGID Subgroup id(s) of the subgroup for which the 'parent'-subgroups are requested.
#'
#' @keywords internal
#' @return List of 'parent'-subgroups.
#'

parents <- function(data, SGID) {
    if (is.null(SGID) | is.integer0(SGID)) {} else {
      Parents_start <- NULL
      for (k in 1:length(SGID)) {
        start <- data$sge[data$sge$SGID == SGID[k], ]
        if (start$nfactors == 1) {
          Parents_start <- NULL
        } else {
          tmp <- start[, colnames(start) %in% data$factors]
          tmp2 <- tmp[, which(start[, colnames(start) %in% data$factors] != "Not used")]
          ind <- which(colnames(start) %in% colnames(tmp2))
          M1 <- as.data.frame(
            createCombinationMatrix(
              start$nfactors,
              start$nfactors - 1,
              start$nfactors - 1
            )
          )
          colnames(M1) <- colnames(tmp2)
          for (i in 1:length(ind)) {
              M1[M1[, i] == 0, i] <- "Not used"
              M1[M1[, i] == 1, i] <- as.character(start[, ind[i]])
          }
          M1$nfactors <- start$nfactors-1
          Parents_start <- rbind(Parents_start, merge(data$sge, M1))
        }
      }
      return(list('Parents' = Parents_start))
    }
}
