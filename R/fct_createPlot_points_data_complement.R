#' Function to create a data set with complement information based on selected subgroup
#'
#' @param results_tmp subscreen data set
#' @param y target variable
#' @param sel_ids selected subgroup id
#'
#' @keywords internal

createPlot_points_data_complement <- function(
  results_tmp,
  y,
  sel_ids
) {

  if (!is.null(results_tmp)) {
    if (
      y != "N.of.subjects" &
      any(startsWith(colnames(results_tmp$sge), "Complement_"))
    ) {
      IDs <- sel_ids
      if (!is.null(IDs)) {
        if (!is.integer0(IDs)) {
          if (!is.na(IDs)) {
            tmp <- results_tmp$sge[,c("SGID","N.of.subjects", colnames(results_tmp$sge[startsWith(colnames(results_tmp$sge), "Complement_")]))]
            tmp$color <- "#fffb00"
            tmp <- tmp[tmp$SGID %in% IDs, ]
            tmp$ID <- paste0("Complement of SGID ", IDs)
            tmp$N.of.subjects.complement <- results_tmp$results_total$N.of.subjects - tmp$N.of.subjects
            tmp
          } else {NULL}
        } else {NULL}
      } else {NULL}
    } else {NULL}
  }
}
