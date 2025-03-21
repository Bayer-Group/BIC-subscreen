#' Function to create a color code vector for each subgroup
#'
#' @param results results data set object of class "SubScreenResult".
#' @param y target variable name.
#' @param point_color color of circles.
#' @param selected_subgroup selected subgroups.
#' @param selected_subgroup_color selected subgroup color.
#' @param filter1 variable name of first filter.
#' @param filter2 variable name of second filter.
#' @param variableChosen1 level of first filter variable.
#' @param variableChosen2 level of second filter variable.
#' @param filter_color color for filtered circles.
#' @param parent_subgroups_color color for parent circles.
#' @param factorial_context_color color for factorial context.
#' @param importance_values subgroup ids with importance values.
#' @param importance_values_color importance values color.
#' @param memorized_data subgroup ids which are memorized.
#' @param memorized_color memorized subgroup id color.
#' @param key factor levels.
#'
#' @keywords internal

createColorVector <- function(
    results,
    y,
    point_color,
    selected_subgroup,
    selected_subgroup_color,
    filter1,
    filter2,
    variableChosen1,
    variableChosen2,
    filter_color,
    parent_subgroups_color,
    factorial_context_color,
    importance_values,
    importance_values_color,
    memorized_data,
    memorized_color,
    key
){
  f <- results$sge[which(results$sge$nfactors >= key[1] & results$sge$nfactors <= key[2]), ]
  #create point color depending on number levels of subgroup
  f$colour <- as.character(
    c(
      grDevices::adjustcolor(point_color, alpha = 1 ),
      grDevices::adjustcolor(point_color, alpha = 0.75 ),
      grDevices::adjustcolor(point_color, alpha = 0.5 ),
      grDevices::adjustcolor(point_color, alpha = 0.25 ),
      grDevices::adjustcolor(point_color, alpha = 0.1 ),
      grDevices::adjustcolor(point_color, alpha = 0.1 ),
      grDevices::adjustcolor(point_color, alpha = 0.1 ),
      grDevices::adjustcolor(point_color, alpha = 0.1 )
    )
  )[match(f$nfactors, 1:8)]
  
  ##filter selection
  if (filter1 != "no selection" & filter2 == "no selection") {
    if (!is.null(variableChosen1)) {
      select_points_data <- results$sge[
        which(
          results$sge$nfactors >=
            key[1] & results$sge$nfactors <=
            key[2] &
            results$sge[, c(filter1)] == variableChosen1
        ),
      ]
      
      f$colour[f$SGID %in% select_points_data$SGID] <- filter_color
    }
  } else if (filter1 != "no selection" & filter2 != "no selection") {
    select_points_data <- results$sge[
      which(
        results$sge$nfactors >=
          key[1] & results$sge$nfactors <=
          key[2] &
          results$sge[, c(filter1)] == variableChosen1 &
          results$sge[, c(filter2)] == variableChosen2
      ),
    ]
    f$colour[f$SGID %in% select_points_data$SGID] <- filter_color
  }
  
  #variable importance
  if(!is.null(importance_values)) {
    f[f$SGID %in% importance_values, 'colour'] <- importance_values_color
  }
  
  #selected points
  if(!is.null(selected_subgroup)) {
    f[f$SGID == selected_subgroup,'colour'] <- selected_subgroup_color
  }
  
  #context
  SGID_clicked <- selected_subgroup
  if (!is.null(SGID_clicked)) {
    if(!is.integer0(SGID_clicked)) {
      if(!is.na(SGID_clicked)) {
        
        tmp <- results$sge[results$sge$FCID_all == results$sge[results$sge$SGID == SGID_clicked,]$FCID_all,]
        
        if (!any(startsWith(colnames(tmp),"FCID_complete_")) & dim(tmp)[1] != 0) {
          tmp <- pseudo_contexts(tmp, y, results$factors)
        }
        #only when pseudo calculation was performed
        #if (any(startsWith(colnames(scresults$sge),"FCID_complete"))) {
        if (paste0("FCID_incomplete_",y) %in% colnames(tmp)) {
          # 1. case: Context complete:
          if (all(tmp[paste0("FCID_incomplete_",y)] == "Complete")) {
            f[f$SGID %in% tmp$SGID, 'colour'] <- factorial_context_color
          }
          #2. case: Context incomplete:
          if (all(tmp[paste0("FCID_incomplete_",y)] == "Incomplete")) {
            f[f$SGID %in% tmp$SGID, 'colour'] <- different_hues(factorial_context_color, value = 100)
          }
          #3. case: Context pseudo complete
          if (all(tmp[paste0("FCID_incomplete_",y)] == "Pseudo complete")) {
            f[f$SGID %in% tmp[tmp[paste0("FCID_pseudo_", y)] != "No Pseudo",]$SGID, 'colour'] <- different_hues(factorial_context_color, value = 50)
            f[f$SGID %in% tmp[tmp[paste0("FCID_pseudo_", y)] == "No Pseudo",]$SGID, 'colour'] <- different_hues(factorial_context_color, value = 150)
          }
        }
      }
    }
  }
  
  f[f$SGID %in% parents(results, selected_subgroup)$Parents$SGID,'colour'] <- parent_subgroups_color
  
  f[f$SGID == selected_subgroup,'colour'] <- selected_subgroup_color
  
  if (!is.null(memorized_data[['SGID']])) {
    if (!is.integer0(memorized_data[['SGID']])) {
      f[f$SGID %in% memorized_data[['SGID']], 'colour'] <- memorized_color
    }
  }
  f$colour
}