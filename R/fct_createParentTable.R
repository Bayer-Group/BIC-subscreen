#' Function to create the subgroup parent table
#'
#' @param results results data set object of class "SubScreenResult".
#' @param y target variable name.
#' @param x variable name.
#' @param x2 second variable name.
#' @param bg.color background color.
#' @param parents subgroup ids parents.
#' @param navpanel navpanel id ("SubscreenExplorer"/"SubscreenComparer").
#'
#' @keywords internal



createParentTable <- function(
  results,
  parents,
  y,
  x,
  x2,
  bg.color,
  navpanel
) {

if (is.null(dim(parents$Parents)) || dim(parents$Parents)[1] == 0) {
    empty_data <- results$sge[0, c("SGID", colnames(results$results_total))]
    if (!is.null(empty_data)) {
      if (dim(empty_data)[2] > 5) {
          empty_data <- empty_data[,1:5]
      }
    }
    tmp <- DT::datatable(
      data = empty_data,
      extensions = 'Buttons',
      options = list(
        language = list(emptyTable = 'Select a subgroup by clicking on a row in the "Selected Subgroups"-list!'),
        initComplete = DT::JS(
        "function(settings, json) {",
        paste0("$(this.api().table().header()).css({'background-color': '",
          bg.color,
          "', 'color': '",
          font_color(different_hues(bg.color)),
          "'});"
        ),"}"
      ),
      dom = 'Brtip',
      buttons = c('copy','print','pageLength',I('colvis')),
      lengthMenu = list(c(6, 12, -1), c("6", "12", "All")),
      pageLength = 6
      ),
      class = 'cell-border stripe',
      rownames = FALSE,
      caption = 'Table of Parent Subgroups',
      filter = 'top'
    )
  } else {
    if (navpanel == "SubscreenExplorer") {
      curr_x <- x
    } else if (navpanel == "SubscreenComparer") {
      curr_x <- x2
    }

    df_par <- subset(
      parents$Parents,
      select = c("SGID", x = curr_x, y = y, "nfactors", results$factors)
    )

    col2hide <- which(sapply(df_par, FUN = function(x){all(x == 'Not used')})) - 1
    names(col2hide) <- NULL

    tmp <- DT::datatable(
      data = df_par,
      extensions = 'Buttons',
      options = list(initComplete = DT::JS(
        "function(settings, json) {",
        paste0("$(this.api().table().header()).css({'background-color': '",
         bg.color,
         "', 'color': '",
         font_color(different_hues(bg.color)),
         "'});"
        ),"}"
      ),
      columnDefs = list(list(targets = col2hide, visible = FALSE)),
      dom = 'Brtip',
      buttons = c('copy','print','pageLength',I('colvis')),
      lengthMenu = list(c(6, 12, -1), c("6", "12", "All")),
      pageLength = 6
      ),
      class = 'cell-border stripe',
      rownames = FALSE,
      caption = 'Table of Parent Subgroups',
      filter = 'top'
    )

    tmp <- DT::formatStyle(
      table = tmp,
      columns = 1:(ncol(df_par) + 1),
      target = "cell",
      backgroundColor = different_hues(bg.color),
      border = paste0('.5px solid ', bg.color)
    )

    tmp.sglev <- levels(relevel(factor(unlist(lapply(df_par[, results$factors], as.character))), ref = 'Not used'))

    colXY <- which(colnames(df_par) %in% c('SGID', names(results$results_total), 'nfactors'))

    col.tabFont <- font_color(different_hues(bg.color))
    col.tabBack <- bg.color
    tmp <- DT::formatStyle(
      table = tmp,
      columns = colXY,
      color = col.tabFont
    )

    tmp <- DT::formatStyle(
      table = tmp,
      columns = results$factors,
      color = DT::styleEqual(
        tmp.sglev, c(col.tabBack, rep(col.tabFont, length(tmp.sglev) - 1))
      )
    )
  }
}
