#' Function to create the subgroup filter table
#'
#' @param filter1 variable name of first filter.
#' @param filter2 variable name of second filter.
#' @param variableChosen1 level of first filter variable.
#' @param variableChosen2 level of second filter variable.
#' @param results results data set object of class "SubScreenResult".
#' @param y target variable name.
#' @param x variable name.
#' @param bg.color background color.
#' @param key number factors.
#'
#' @keywords internal

createFilteredTable <- function(
  filter1,
  filter2,
  variableChosen1,
  variableChosen2,
  results,
  y,
  x,
  bg.color,
  key
) {
  if (filter1 != "no selection" & filter2 == "no selection") {
    choice <- variableChosen1
    select_points_data <- results$sge[which(results$sge$nfactors >= key[1] &
                                      results$sge$nfactors <= key[2] &
                                      results$sge[, c(filter1)] == choice),]
  } else if (filter1 != "no selection" &  filter2 != "no selection") {
    choice <- variableChosen1
    choice2 <- variableChosen2
    select_points_data <- results$sge[which(results$sge$nfactors >= key[1] &
                                      results$sge$nfactors <= key[2] &
                                      results$sge[, c(filter1)] == choice &
                                      results$sge[, c(filter2)] == choice2),]
  } else {
    select_points_data <- data.frame(x = numeric(), y = numeric(), SGID = numeric())
  }

  if (filter1 == "no selection" ||  !is.null(variableChosen1)) {
    if (!is.null(results$sge)) {
      empty_data <- results$sge[0,c("SGID", colnames(results$results_total))]
      if (dim(empty_data)[2] > 5) {
        empty_data <- empty_data[,1:5]
      }


      tmp <- DT::datatable(
        data = empty_data ,
        extensions = 'Buttons',
        options = list(
          language = list(emptyTable = 'To get specific subgroups listed here, use the "Subgroup Filter"-option in the "Variable Options"-tab!'),
          initComplete = DT::JS(
            "function(settings, json) {",
            paste0("$(this.api().table().header()).css({'background-color': '",
              bg.color,
              "', 'color': '",
              font_color(different_hues(bg.color)),
              "'});"
            ),
            "}"
          ),
          dom = 'Brtip',buttons = c('copy', 'print', 'pageLength', I('colvis')),
          lengthMenu = list(c(6, 12, -1), c("6", "12", "All")),
          pageLength = 6
        ),
        class = 'cell-border stripe',
        rownames = FALSE,
        caption = 'Table of Filtered Subgroups',
        filter = 'top'
      )

      tmp <- DT::formatStyle(
        table = tmp,
        columns = 1:(ncol(empty_data) + 1),
        target = "cell",
        backgroundColor = different_hues(bg.color),
        border = paste0('.5px solid ', bg.color)
      )

      tmp
    }
  }

  if (filter1 != "no selection" &  !is.null(variableChosen1)) {
    df_filt <- subset(select_points_data, select = c(x = x, y = y, "nfactors", results$factors))

    col2hide <- which(sapply(df_filt, FUN = function(x){all(x == 'Not used')})) - 1
    names(col2hide) <- NULL
    tmp <- DT::datatable(
      data = df_filt ,
      extensions = 'Buttons',
      options = list(
        initComplete = DT::JS(
          "function(settings, json) {",
          paste0("$(this.api().table().header()).css({'background-color': '",
            bg.color,
            "', 'color': '",
            font_color(different_hues(bg.color)),
            "'});"
          ),
          "}"
        ),
        columnDefs = list(list(targets = col2hide, visible = FALSE)),
        dom = 'Brtip',buttons = c('copy', 'print', 'pageLength', I('colvis')),
        lengthMenu = list(c(6, 12, -1), c("6", "12", "All")),
        pageLength = 6
      ),
      class = 'cell-border stripe',
      rownames = FALSE,
      caption = 'Table of Filtered Subgroups',
      filter = 'top'
    )

    tmp <- DT::formatStyle(
      table = tmp,
      columns = 1:ncol(df_filt),
      target = "cell",
      backgroundColor = different_hues(bg.color),
      border = paste0('.5px solid ',bg.color)
    )

    if (dim(df_filt)[1] != 0) {
      tmp.sglev <- levels(
        relevel(
          factor(
            unlist(
              unique(
                lapply(df_filt, as.character)
              )
            )
          ), ref = "Not used"
        )
      )
      colXY <- which(
        colnames(
          subset(
            df_filt,
            select = c(x = x, y = y, 'nfactors', results$factors)
          )
        ) %in% c('SGID', names(results$results_total), 'nfactors')
      )

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
          tmp.sglev, c('black', rep(col.tabFont, length(tmp.sglev) - 1))
        )
      )
    }
    tmp
  }
}
