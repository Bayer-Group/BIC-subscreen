#' (ii) Visualization
#'
#' Start the Shiny based interactive visualization tool to show the subgroup results
#' generated by subscreencalc.
#' See and explore all subgroup results at one glance. Pick and chose a specific
#' subgroup, the level of combinations or a certain factor with its combinations.
#' Switch easily between different endpoint/target variables.
#'
#' @param scresults SubScreenResult object with results from a subscreencalc call
#' @param variable_importance variable importance object calculated via subscreenvi to unlock
#'        'variable importance'-tab in the app
#' @param host host name or IP address for Shiny display
#' @param port  port number for Shiny display
#' @param NiceNumbers list of numbers used for a 'nice' scale
#' @param windowTitle title which is shown for the browser tab
#' @param graphSubtitle subtitle for explorer plot
#' @param favour_label_verum_name verum name for label use in explorer graph
#' @param favour_label_comparator_name comparator name for label use in explorer graph
#' @param showTables logical for display tables in 'Explorer tab'
#' @param reference_line_at_start logical for reference line appearance at start (TRUE/FALSE)
#' @param reference_value numeric value of horizontal reference line
#' @param favour_label_at_start logical for favour labels appearance at start (TRUE/FALSE)
#' @param favour_direction logical for favor label direction (TRUE/FALSE) where TRUE means
#'        favour_label_verum_name is on top
#' @param subgroup_levels_at_start integer value for subgroup level slider at start
#' @param yaxis_type character ("lin"/"log") for yaxis type
#' @param add_funnel_at_start logical for funnel appearance at start (TRUE/FALSE)
#'
#' @keywords subgroup analysis visualization
#' @import shiny
#' @importFrom methods is
#' @export subscreenshow

subscreenshow <- function (
    scresults = NULL,
    variable_importance = NULL,
    host = NULL,
    port = NULL,
    NiceNumbers = c(1, 1.5, 2, 4, 5, 6, 8, 10),
    windowTitle = "Subgroup Explorer",
    graphSubtitle = NULL,
    favour_label_verum_name = NULL,
    favour_label_comparator_name = NULL,
    showTables = FALSE,
    reference_line_at_start = FALSE,
    reference_value = 1,
    favour_label_at_start = FALSE,
    favour_direction = TRUE,
    subgroup_levels_at_start = NA,
    yaxis_type = "lin",
    add_funnel_at_start = FALSE
  ){

  ####.. warnings ####
  if (!is.null(scresults)) {
    if (is(scresults) != "SubScreenResult") {
      cat("Error: subscreenshow requires results of class 'SubScreenResult'")
      stop()
    }
  }
  if (!requireNamespace("shiny", quietly = TRUE)) {
    cat("Error: subscreenshow requires the package shiny to be installed")
    stop()
  }
  if (!requireNamespace("shinyjs", quietly = TRUE)) {
    cat("Error: subscreenshow requires the package shinyjs to be installed")
    stop()
  }
  if (!requireNamespace("bsplus", quietly = TRUE)) {
    cat("Error: subscreenshow requires the package bsplus to be installed")
    stop()
  }
  if (!requireNamespace("colourpicker", quietly = TRUE)) {
    cat("Error: subscreenshow requires the package colourpicker to be installed")
    stop()
  }
  if (!requireNamespace("DT", quietly = TRUE)) {
    cat("Error: subscreenshow requires the package DT to be installed")
    stop()
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    cat("Error: subscreenshow requires the package dplyr to be installed")
    stop()
  }

  appDir <- system.file("shiny-app", "subscreen",  "app.R", package = "subscreen")
  if (appDir == "") {
    stop("Could not find Shiny app directory. Please try to re-install `subscreen`.", call. = FALSE)
  }


  if (yaxis_type != "log" & yaxis_type != "lin") {
    cat("Error: parameter yaxis_type needs to be 'lin' or 'log' in subscreenshow")
    stop()
  }
  if (!is.logical(reference_line_at_start)) {
    cat("Error: parameter reference_line_at_start needs to be logical (TRUE/FALSE) in subscreenshow")
    stop()
  }
  if (!is.logical(favour_label_at_start)) {
    cat("Error: parameter favour_label_at_start needs to be logical (TRUE/FALSE) in subscreenshow")
    stop()
  }
  if (!is.logical(favour_direction)) {
    cat("Error: parameter favour_direction needs to be logical (TRUE/FALSE) in subscreenshow")
    stop()
  }
  if (!is.logical(add_funnel_at_start)) {
    cat("Error: parameter add_funnel_at_start needs to be logical (TRUE/FALSE) in subscreenshow")
    stop()
  }
  if (!is.numeric(reference_value)) {
    cat("Error: parameter reference_value needs to be numeric in subscreenshow")
    stop()
  }
  if(!is.na(subgroup_levels_at_start)) {
    if (!subgroup_levels_at_start %% 1 == 0) {
      cat("Error: parameter subgroup_levels_at_start needs to be integer in subscreenshow")
      stop()
    }
  }
  apppars <- list(
    scresults = scresults,
    scresults_name = deparse(substitute(scresults)),
    variable_importance = variable_importance,
    NiceNumbers = NiceNumbers,
    showTables = showTables,
    reference_line_at_start = reference_line_at_start,
    reference_value = reference_value,
    favour_label_at_start = favour_label_at_start,
    favour_direction = favour_direction,
    yaxis_type = yaxis_type,
    subgroup_levels_at_start = subgroup_levels_at_start,
    add_funnel_at_start = add_funnel_at_start
  )

  ui <- server <- NULL

  source(appDir, local=TRUE)
  server_env <- environment(server)

  server_env$apppars <- apppars

  app <- shiny::shinyApp(ui, server)

  shiny::runApp(app, display.mode = "normal", host = host, port = port)

}

