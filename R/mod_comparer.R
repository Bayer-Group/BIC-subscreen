#' comparer module User-Interface
#'
#' @description A shiny Module for the Comparer-tab in Subscreen.
#'
#' @param id Internal parameters for shiny.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_comparer_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      shiny::column(3,
        shiny::wellPanel(
          shiny::div(
            style = "position:absolute;right:2em;",
            bsplus::bs_embed_tooltip(
              tag = bsplus::shiny_iconlink("question"),
              title = "Variable plotted on the y-axis (upper plot).",
              placement = "top",
              expanded = TRUE
            )
          ),
          shiny::uiOutput(ns("y1")),
          shiny::div(
            style = "position:absolute;right:2em;",
            bsplus::bs_embed_tooltip(
              tag = bsplus::shiny_iconlink("question"),
              title = "Change the scale on the y-axis (upper plot).",
              placement = "top",
              expanded = TRUE
            )
          ),
          shiny::uiOutput(ns("plot_type2")),
          shiny::div(
            style = "position:absolute;right:2em;",
            bsplus::bs_embed_tooltip(
              tag = bsplus::shiny_iconlink("question"),
              title = "Change the y-axis limits (upper plot).",
              placement = "top",
              expanded = TRUE
            )
          ),
          shiny::uiOutput(ns("YRange2")),
          bsplus::use_bs_popover(),
          bsplus::use_bs_tooltip()
        ),
        shiny::wellPanel(
          shiny::div(
            style = "position:absolute;right:2em;",
            bsplus::bs_embed_tooltip(
              tag = bsplus::shiny_iconlink("question"),
              title = "Variable t obe plotted on the y-axis (lower plot).",
              placement = "top",
              expanded = TRUE
            )
          ),
          shiny::uiOutput(ns("y2")),
          shiny::div(
            style = "position:absolute;right:2em;",
            bsplus::bs_embed_tooltip(
              tag = bsplus::shiny_iconlink("question"),
              title = "Change the scale on the y-axis (lower plot).",
              placement = "top",
              expanded = TRUE
            )
          ),
          shiny::uiOutput(ns("plot_type3")),
          shiny::div(
            style = "position:absolute;right:2em;",
            bsplus::bs_embed_tooltip(
              tag = bsplus::shiny_iconlink("question"),
              title = "Change y-axis limits (lower plot).",
              placement = "top",
              expanded = TRUE
            )
          ),
          shiny::uiOutput(ns("YRange3")),
          bsplus::use_bs_popover(),
          bsplus::use_bs_tooltip()
        ),
        shiny::wellPanel(
          shiny::div(style = "position:absolute;right:2em;",
            bsplus::bs_embed_tooltip(tag = bsplus::shiny_iconlink("question"),
            title = "Variable plotted on the x-axes.",
            placement = "top",
            expanded = TRUE
            )
          ),
          shiny::uiOutput(ns("x2"))
        )
      ),
      shiny::mainPanel(
        shiny::tabsetPanel(type = "tabs",
          shiny::tabPanel("Compare",
            mod_legend_ui(ns("legend2")),
            shiny::column(7,
              mod_graph_ui(ns("graph2"),
               plotHeight = 390,
               plotWidth = 1100
              )
            ),
            shiny::column(7,
              mod_graph_ui(ns("graph3"),
                plotHeight = 390,
                plotWidth = 1100
              )
            )
          ),
          ####.. Bubble plot####
          shiny::tabPanel("Bubble plot",
            mod_legend_ui(ns("legend3")),
            shiny::column(7,
              mod_bubble_ui(ns("graph4"),
                plotHeight = 780,
                plotWidth = 1100
              )
            )
          )
        )
      )
    )
  )
}

#' comparer module server side
#'
#' @param input,output,session Internal parameters for shiny.
#' @param results SubScreenResult object with results from a subscreencalc call
#' @param YRange Range of y-axis
#' @param XRange Range of x-axis
#' @param plot_type Type.
#' @param point_size size of circles.
#' @param pch_value symbol of circles.
#' @param color plot color.
#' @param ColorBGplot background color.
#' @param ColorTabClicked clicked circle color.
#' @param ColorPoints circle color.
#' @param colthemeCol theme color.
#' @param ColorReference reference color.
#' @param ColorMemorized memorized circle/label color.
#' @param x x variable
#' @param y y variable
#' @param plot_points_data_complement complement results.
#' @param key number factors.
#' @param pickradius radius size.
#' @param nice_Numbers nice numbers.
#' @param xlabel x label.
#' @param grid grid displayed (TRUE/FALSE)
#' @param circlestyle circle style.
#' @param memorized_Data memorized data.
#' @param point_brightness brigthness of circles.
#' @noRd
mod_comparer_server <- function(input, output, session,
    results,
    YRange,
    XRange,
    plot_type,
    point_size,
    color,
    ColorBGplot,
    ColorTabClicked,
    ColorPoints,
    colthemeCol,
    ColorReference,
    ColorMemorized,
    x,
    y,
    plot_points_data_complement,
    key,
    pickradius,
    nice_Numbers,
    xlabel,
    grid,
    circlestyle,
    memorized_Data,
    point_brightness,
    show_ref_line,
    add_custom_ref_line,
    value_custom_ref_line,
    show_favour_arrows,
    favour_direction,
    favour_comparator_name,
    favour_verum_name
) {

  ns <- session$ns

  plot_points_data2 <- shiny::reactive({
    shiny::req(key(), input$x2, input$y1)
    data.frame(
      x = results()$sge[, c(input$x2)][results()$sge$nfactors >= key()[1] & results()$sge$nfactors <= key()[2]],
      y = results()$sge[, c(input$y1)][results()$sge$nfactors >= key()[1] & results()$sge$nfactors <= key()[2]],
      ID = results()$sge[, "SGID"][results()$sge$nfactors >= key()[1] & results()$sge$nfactors <= key()[2]]
    )
  })

 plot_points_data3 <- shiny::reactive({
    shiny::req(key(), input$x2, input$y2)
    data.frame(
      x = results()$sge[, c(input$x2)][results()$sge$nfactors >= key()[1] & results()$sge$nfactors <= key()[2]],
      y = results()$sge[, c(input$y2)][results()$sge$nfactors >= key()[1] & results()$sge$nfactors <= key()[2]],
      ID = results()$sge[, "SGID"][results()$sge$nfactors >= key()[1] & results()$sge$nfactors <= key()[2]]
    )
  })

  plot_points_data4 <- shiny::reactive({
    shiny::req(key(), input$y1, input$y2)
    if (input$y1 %in% colnames(results()$sge) & input$y2 %in% colnames(results()$sge)) {
      data.frame(
        x = results()$sge[, c(input$y1)][results()$sge$nfactors >= key()[1] & results()$sge$nfactors <= key()[2]],
        y = results()$sge[, c(input$y2)][results()$sge$nfactors >= key()[1] & results()$sge$nfactors <= key()[2]],
        ID = results()$sge[, "SGID"][results()$sge$nfactors >= key()[1] & results()$sge$nfactors <= key()[2]]
      )
    }
  })

  # helper functions
  roundUpNice <- function(x, nice = nice_Numbers) {
    if (length(x) != 1) stop("'x' must be of length 1")
    if (x >= 0) 10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) *nice)[[1]]]]
    else -1 * (roundDownNice(-x, nice = nice_Numbers))
  }

  roundDownNice <- function(x, nice = nice_Numbers) {
    if (length(x) != 1) stop("'x' must be of length 1")
    if (x >= 0) 10^floor(log10(x)) * nice[[max(which(x >= 10^floor(log10(x)) * nice))]]
    else -1 * (roundUpNice(-x, nice = nice_Numbers))
  }

  output$y1 <- shiny::renderUI({
     choi <- setdiff(
      setdiff(names(results()$results_total[ ,!is.na(results()$results_total)]),'N.of.subjects'),
      names(which(apply(results()$sge[,names(results()$results_total)],2,function(x) {(!all(is.finite(x[!is.na(x)])))})))
    )
    shiny::selectInput(
      inputId = ns("y1"),
      label = "First Target variable",
      choices = choi,
      selected = choi[1]
    )
  })

  output$plot_type2 <- shiny::renderUI({
    shiny::radioButtons(
      inputId = ns("plot_type2"),
      label ="Type (Compare Plot: y-axis / Bubble Plot: x-axis)",
      selected = "lin",
      inline = TRUE,
      choiceNames = list("linear", "logarithmic"),
      choiceValues = c("lin", "log")
    )
  })

    shiny::observeEvent(input$y1, {
      shiny::req(input$y1)
      shiny::req(results())
    if (roundDownNice(min(results()$sge[, input$y1], na.rm = TRUE), nice = nice_Numbers) <= 0) {
      shiny::updateRadioButtons(
        inputId = "plot_type2",
        label = "Type",
        choices = c(linear = "lin"),
        selected = "lin",
        inline = TRUE
      )
    } else {
      shiny::updateRadioButtons(
        inputId = "plot_type2",
        label = "Type",
        choices = c(linear = "lin", log = "log"),
        selected = "lin",
        inline = TRUE
      )
    }
  })

   shiny::observeEvent(input$y2, {
      shiny::req(input$y2)
      shiny::req(results())
    if (roundDownNice(min(results()$sge[, input$y2], na.rm = TRUE), nice = nice_Numbers) <= 0) {
      shiny::updateRadioButtons(
        inputId = "plot_type3",
        label = "Type",
        choices = c(linear = "lin"),
        selected = "lin",
        inline = TRUE
      )
    } else {
      shiny::updateRadioButtons(
        inputId = "plot_type3",
        label = "Type",
        choices = c(linear = "lin", log = "log"),
        selected = "lin",
        inline = TRUE
      )
    }
  })


  output$YRange2 <- shiny::renderUI({
    shiny::req(input$y1)
    shiny::req(input$plot_type2)
    if (input$plot_type2 == "lin") {
      shiny::req(results())
      if (input$y1 %in% colnames(results()$sge)) {
        shiny::sliderInput(
          inputId = ns("YRange2"),
          label = "Range (Compare Plot: y-axis / Bubble Plot: x-axis)",
          min = roundDownNice(min(results()$sge[, input$y1], na.rm = TRUE), nice = nice_Numbers),
          max = roundUpNice(max(results()$sge[, input$y1], na.rm = TRUE), nice = nice_Numbers),
          value = c(roundDownNice(min(results()$sge[, input$y1], na.rm = TRUE), nice = nice_Numbers),roundUpNice(max(results()$sge[, input$y1], na.rm = TRUE), nice = nice_Numbers)),
          step = roundUpNice((max(results()$sge[, input$y1], na.rm = TRUE) - min(results()$sge[, input$y1], na.rm = TRUE))/100, nice = nice_Numbers)
        )
      }
    } else {
      rg.z <- log(
        range(
          roundDownNice(min(results()$sge[, input$y1], na.rm = TRUE), nice = nice_Numbers),
          roundUpNice(max(results()$sge[, input$y1], na.rm = TRUE), nice = nice_Numbers)
        )
      )
      choices <- unique(
        unlist(
          lapply(
            exp(seq(rg.z[1], rg.z[2], length.out = 20)),
            function(x) {signif(x, 2)}
          )
        )
      )
      shinyWidgets::sliderTextInput(
        inputId = ns("YRange2"),
        label = "Log Range (Compare Plot: y-axis / Bubble Plot: x-axis)",
        hide_min_max = TRUE,
        choices = choices,
        selected = c(choices[1], choices[length(choices)]),
        grid = TRUE
      )
     }
  })

  output$y2 <- shiny::renderUI({

    choi <- setdiff(
      setdiff(names(results()$results_total[ ,!is.na(results()$results_total)]),'N.of.subjects'),
      names(which(apply(results()$sge[,names(results()$results_total)],2,function(x) {(!all(is.finite(x[!is.na(x)])))})))
    )
    shiny::selectInput(
      inputId = ns("y2"),
      label = "Second Target variable",
      choices = choi,
      selected = choi[2]
    )
  })

  output$plot_type3 <- shiny::renderUI({
    shiny::radioButtons(
      inputId = ns("plot_type3"),
      label = "Type (Compare Plot: y-axis / Bubble Plot: y-axis)",
      choiceNames = list("linear", "logarithmic"),
      choiceValues = c("lin", "log"),
      selected = "lin",
      inline = TRUE
    )
  })

  output$x2 <- shiny::renderUI({
    shiny::selectInput(
      inputId = ns("x2"),
      label = "Reference variable",
      choices = names(results()$results_total[,!is.na(results()$results_total)]),
      selected =  "N.of.subjects"
    )
  })

  output$YRange3 <- shiny::renderUI({
    shiny::req(input$y2)
    shiny::req(input$plot_type3)

    if (input$plot_type3 == "lin") {
    shiny::req(results())
    if (input$y2 %in% colnames(results()$sge)) {
      shiny::sliderInput(
        inputId = ns("YRange3"),
        label = "Y Range (Compare Plot: y-axis / Bubble Plot: y-axis)",
        min = roundDownNice(min(results()$sge[, input$y2], na.rm = TRUE), nice = nice_Numbers),
        max = roundUpNice(max(results()$sge[, input$y2], na.rm = TRUE), nice = nice_Numbers),
        value = c(roundDownNice(min(results()$sge[, input$y2], na.rm = TRUE), nice = nice_Numbers), roundUpNice(max(results()$sge[, input$y2], na.rm = TRUE), nice = nice_Numbers)),
        step = roundUpNice((max(results()$sge[, input$y2], na.rm = TRUE) - min(results()$sge[, names(results()$results_total[ ,!is.na(results()$results_total)])[2]], na.rm = TRUE))/100, nice = nice_Numbers)
      )
    }
  } else {
      rg.z <- log(
        range(
          roundDownNice(min(results()$sge[, input$y2], na.rm = TRUE), nice = nice_Numbers),
          roundUpNice(max(results()$sge[, input$y2], na.rm = TRUE), nice = nice_Numbers)
        )
      )

      choices <- unique(
        unlist(
          lapply(
            exp(seq(rg.z[1], rg.z[2], length.out = 20)),
            function(x) {signif(x, 2)}
          )
        )
      )

      shinyWidgets::sliderTextInput(
        inputId = ns("YRange3"),
        label = "Log Range (Compare Plot: y-axis / Bubble Plot: y-axis)",
        hide_min_max = TRUE,
        choices = choices,
        selected = c(choices[1], choices[length(choices)]),
        grid = TRUE
      )
     }
  })

  shiny::callModule(
    mod_legend_server,
    "legend2",
    plot_color = color,
    colthemeCol = colthemeCol,
    rowwise = TRUE,
    complement = shiny::reactive({ifelse(!is.null(plot_points_data_complement()), TRUE, FALSE)}),
    point_brightness = point_brightness
  )

  shiny::callModule(
    mod_legend_server,
    "legend3",
    plot_color = color,
    colthemeCol = colthemeCol,
    rowwise = TRUE,
    complement = shiny::reactive({FALSE}),
    point_brightness = point_brightness
  )

  mod_graph_vars2 <- shiny::callModule(
    module = mod_graph_server,
    id = "graph2",
    results = results,
    plot_point = shiny::reactive({plot_points_data2()}),
    YRange = shiny::reactive({shiny::req(input$YRange2)}),
    XRange = shiny::reactive({NULL}),
    plot_type = shiny::reactive({shiny::req(input$plot_type2)}),
    point_size = point_size,
    #pch_value = pch_value,
    color = color,
    ColorBGplot = ColorBGplot,
    ColorTabClicked = ColorTabClicked,
    ColorReference = ColorReference,
    ColorPoints = ColorPoints,
    ColorMemorized = ColorMemorized,
    x = shiny::reactive({shiny::req(input$x2)}),
    y = shiny::reactive({shiny::req(input$y1)}),
    plot_points_data_complement = plot_points_data_complement,
    pickradius = pickradius,
    key = key,
    nice_Numbers = nice_Numbers,
    xlabel = xlabel,
    grid = grid,
    circlestyle = circlestyle,
    memorized_Data = memorized_Data,
    show_ref_line = show_ref_line,
    add_custom_ref_line  = add_custom_ref_line,
    value_custom_ref_line  = value_custom_ref_line,
    show_favour_arrows = show_favour_arrows,
    favour_direction = favour_direction,
    favour_verum_name = favour_verum_name,
    favour_comparator_name = favour_comparator_name
  )

  mod_graph_vars3 <- shiny::callModule(
    module = mod_graph_server,
    id = "graph3",
    results = results,
    plot_point = shiny::reactive({plot_points_data3()}),
    YRange = shiny::reactive({shiny::req(input$YRange3)}),
    XRange = shiny::reactive({NULL}),
    plot_type = shiny::reactive({shiny::req(input$plot_type3)}),
    point_size = point_size,
    #pch_value = pch_value,
    color = color,
    ColorBGplot = ColorBGplot,
    ColorTabClicked = ColorTabClicked,
    ColorReference = ColorReference,
    ColorPoints = ColorPoints,
    ColorMemorized = ColorMemorized,
    x = shiny::reactive({shiny::req(input$x2)}),
    y = shiny::reactive({shiny::req(input$y2)}),
    plot_points_data_complement = plot_points_data_complement,
    key = key,
    pickradius = pickradius,
    nice_Numbers = nice_Numbers,
    xlabel = xlabel,
    grid = grid,
    circlestyle = circlestyle,
    memorized_Data = memorized_Data,
    show_ref_line = show_ref_line,
    add_custom_ref_line  = add_custom_ref_line,
    value_custom_ref_line  = value_custom_ref_line,
    show_favour_arrows = show_favour_arrows,
    favour_direction = favour_direction,
    favour_verum_name = favour_verum_name,
    favour_comparator_name = favour_comparator_name
  )


  mod_graph_vars4 <- shiny::callModule(
    module = mod_bubble_server,
    id = "graph4",
    results = results,
    plot_point = shiny::reactive({plot_points_data4()}),
    XRange = shiny::reactive({shiny::req(input$YRange2)}),
    YRange = shiny::reactive({shiny::req(input$YRange3)}),
    plot_type = shiny::reactive({shiny::req(input$plot_type2)}),
    plot_type2 = shiny::reactive({shiny::req(input$plot_type3)}),
    point_size = point_size,
    #pch_value = pch_value,
    color = color,
    ColorBGplot = ColorBGplot,
    ColorTabClicked = ColorTabClicked,
    ColorReference = ColorReference,
    ColorPoints = ColorPoints,
    x = shiny::reactive({shiny::req(input$x2)}),
    y = shiny::reactive({shiny::req(input$y1)}),
    y2 = shiny::reactive({shiny::req(input$y2)}),
    plot_points_data_complement = plot_points_data_complement,
    key = key,
    pickradius = pickradius,
    nice_Numbers = nice_Numbers,
    xlabel = xlabel,
    grid = grid,
    circlestyle = circlestyle
  )

 return(
    list(
      clicked_points2 = shiny::reactive({mod_graph_vars2$clicked_points()}),
      plot_clicked2 = shiny::reactive({mod_graph_vars2$plot_click()}),
      clicked_points3 = shiny::reactive({mod_graph_vars3$clicked_points()}),
      plot_clicked3 = shiny::reactive({mod_graph_vars3$plot_click()}),
      clicked_points4 = shiny::reactive({mod_graph_vars4$clicked_points()}),
      plot_clicked4 = shiny::reactive({mod_graph_vars4$plot_click()}),
      selected_SGIDs2 = shiny::reactive({mod_graph_vars2$selected_SGIDs()}),
      selected_SGIDs3 = shiny::reactive({mod_graph_vars3$selected_SGIDs()}),
      selected_SGIDs4 = shiny::reactive({mod_graph_vars4$selected_SGIDs()})
    )
  )
}
