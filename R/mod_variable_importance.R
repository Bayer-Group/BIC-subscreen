#' variable_importance UI Function
#'
#' @description A shiny Module for the variable importance tab in Subscreen.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_variable_importance_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::conditionalPanel(
      condition = "output.importanceenabled == true",
      shiny::wellPanel(
      shiny::radioButtons(
        inputId = ns("Impo_opt"),
        label = shiny::HTML('<p style="color:white"> Importance Value Option </p>'),
        choices = list(
          "No Importance Value" = 0,
          "Use Variable Importance Values" = 1,
          "Use Ranking of Variable Importance Values" = 2
        ),
        selected = 0
      ),
      shiny::uiOutput(ns("select_importance_variable")),
      shiny::conditionalPanel("input.Impo_opt == '1'", ns = ns,
        shiny::div(style = "position:absolute;right:2em;",
          bsplus::bs_embed_tooltip(
            tag = bsplus::shiny_iconlink("question"),
            title = "Use the slider to set the range of 'Important values' which
            can be signified through colors in the plot.",
            placement = "top",
            expanded = TRUE
          )
        ),
        shiny::tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
        shiny::uiOutput(ns("impo"))
      ),
      shiny::conditionalPanel("input.Impo_opt == '2'", ns = ns,
        shiny::div(style = "position:absolute;right:2em;",
          bsplus::bs_embed_tooltip(
            tag = bsplus::shiny_iconlink("question"),
            title = "Use the slider to adjust the number of variables",
            placement = "top",
            expanded = TRUE
          )
        ),
        shiny::uiOutput(ns("impo2"))
      ),
      shiny::conditionalPanel("input.Impo_opt == '2'", ns = ns,
        shiny::div(style = "position:absolute;right:2em;",
          bsplus::bs_embed_tooltip(
            tag = bsplus::shiny_iconlink("question"),
            title = "The variable important order can be prioritized by using increasing or decreasing values.",
            placement = "top",
            expanded = TRUE
          )
        ),
        shiny::radioButtons(
          inputId = ns("decrease"),
          label = shiny::HTML('<p style="color:white"> Sorting order: </p>'),
          choices = list("Increase" = FALSE, "Decrease" = TRUE),
          selected = FALSE
        )
      ),
      shiny::conditionalPanel("input.Impo_opt == '1'" , ns = ns,
        shiny::tableOutput(ns('imp_var_list'))
      ),
      shiny::conditionalPanel("input.Impo_opt == '2'" , ns = ns,
        shiny::div(style = "position:absolute;right:2em;",
          bsplus::bs_embed_tooltip(
            tag = bsplus::shiny_iconlink("question"),
            title = "Variables for colorized dots are displayed in this table.",
            placement = "top",
            expanded = TRUE
          )
        ),
        shiny::tableOutput(ns('imp_var_list2'))
      ),
      bsplus::use_bs_popover(),
      bsplus::use_bs_tooltip()
    )),
    shiny::conditionalPanel(
      condition = "output.importanceenabled == false",
      shiny::wellPanel(
        shiny::p("To use the Variable Importance Feature, please upload a variable importance data set on the upload page.
                  Variable importance data sets can be created using the screenvi-function.")
      )
    )
  )
}

#' variable_importance Server Function
#'
#' @noRd
mod_variable_importance_server <- function(
  input,
  output,
  session,
  variable_importance,
  results
) {

  ns <- session$ns

  vi_importance_IDs <- shiny::reactiveValues(val = NULL)

  vi_variable <- shiny::reactive({
    shiny::req(input$select_importance_variable)
    if (is.null(variable_importance())) {
      NULL
    } else if (!is.null(variable_importance()) & input$select_importance_variable == "NULL") {
      variable_importance()
    } else if (!is.null(variable_importance()) & input$select_importance_variable != "NULL") {
      variable_importance()[[input$select_importance_variable]]
    }
  })

  shiny::observeEvent(vi_variable(), {
    import_reac$reactive <- c(min(vi_variable()$Importance),
                              max(vi_variable()$Importance))
  })
  shiny::observe({
    val <- input$Impo_opt
    if (val == 1) {
      im <- import_reac$reactive

      if (!is.null(im)) {
        vek1 <- vi_variable()[vi_variable()$Importance >= im[1] & vi_variable()$Importance <= im[2], 1]
        vek1 <- vek1[vek1 %in% results()$factors]
        tmp1 <- NULL

        if (length(vek1) > 0) {
          for (i in 1:length(vek1)) {
            tmp1 <- rbind(tmp1, results()$sge[results()$sge[, as.character(eval(parse(text = 'vek1[i]')))] != "Not used", ])
            tmp1 <- unique(tmp1)
          }
        }

        vi_importance_IDs$val <- tmp1
        output$imp_var_list <- shiny::renderTable({
          tab1 <- data.frame('Used importance variables' = vek1)
          names(tab1) <- "Used/colored importance variables"
          tab1
          },
          hover = TRUE,
          spacing = 'xs',
          na = 'none',
          digits = 0,
          caption.placement = 'top'
        )
      }
    }
    if (val == 2) {
      de <- input$decrease
      im2 <- input$impo2
      if(!is.null(im2)){
        vek2 <- shiny::isolate(vi_variable())[
          order(
            shiny::isolate(vi_variable()$Importance), decreasing = as.logical(de)
          )[1:im2], 1]
        vek2 <- vek2[vek2 %in% results()$factors]
        tmp2 <- NULL
         if (length(vek2) > 0) {
          for (i in 1:length(vek2)) {
            tmp2 <- rbind(
              tmp2,
              results()$sge[results()$sge[, as.character(eval(parse(text = 'vek2[i]')))] != "Not used", ]
            )
            tmp2 <- unique(tmp2)
          }
        }
        vi_importance_IDs$val <- tmp2
        output$imp_var_list2 <- shiny::renderTable({
          tab2 <- data.frame('Used importance variables' = vek2)
          names(tab2) <- "Used/colored importance variables"
          tab2
          },
          hover = TRUE,
          spacing = 'xs',
          na = 'none',
          digits = 0,
          caption.placement = 'top'
        )
      }
    }
    if (val == 0) {
      vi_importance_IDs$val <- NULL
    }
  })

   output$select_importance_variable <- shiny::renderUI({
    if (is.data.frame(variable_importance())) {
     choices <- "NULL"
    } else if (is.list(variable_importance())) {
     choices <- names(variable_importance())
    } else  {
     choices <-"NULL"
    }
    shiny::selectInput(
      inputId = ns("select_importance_variable"),
      "Select Variable",
      choices = choices,
      selected = choices[1]
    )
  })

  output$impo <- shiny::renderUI({
    shiny::req(vi_variable())
    shiny::sliderInput(
      inputId = ns("impo"),
      label = "Choose importance Range",
      min = min(vi_variable()$Importance),
      max = max(vi_variable()$Importance),
      value = c(min(vi_variable()$Importance), min(vi_variable()$Importance)),
      step = 0.5
    )
  })

  import_reac <- shiny::reactiveValues(
    reactive = c(NULL, NULL)
  )
  shiny::observeEvent(input$impo, {
    import_reac$reactive <- input$impo
  })

  output$impo2 <- shiny::renderUI({
    shiny::sliderInput(
      inputId = ns("impo2"),
      label = "Choose number of Variables which are most important",
      min = 1,
      max = length(vi_variable()$Importance),
      value = 1,
      step = 1
    )
  })

  return(
    list(
      val = shiny::reactive({vi_importance_IDs$val$SGID})
    )
  )
}
