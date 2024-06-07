#' User interface part of module 'mod_asmus'
#' used in R shiny App 'Subgroup Explorer' in R package 'subscreen'
#'
#' @param id Internal parameter for shiny
#'
#' @import shiny
#' @noRd

asmus2_module_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::tags$style(
      shiny::HTML(
        paste0(
          ".dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing,.dataTables_wrapper .dataTables_paginate .paginate_button, .dataTables_wrapper .dataTables_paginate .paginate_button.disabled {
          color: #ffffff !important;
          }"
        )
      )
    ),
    shiny::column(12,
      shiny::HTML(
        "<h3>
          <b style = 'color: #e2b007'>A</b>dvanced
          <b style = 'color: #e2b007'>S</b>creening of one- or
          <b style='color: #e2b007'>MU</b>lti-factorial
          <b style='color: #e2b007'>S</b>ubgroups -
          <b style='color: #e2b007'>ASMUS</b>
        </h3>"
      )
    ),
    shiny::uiOutput(ns("helptext")),
    shiny::uiOutput(ns("missing_factor_text")),
    shiny::conditionalPanel(condition = paste0('output[\'', ns('cond_panel'), "\'] == true"),
      shiny::conditionalPanel(condition = paste0('output[\'', ns('PageASMUS'), "\'] == true"),
        shiny::column(12,
          shiny::fluidRow(
            column(12,
              shiny::HTML(
                "<p>
                  Please make a selection and press
                  <b style = 'background-color:#1eba1e; color:#fff'>Continue </b>.
                </p>"
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(5,
              shiny::column(2,
                shiny::tags$style(
                  ".btn-asmuscustom {background-color: #e2b007; color: #FFF;}"
                ),
                shiny::uiOutput(ns("asmus2_color_dropdown")),
                shinyWidgets::dropdownButton(
                  inputId = ns("MyDropDown2"),
                  shiny::tags$h4("About ASMUS:"),
                  shiny::tags$h5(shiny::tags$b(shiny::tags$u("What is a factorial context?"))),
                  "For a subgroup defined by one factor the context consists of all levels of that factor. For multi-factorial subgroups the context is the set of all combinations of levels of the respective factors.",
                  shiny::tags$h5(shiny::tags$b(shiny::tags$u("Completeness of factorial contexts"))),
                  "Complete factorial context: For all possible factor level combinations there is an estimate",
                  "Incomplete factorial context: For at least one factor level combination no estimate is available",
                  "Pseudo(-complete) factorial context: An incomplete factorial context that can be made complete ignoring certain factor levels",
                  circle = TRUE,
                  status = "asmuscustom",
                  icon = icon("question"),
                  width = "300px",
                  tooltip = shinyWidgets::tooltipOptions(title = "Click to see further Information!")
                )
              ),
              shiny::column(10,
                shiny::selectInput(
                  inputId = ns("asmus2_y"),
                  label = "Target variable",
                  choices = NULL,
                  selected = NULL

                )
              ),
              shiny::column(4,
                shiny::radioButtons(
                  inputId = ns("asmus2_plot_type"),
                  label ="Type",
                  selected = "lin",
                  inline = TRUE,
                  choiceNames = list("linear", "logarithmic"),
                  choiceValues = c("lin", "log")
                )
              ),
              shiny::column(8,
                shiny::uiOutput(ns("asmus2_slider"))
              )
            ),
            shiny::column(2,
              shiny::tagList(
                shiny::HTML("<p> Do you want to include pseudo factorial context?"),
                shinyWidgets::prettyToggle(
                  inputId = ns("asmus_include_pseudo"),
                  label_on = "Yes",
                  icon_on = icon("check"),
                  status_on = "info",
                  value = TRUE,
                  status_off = "warning",
                  label_off = "No",
                  icon_off = icon("remove")
                )
              )
            ),
            shiny::column(4,
              shiny::uiOutput(ns('asmus2_total_numbers'))
            )
          ),
          shiny::fluidRow(
            shiny::column(5,
              shiny::plotOutput(
                outputId = ns("graph_asmus2"),
                hover = hoverOpts(
                  ns("graph_asmus_hover1"),
                  delay = 300,
                  delayType = "debounce"
                ),
              )
            ),
            shiny::uiOutput(ns("asmus_hover_info1")),
            shiny::column(6,
              shiny::column(12,
                shinyWidgets::materialSwitch(
                  inputId = ns("asmus_use_ref"),
                  label = "Show all subgroups as reference (Includes subgroups with incomplete factorial context)",
                  value = TRUE,
                  status = "primary"
                )
              ),
              shiny::column(12,
                shiny::HTML(
                  paste0(
                    "<p> Please consider a lower limit value for which the target value
                      might be big enough to be remarkable and an upper limit value for which the
                      target variable is definitely remarkable. Use the 'arrow'-button, if lower
                      values are remarkable.
                    </p>"
                  )
                )
              ),
              shiny::column(6,
                shiny::numericInput(
                  inputId = ns("asmus2_remarkability_1"),
                  label = "Lower value (y-axis):",
                  value  = NULL
                )
              ),
              shiny::column(6,
                shiny::numericInput(
                  inputId = ns("asmus2_remarkability_2"),
                  label = "Upper value (y-axis):",
                  value  = NULL
                )
              ),
              shiny::column(12,
                shinyWidgets::prettyToggle(
                  inputId = ns("asmus_direction"),
                  label_on = "High values are remarkable",
                  icon_on = icon("arrow-up"),
                  status_on = "success",
                  value = TRUE,
                  status_off = "danger",
                  label_off = "Smaller values are remarkable",
                  icon_off = icon("arrow-down")
                )
              ),
              shiny::column(12,
                shiny::HTML(
                  paste0(
                    "<p> Please consider a lower limit value for which the number of subjects
                      might be big enough to be reliable and an upper limit value for which the
                      number of subjects is definitely reliable.
                    </p>"
                  )
                )
              ),
              shiny::column(6,
                shiny::numericInput(
                  inputId = ns("asmus2_reliability_1"),
                  label = "Lower value (x-axis):",
                  value  = NULL
                )
              ),
              shiny::column(6,
                shiny::numericInput(
                  inputId = ns("asmus2_reliability_2"),
                  label = "Upper value (x-axis):",
                  value  = NULL
                )
              ),
              shiny::column(6,
                shiny::uiOutput(ns('cont1_text')),
                shiny::uiOutput(ns('cont1')),
                shiny::uiOutput(ns('cont2_text'))
              ),
              shiny::column(4,
                shiny::numericInput(
                  inputId = ns("fuzzy_multiplicity_value"),
                  label = "Multiplicity value [0,1]",
                  value = 0.25,
                  step = 0.05,
                  min = 0,
                  max = 1
                )
              )
            )
          ),
          shiny::fluidRow(
            shiny::column(2,
              shiny::actionButton(
                inputId = ns("asmus2_continue"),
                label = "Continue",
                icon = icon("step-forward")
              ),
              shiny::uiOutput(ns('continue_cont1')),
              shiny::uiOutput(ns('continue_cont2_text'))
            ),
            shiny::column(10,
              shiny::uiOutput(ns("asmus2_rel_and_rem_number_text"))
            )
          )
        )
      ),
      shiny::conditionalPanel(condition = paste0('output[\'', ns('PageASMUS'), "\'] == false"),
        shiny::column(12,
          shiny::uiOutput(ns('helptext_graph2'))
        ),
        shiny::column(12,
            shiny::actionButton(
              inputId = ns("asmus2_back"),
              label = "Back",
              icon = icon("step-backward"),
              style = "color: #fff; background-color: #ba1637; border-color: #fff"
            ),
            br(),
            br()
          ),
          shiny::column(5,
            DT::dataTableOutput(ns('asmus2_table_rem_and_rel')),
            shiny::helpText("The memorized subgroups are displayed in the 'Memorized Subgroups'-tab within the 'Explorer'-tab.")
          ),
          shiny::column(7,
            shiny::column(6,
              shiny::plotOutput(ns('graph2_asmus2'))
            ),
            br(),
            shiny::column(6,
              shiny::plotOutput(ns('asmus2_interaction'))
            ),
            shiny::tags$head(
              shiny::tags$style(
                ".fa-mouse-pointer {color: #ff8a8a}"
              )
            ),
            shiny::column(12,
              shiny::tabsetPanel(type = "tabs",
                shiny::tabPanel(
                  "Selected Subgroup",
                  DT::dataTableOutput(ns("selectedSG_asmus2")),
                  icon = icon("mouse-pointer")

                ),
                shiny::tabPanel(
                  title = "Parent Subgroups",
                  DT::dataTableOutput(ns("parents_asmus2")),
                  icon = icon("sitemap")
                ),
                shiny::tabPanel(
                  title = "Factorial Contexts",
                  DT::dataTableOutput(ns("factorial_asmus2")),
                  icon = icon("list")
              )
            )
          )
        )
      )
    )
  )
}

#' Server part of module 'mod_asmus'
#' used in R shiny App 'Subgroup Explorer' in R package 'subscreen'
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @param results SubScreenResult object with results from a subscreencalc call
#' @param ColorReference reference line color
#' @param ColorBGplot background color
#' @param ColorPoints point color
#' @param nice_Numbers list of numbers used for a 'nice' scale
#'
#' @noRd
#' @importFrom dplyr %>%

asmus2_module_server <- function(
  input,
  output,
  session,
  results,
  ColorReference,
  ColorBGplot,
  ColorPoints,
  nice_Numbers
) {
  # set module namespace
  ns <- session$ns

  SGID <- nfactors <- N.of.subjects <- delete <- fun2_ <- NULL
  #### FUNCTIONS ####

  calc_y = function(x, a_rem1, a_rem2, a_rel1, a_rel2, multiplicity_val = 0.25, direction_up = TRUE) {
    if (direction_up) {
      return(
        (multiplicity_val * (((a_rel2*a_rem2) - (a_rel2*a_rem1) - (a_rel1*a_rem2) + (a_rel1*a_rem1))) - (a_rel1*a_rem1) + (a_rem1*x)) / (x-a_rel1)
      )
    } else {
      return(
        (multiplicity_val * (((a_rel2*a_rem2) - (a_rel2*a_rem1) - (a_rel1*a_rem2) + (a_rel1*a_rem1))) + (a_rel1*a_rem2) - (a_rem2*x)) / (-x + a_rel1)
      )
    }
  }

  font_color <- function (hex_code) {
    ifelse(
      ((grDevices::col2rgb(hex_code)[1] * 0.299) + (grDevices::col2rgb(hex_code)[2] * 0.587) + (grDevices::col2rgb(hex_code)[3] * 0.114) > 186),
      "#000000",
      "#ffffff"
    )
  }

  different_hues <- function(hex_code, value = 21) {
    ifelse(
      ((grDevices::col2rgb(hex_code)[1] * 0.299) + (grDevices::col2rgb(hex_code)[2] * 0.587) + (grDevices::col2rgb(hex_code)[3] * 0.114) > 186),
      grDevices::rgb(max(grDevices::col2rgb(hex_code)[1] - value, 0), max(grDevices::col2rgb(hex_code)[2] - value, 0), max(grDevices::col2rgb(hex_code)[3] - value, 0), maxColorValue = 255),
      grDevices::rgb(min(grDevices::col2rgb(hex_code)[1] + value, 255), min(grDevices::col2rgb(hex_code)[2] + value, 255), min(grDevices::col2rgb(hex_code)[3] + value,255), maxColorValue = 255)
    )
  }

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

  is.integer0 <- function(x) {
    is.integer(x) && length(x) == 0L
  }


  factorialContext <- function(data, SGID) {
    SGID <- SGID[1]
    if (is.null(SGID) | is.integer0(SGID)){} else {

      nfac <- data$sge[which(data$sge$SGID == SGID), ]$nfactors
      tmp <- colnames(data$sge[which(data$sge$SGID == SGID), data$factors])[which(data$sge[data$sge$SGID == SGID, data$factors] != "Not used")]
      tmp2 <- data$sge[apply(data$sge[ , c("SGID","nfactors", tmp)] != "Not used", 1, sum) == (2 + nfac), ]
      tmp3 <- tmp2[tmp2$nfactors == nfac,]
      ges <- 1
      if (length(tmp) > 0) {
        for(i in 1:length(tmp)) {
          ges <- sum(levels(data$sge[[tmp[i]]]) != "Not used") * ges
        }
      } else {
         ges <- 0
      }

      status_ <- ifelse(ges == dim(tmp3)[1], "Complete", "Incomplete")

      return(list(
        'Factorial' = tmp3,
        'Number Factors' = nfac,
        'Variables' = tmp,
        'Status' = status_
        )
      )
    }
  }

  zf <- function(x,a,b, direction_up = TRUE){
      d <- b-a
      if (direction_up) {
        s <- ifelse(x < a, 0, ifelse(x < b, (x-a)/d, 1))
      } else {
        s <- ifelse(x < a, 1, ifelse(x < b, (b-x)/d, 0))
      }
    mean(s)
  }

  #### REACTIVE VALUES ####
  #reactive value if the asmus tab should be shown
  cond_panel_val <- shiny::reactiveValues(val = FALSE)
  output$cond_panel <- shiny::reactive({
    cond_panel_val$val
  })
  outputOptions(output, "cond_panel", suspendWhenHidden = FALSE)

  #### OBSERVER ####
  # observe new data set
  shiny::observeEvent(results(), {
    if (!is.null(results())) {
      if (!any(startsWith(colnames(results()$sge),"FCID_complete_"))
        & (!"FCID_complete" %in% colnames(results()$sge))) {
        cond_panel_val$val <- FALSE
        output$helptext <- shiny::renderUI({
            shiny::HTML(
             "<p>
              To use the ASMUS-tab, please set parameter <code>factorial = TRUE</code> in
              the subscreencalc() function call.
              </p>
              "
            )
        })
      }
      if (!any(startsWith(colnames(results()$sge),"FCID_complete_"))
        & ("FCID_complete" %in% colnames(results()$sge))) {
        cond_panel_val$val <- FALSE
        output$helptext <- shiny::renderUI({
            shiny::HTML(
             "<p>
              Outdated results structure detected.
              Please use package version (>4.0.0) of subscreencalc to use the ASMUS-tab.
              </p>
              "
            )
        })
      }

      if (any(startsWith(colnames(results()$sge),"FCID_complete_"))
        ) {
        cond_panel_val$val <- TRUE
        output$helptext <- shiny::renderUI({
            shiny::HTML(
             "<p>
              </p>
              "
            )
        })
      }
    } else {
      output$helptext <- shiny::renderUI({
          shiny::HTML(
           "<p>
            </p>
            "
          )
      })
    }
  })

  #### 1. if results() changes, update (target variable) input$asmus2_y : ####
  shiny::observeEvent(results(), {
    if(cond_panel_val$val) {
    if(!is.null(results())) {
      if (any(startsWith(colnames(results()$sge),"FCID_complete_"))) {

       choi <- setdiff(
          setdiff(names(results()$results_total[ ,!is.na(results()$results_total)]),'N.of.subjects'),
          names(which(apply(results()$sge[,names(results()$results_total)],2,function(x) {(!all(is.finite(x[!is.na(x)])))})))
        )

        shiny::updateSelectInput(
          session,
          inputId = "asmus2_y",
          label = "Target variable",
          choices = choi,
          selected = choi[1]
        )

      }
    } else {
      shiny::updateSelectInput(
        session,
        inputId = "asmus2_y",
        label = "Target variable",
        choices = NULL,
        selected = NULL
      )
    }
    }
  })

  #### 2. if input$asmus2_y changes, update all parameters: ####
  shiny::observeEvent(input$asmus2_y, {
    shiny::req(input$asmus2_y)
    if(cond_panel_val$val) {
     ## update all widgets
    if (!is.null(results()$sge[, input$asmus2_y])) {

     if (roundDownNice(min(results()$sge[, input$asmus2_y], na.rm = TRUE), nice = nice_Numbers) <= 0) {
        shiny::updateRadioButtons(
          session,
          inputId = "asmus2_plot_type",
          selected = "lin",
          inline = TRUE,
          choiceNames = list("linear"),
          choiceValues = c("lin")
          )
      } else {
        shiny::updateRadioButtons(
          session,
          inputId = "asmus2_plot_type",
          label = "Type",
          inline = TRUE,
          choiceNames = list("linear", "logarithmic"),
          choiceValues = c("lin", "log")
        )
      }
    }
    shinyWidgets::updatePrettyToggle(
      session,
      inputId = "asmus_include_pseudo",
      value = TRUE
    )

     shinyWidgets::updateMaterialSwitch(
      session,
      inputId = "asmus_use_ref",
      value = TRUE
    )

    shinyWidgets::updatePrettyToggle(
      session,
      inputId = "asmus_direction",
      value = TRUE
    )

    shiny::updateNumericInput(
      session,
      inputId ="asmus2_remarkability_1",
      value  = reactive({NULL})
    )
    shiny::updateNumericInput(
      session,
      inputId ="asmus2_remarkability_2",
      value = reactive({NULL})
    )
        shiny::updateNumericInput(
      session,
      inputId ="asmus2_reliability_1",
      value  = reactive({NULL})
    )
    shiny::updateNumericInput(
      session,
      inputId ="asmus2_reliability_2",
      value = reactive({NULL})
    )
     shiny::updateNumericInput(
       session,
        inputId = "fuzzy_multiplicity_value",
        value = 0.25
      )
  }
  })

  #### OUTPUTS ####

  ####... asmus2_slider ####
  output$asmus2_slider <- shiny::renderUI({
    if(!is.null(results())) {
    if (any(startsWith(colnames(results()$sge),"FCID_complete_"))) {
    if (input$asmus2_plot_type == "lin") {

      shiny::sliderInput(
        inputId = ns("asmus2_slider"),
        label = "Y Range",
        min = roundDownNice(min(results()$sge[, input$asmus2_y], na.rm = TRUE), nice = nice_Numbers),
        max = roundUpNice(max(results()$sge[, input$asmus2_y], na.rm = TRUE), nice = nice_Numbers),
        value = c(roundDownNice(min(results()$sge[, input$asmus2_y], na.rm = TRUE), nice = nice_Numbers),roundUpNice(max(results()$sge[, input$asmus2_y], na.rm = TRUE), nice = nice_Numbers)),
        step = roundUpNice((max(results()$sge[, input$asmus2_y], na.rm = TRUE) - min(results()$sge[, input$asmus2_y], na.rm = TRUE))/100, nice = nice_Numbers)
      )
     } else {
      rg.z <- log(
        range(
          roundDownNice(
            min(results()$sge[, input$asmus2_y], na.rm = TRUE), nice = nice_Numbers
          ),
          roundUpNice(
            max(results()$sge[, input$asmus2_y], na.rm = TRUE), nice = nice_Numbers
          )
        )
      )
      choices <- unique(unlist(lapply(exp(seq(rg.z[1], rg.z[2], length.out = 20)), function(x){signif(x, 2)})))
      shinyWidgets::sliderTextInput(
        inputId = ns("asmus2_slider"),
        label = "Log Y Range:",
        hide_min_max = TRUE,
        choices = choices,
        selected = c(choices[1],choices[length(choices)]),
        grid = TRUE
      )
     }
    }
    }
  })

  output$asmus2_slider <- shiny::renderUI({
    if (!is.null(results())) {
      if (any(startsWith(colnames(results()$sge),"FCID_complete_"))) {
        if (input$asmus2_plot_type == "lin") {
        shiny::sliderInput(
          inputId = ns("asmus2_slider"),
          label = "Y Range",
          min = roundDownNice(min(results()$sge[, input$asmus2_y], na.rm = TRUE), nice = nice_Numbers),
          max = roundUpNice(max(results()$sge[, input$asmus2_y], na.rm = TRUE), nice = nice_Numbers),
          value = c(roundDownNice(min(results()$sge[, input$asmus2_y], na.rm = TRUE), nice = nice_Numbers),roundUpNice(max(results()$sge[, input$asmus2_y], na.rm = TRUE), nice = nice_Numbers)),
          step = roundUpNice((max(results()$sge[, input$asmus2_y], na.rm = TRUE) - min(results()$sge[, input$asmus2_y], na.rm = TRUE))/100, nice = nice_Numbers)
        )
       } else {
        rg.z <- log(
          range(
            roundDownNice(
              min(results()$sge[, input$asmus2_y], na.rm = TRUE), nice = nice_Numbers
            ),
            roundUpNice(
              max(results()$sge[, input$asmus2_y], na.rm = TRUE), nice = nice_Numbers
            )
          )
        )
        choices <- unique(unlist(lapply(exp(seq(rg.z[1], rg.z[2], length.out = 20)), function(x){signif(x, 2)})))
        shinyWidgets::sliderTextInput(
          inputId = ns("asmus2_slider"),
          label = "Log Y Range:",
          hide_min_max = TRUE,
          choices = choices,
          selected = c(choices[1],choices[length(choices)]),
          grid = TRUE
        )
       }
      }
    }
  })

   output$asmus2_total_numbers <- shiny::renderUI({
    if(!is.null(results())) {
      if (any(startsWith(colnames(results()$sge),"FCID_complete_"))) {
        shiny::HTML(
          paste0(
            "<p>
              Number of total subgroups:
              <b style='font-size: 180%;color: '", font_color(ColorBGplot()) ,"'>", sum(length(results()$sge$SGID)),
              "</b> <br>
              Number of subgroups within complete or pseudo complete factorial context:
              <b style='font-size: 180%;color: #5CA9FF'>",
              sum(results()$sge[[paste0("FCID_complete_",input$asmus2_y)]] != "Not complete") + sum(results()$sge[[paste0("FCID_pseudo_",input$asmus2_y)]] != "No Pseudo"),
              "</b> <br>
              Number of subgroups within complete factorial context:
              <b style='font-size: 180%;color: #0350E0'>",
              sum(results()$sge[[paste0("FCID_complete_",input$asmus2_y)]] != "Not complete"),"</b>
            </p>"
          )
        )
      }
    }
  })

   ####... graph_asmus2 ####
  output$graph_asmus2 <- shiny::renderPlot({
    if (!is.na(input$fuzzy_multiplicity_value)) {
      if (input$fuzzy_multiplicity_value < 0.0001) {
        fuzzy_multiplicity_value <- 0.000000001
      } else if (input$fuzzy_multiplicity_value > 1) {
        fuzzy_multiplicity_value <- 1
      } else {
        fuzzy_multiplicity_value <- input$fuzzy_multiplicity_value
      }
    } else {
      fuzzy_multiplicity_value <- input$fuzzy_multiplicity_value
    }

    if(!is.null(results()) & !is.null(input$asmus2_slider)) {
      if (any(startsWith(colnames(results()$sge),"FCID_complete_"))) {
      graphics::par(
        oma = c(0, 0, 0, 0),
        mar = c(0, 3, 0, 0),
        bg = ColorBGplot()
      )
      plot_point <- plot_points_asmus2()


      plot(
        NULL,
        NULL,
        ylim = input$asmus2_slider,
        xlim= c(0,max(results()$sge[,c("N.of.subjects")],na.rm=TRUE)),
        xlab = "",
        ylab = "",
        log = ifelse(input$asmus2_plot_type == "log", "y", ""),
        cex.axis = 1.5,
        cex.lab = 1.5,
        type = "n",
        axes = FALSE
      )
      graphics::rect(
      xleft = graphics::grconvertX(0,'ndc','user') - ifelse(input$asmus2_plot_type == "lin", 1000, 0),
      xright = graphics::grconvertX(1,'ndc','user') + ifelse(input$asmus2_plot_type == "lin", 1000, 0),
      ybottom = graphics::grconvertY(0,'ndc','user') - ifelse(input$asmus2_plot_type == "lin", 1000, 0),
      ytop = graphics::grconvertY(1,'ndc','user') + ifelse(input$asmus2_plot_type == "lin", 1000, 0),
      border = NA,
      col = ColorBGplot(),
      xpd = TRUE
    )

    if (ifelse(shiny::isolate(input$asmus2_plot_type) == "log", "y", "") == "y") {
      miniy <- 10^graphics::par("usr")[3]
      maxiy <- 10^graphics::par("usr")[4]
      lowy <- 10^(graphics::par("usr")[3] + (graphics::par("usr")[4] - graphics::par("usr")[3])/40)
      lowyp <- 10^(graphics::par("usr")[3] + (graphics::par("usr")[4] - graphics::par("usr")[3])/20)
      minplustinyy <- 10^(graphics::par("usr")[3] + (graphics::par("usr")[4] -
                                                       graphics::par("usr")[3])/1400)
    } else {
      miniy <- graphics::par("usr")[3]
      maxiy <- graphics::par("usr")[4]
      lowy <- miniy + (maxiy - miniy)/40
      lowyp <- miniy + (maxiy - miniy)/20
      minplustinyy <- miniy + (maxiy - miniy)/1400
    }

    minix <- roundDownNice(graphics::par("usr")[1], nice = nice_Numbers)
    maxix <- roundUpNice(graphics::par("usr")[2], nice = nice_Numbers)
    nr <- 7
    stepx <- roundUpNice((maxix - minix)/(nr +  1))

    if (minix < stepx)
      minix <- 0
    stripesx <- 0:(nr + 1)
    stripesx <- lapply(stripesx, function(x) x * stepx)
    stripesx <- lapply(stripesx, function(x) x + minix)
    stripesxp <- lapply(stripesx, function(x) paste(floor(x/results()$results_total[,c(input$x)] * 100), "%"))
    for (i in seq(1, nr, 2)) graphics::rect(stripesx[i],miniy, stripesx[i + 1], maxiy, col = different_hues(ColorBGplot()), border = NA)

    graphics::text(stripesx, lowy, stripesx, cex = 1.5,col = font_color(ColorBGplot()))

    graphics::box(col = font_color(ColorBGplot()))
    graphics::axis(
      2,
      col = font_color(ColorBGplot()),
      col.ticks = font_color(ColorBGplot()),
      col.axis = font_color(ColorBGplot()),
      cex.axis = 1
    )

    if (input$asmus_use_ref) {
      graphics::points(
        results()$sge[, "N.of.subjects"],
        results()$sge[, input$asmus2_y],
        pch = 19,
        cex = 1,
        col = paste0(ColorPoints(),"20")
      )
    }

    # draw background for remarkability if values are entered
    if (
      !is.na(input$asmus2_remarkability_1) &
      !is.na(input$asmus2_remarkability_2)
    ) {

      graphics::polygon(
        x = c(
          -100,
          -100,
          max(results()$sge[,c("N.of.subjects")], na.rm = TRUE) ,
          max(results()$sge[,c("N.of.subjects")], na.rm = TRUE)
        ),
        y = c(
          input$asmus2_remarkability_1,
          input$asmus2_remarkability_2,
          input$asmus2_remarkability_2,
          input$asmus2_remarkability_1
        ),
        col = "#ff8a8a50",
        border = NA
      )
    }
    # draw background for reliability if values are entered
    if (
      !is.na(input$asmus2_reliability_1) &
      !is.na(input$asmus2_reliability_2)
    ) {

      graphics::polygon(
        x = c(
          input$asmus2_reliability_1,
          input$asmus2_reliability_2,
          input$asmus2_reliability_2,
          input$asmus2_reliability_1
        ),
        y = c(
          -100,
          -100,
          max(results()$sge[, c("N.of.subjects")], na.rm = TRUE) ,
          max(results()$sge[, c("N.of.subjects")], na.rm = TRUE)
        ),
        col = "#ff8a8a50",
        border = NA
      )
    }

    if (!is.na(input$asmus2_reliability_1) &
       !is.na(input$asmus2_reliability_2) &
       !is.na(input$asmus2_remarkability_1) &
       !is.na(input$asmus2_remarkability_2) &
       !is.na(input$fuzzy_multiplicity_value)
    ) {
      lower_y_value <- roundDownNice(min(results()$sge[, input$asmus2_y], na.rm = TRUE), nice = nice_Numbers)
      upper_y_value <- roundUpNice(max(results()$sge[, input$asmus2_y], na.rm = TRUE), nice = nice_Numbers)

      #(1) fuzzy_multiplicity_value
      graphics::polygon(
        x = c(input$asmus2_reliability_1 + ((input$asmus2_reliability_2-input$asmus2_reliability_1) * fuzzy_multiplicity_value),
          input$asmus2_reliability_1 + ((input$asmus2_reliability_2-input$asmus2_reliability_1) * fuzzy_multiplicity_value),
          input$asmus2_reliability_2,
          input$asmus2_reliability_2),
        c(ifelse(input$asmus_direction, input$asmus2_remarkability_2,  lower_y_value),
          ifelse(input$asmus_direction, upper_y_value, input$asmus2_remarkability_1),
          ifelse(input$asmus_direction, upper_y_value, input$asmus2_remarkability_1),
          ifelse(input$asmus_direction, input$asmus2_remarkability_2, lower_y_value)
        ),
        col = "#80008080",
        border = NA
      )

      # (2) fuzzy_multiplicity_value
      graphics::polygon(
        x = c(input$asmus2_reliability_2, input$asmus2_reliability_2, max(results()$sge[,c("N.of.subjects")],na.rm=TRUE) , max(results()$sge[,c("N.of.subjects")],na.rm=TRUE)),
        y = c(
          ifelse(input$asmus_direction, input$asmus2_remarkability_1+((input$asmus2_remarkability_2-input$asmus2_remarkability_1)*fuzzy_multiplicity_value), lower_y_value),
          ifelse(input$asmus_direction, upper_y_value, input$asmus2_remarkability_1+((input$asmus2_remarkability_2-input$asmus2_remarkability_1)*(1-fuzzy_multiplicity_value))),
          ifelse(input$asmus_direction, upper_y_value, input$asmus2_remarkability_1+((input$asmus2_remarkability_2-input$asmus2_remarkability_1)*(1-fuzzy_multiplicity_value))),
          ifelse(input$asmus_direction, input$asmus2_remarkability_1+((input$asmus2_remarkability_2-input$asmus2_remarkability_1)*fuzzy_multiplicity_value), lower_y_value)
        ),
        col = "#80008080",
        border = NA
      )
      # (3) fuzzy_multiplicity_value
      tmp_x <- seq((input$asmus2_reliability_1+((input$asmus2_reliability_2-input$asmus2_reliability_1)*fuzzy_multiplicity_value)), input$asmus2_reliability_2, length = 100)
      tmp_y <- calc_y(
        x = seq((input$asmus2_reliability_1+((input$asmus2_reliability_2-input$asmus2_reliability_1)*fuzzy_multiplicity_value)), input$asmus2_reliability_2, length = 100) ,
        a_rem1 = input$asmus2_remarkability_1,
        a_rem2 = input$asmus2_remarkability_2,
        a_rel1 = input$asmus2_reliability_1,
        a_rel2 = input$asmus2_reliability_2,
        multiplicity_val = fuzzy_multiplicity_value,
        direction_up = input$asmus_direction
      )

      graphics::polygon(
        x = c(tmp_x,input$asmus2_reliability_2),
        y = c(tmp_y,ifelse(input$asmus_direction, input$asmus2_remarkability_2, input$asmus2_remarkability_1)),
        col = "#80008080",
        border = NA
      )
    }

    if (!is.null(asmus2_rem_and_rel_subgroups())) {
      graphics::points(
        plot_point$x,
        plot_point$y,
        pch = 19,
        cex = 1,
        col =  paste0(plot_point$col,20)
      )

    } else {
      graphics::points(
        plot_point$x,
        plot_point$y,
        pch = 19,
        cex = 1,
        col = paste0(plot_point$col,99)
      )
    }

    graphics::abline(h = input$asmus2_remarkability_1, lty = 2, lwd = 1, col = "#ff8a8a99")
    graphics::abline(h = input$asmus2_remarkability_2, lty = 2, lwd = 1, col = "#ff8a8a99")
    graphics::abline(v = input$asmus2_reliability_1, lty = 2, lwd = 1, col = "#ff8a8a99")
    graphics::abline(v = input$asmus2_reliability_2, lty = 2, lwd = 1, col = "#ff8a8a99")

    if (!is.null(asmus2_rem_and_rel_subgroups())) {
      graphics::points(
        plot_point[plot_point$ID %in% asmus2_rem_and_rel_subgroups(),]$x,
        plot_point[plot_point$ID %in% asmus2_rem_and_rel_subgroups(),]$y,
        pch = 19,
        cex = 1,
        col = "#ff8a8a99"
      )
    }
    graphics::abline(h = results()$results_total[, c(input$asmus2_y)], lwd = 3, col = ColorReference)

    graphics::text(
      x = graphics::grconvertX(0.97, from = 'nfc', to = 'user'),
      y = results()$results_total[, c(input$asmus2_y)] + diff(input$asmus2_slider)/50,
      paste0(shiny::isolate(results()$results_total[, c(input$asmus2_y)])),
      col = ColorReference
    )
    }
    }
  })

  output$asmus_hover_info1 <- shiny::renderUI({
    if(!is.null(results())) {
    if (any(startsWith(colnames(results()$sge),"FCID_complete_"))) {
    shiny::req(input$graph_asmus_hover1)
    input$graph_asmus_hover1
    plot_point <- plot_points_asmus2()
    all_points <- plot_point

    colored_points <- all_points[!startsWith(all_points$col, ColorPoints()),  ]
    hover <- input$graph_asmus_hover1
    hover$mapping <- list(xintercept = "xintercept", x = "x", y = "y")

    point <- nearPoints(colored_points, hover)

    if (nrow(point) == 0) return(NULL)

    left_pct <- (hover$coords_img$x - hover$range$left) / (hover$range$right - hover$range$left)
    top_pct <- (hover$domain$top - ifelse(input$plot_type == "lin", hover$y, log10(hover$y))) / (hover$domain$top - hover$domain$bottom)

    left_px <- ifelse(left_pct <= 0.75,
                      20 + hover$range$left + left_pct * (hover$range$right - hover$range$left) / hover$img_css_ratio$x,
                      - 175 + hover$range$left + left_pct * (hover$range$right - hover$range$left) / hover$img_css_ratio$x)

    top_px <- ifelse(top_pct <= 0.5,
                     20 + hover$range$top + top_pct * (hover$range$bottom - hover$range$top),
                     - 115 + hover$range$top + top_pct * (hover$range$bottom - hover$range$top))
    style <- paste0("position:absolute; z-index:100;background-color: rgba(",grDevices::col2rgb(point$col)[1],",",grDevices::col2rgb(point$col)[2],",",grDevices::col2rgb(point$col)[3],",0.85); ",
                    "left:", left_px, "px; top:", top_px, "px; border: 0px;")
    point <- point[1,]

    tmp1 <- colnames(results()$sge[which(results()$sge$SGID == point$ID), results()$factors])[which(results()$sge[which(results()$sge$SGID == point$ID), results()$factors] != "Not used")]

    tmp2 <- results()$sge %>%
      dplyr::filter(SGID %in% point$ID) %>%
      dplyr::select(colnames(results()$sge[which(results()$sge$SGID == point$ID), results()$factors])[which(results()$sge[which(results()$sge$SGID == point$ID), results()$factors] != "Not used")])

    tmp2 <- data.frame(lapply(tmp2, as.character), stringsAsFactors = FALSE)

    shiny::wellPanel(
      style = style,
      shiny::p(
        shiny::HTML(
          ifelse(length(tmp1)>0,
          paste0(
            "<b style = 'color: ",
            font_color(point$col),
            "'> ",
            "N.of.subjects",
            ": ",
            point$x,
            "</br>",
            "<b style = 'color: ",
            font_color(point$col),
            "'> ",
            input$asmus2_y,
            ": ",
            point$y,
            "</br>",
            "<b style = 'color: ",
            font_color(point$col),
            "'> Factors(",
            length(tmp1),
            "): ",
            paste(
              paste0(
                tmp1,": ", tmp2
              ), collapse = ", "
            ),
            "</br>"
          ),
          paste0(
            "<b style = 'color: ",
            font_color(point$col),
            "'> ",
            "N.of.subjects",
            ": ",
            point$x,
            "</br>",
            "<b style = 'color: ",
            font_color(point$col),
            "'> ",
            input$asmus2_y,
            ": ",
            point$y
          )
          )
        )
      )
    )
    }
    }
  })


  shinyjs::disable(id = "asmus2_continue")

  # Run asmus only if calculation for factorial factors were performed

  #### Page 1 ####
  output$asmus2_color_dropdown <- shiny::renderUI({
    shiny::tags$head(
      shiny::tags$style(
        shiny::HTML(
          paste0(
            "#dropdown-menu-", session$ns("MyDropDown2")," {
            background-color: ",
            ColorBGplot(),
            " !important;}
            "
          )
        )
      )
    )
  })


  #### REACTIVES ####
  ####... plot_points_asmus2 ####
  # subgroups within factorial contexts used in asmus
  plot_points_asmus2 <- shiny::reactive({
    if (!is.null(results())) {
      if (any(startsWith(colnames(results()$sge),"FCID_complete_"))) {
        if (input$asmus2_y %in% colnames(results()$sge)) {
          dat_frame <- data.frame(
            x = results()$sge[, c("N.of.subjects")],
            y = results()$sge[, c(input$asmus2_y)],
            ID = results()$sge[, "SGID"],
            col = ifelse(results()$sge[[paste0("FCID_complete_",input$asmus2_y)]] != "Not complete", "#0350E0","#5CA9FF")
          )

          # depending on in app decision if only complete or complete and pseudo complete factorial contexts should be used
          if (!input$asmus_include_pseudo) {
            dat_frame <- dat_frame[(dat_frame$ID %in% results()$sge$SGID[results()$sge[[paste0("FCID_complete_",input$asmus2_y)]] != "Not complete"]),]
          } else {
            dat_frame <- dat_frame[(dat_frame$ID %in% c(results()$sge$SGID[results()$sge[[paste0("FCID_complete_",input$asmus2_y)]] != "Not complete"], results()$sge$SGID[results()$sge[[paste0("FCID_pseudo_",input$asmus2_y)]] != "No Pseudo"])),]
          }
          dat_frame
        }
      }
    }
  })

  asmus2_rem <- shiny::reactive({
    if (!is.null(results())) {
      if (any(startsWith(colnames(results()$sge),"FCID_complete_"))) {
        plot_point <- plot_points_asmus2()
        if (!is.na(input$asmus2_remarkability_1) & !is.na(input$asmus2_remarkability_2)) {
          rem <- unlist(lapply(plot_point[, "y"], function(x) zf(x,input$asmus2_remarkability_1,input$asmus2_remarkability_2, direction_up = input$asmus_direction)))
        } else {
          rem <- NULL
        }
        rem
      }
    } else {
      NULL
    }
  })

  asmus2_rel <- shiny::reactive({
    if(!is.null(results())) {
      if (any(startsWith(colnames(results()$sge),"FCID_complete_"))) {
      plot_point <- plot_points_asmus2()

      if (!is.na(input$asmus2_reliability_1) & !is.na(input$asmus2_reliability_2)) {
        b <- Sys.time()
        rel <- unlist(lapply(plot_point[, "x"], function(x) zf(x, input$asmus2_reliability_1, input$asmus2_reliability_2)))
      } else {
        rel <- NULL
      }
      rel
      }
    } else {NULL}
  })

  asmus2_rem_and_rel_subgroups <- shiny::reactive({
    if (!is.null(results())) {
      if (any(startsWith(colnames(results()$sge),"FCID_complete_"))) {
      rem <- asmus2_rem()
      rel <- asmus2_rel()
      if (!is.na(input$fuzzy_multiplicity_value)) {
        if (input$fuzzy_multiplicity_value < 0.0001) {
          fuzzy_multiplicity_value <- 0.000000001
        } else if (input$fuzzy_multiplicity_value > 1) {
          fuzzy_multiplicity_value <- 1
        } else {
          fuzzy_multiplicity_value <- input$fuzzy_multiplicity_value
        }
      } else {
        fuzzy_multiplicity_value <- input$fuzzy_multiplicity_value
      }
      # (4) fuzzy_multiplicity_value
      plot_point <- plot_points_asmus2()
      if (
        !is.null(rem) &
        !is.null(rel) &
       !is.na(input$fuzzy_multiplicity_value)
      ) {
        d <- Sys.time()
        rr <- ifelse(rem * rel < fuzzy_multiplicity_value | is.na(rem * rel), FALSE, TRUE)
      } else {
        rr <- NULL
      }
      if (!is.null(rr)) {
        if (any(rr)) {
          res <- plot_point[rr,]$ID
        } else {
          res <- NULL
        }
      } else {
        res <- NULL
      }
      res
      }
    } else {
    NULL
    }
  })


  # button colors:
  output$continue_cont1 <- shiny::renderUI({
    list(
      shiny::tags$head(
        tags$style(HTML(paste0('#', session$ns("asmus2_continue"),'{color: #ffffff; background-color:#e3e3e3;}')))
      )
    )
  })
  #change color of the Create/Upload Plots Buttons
  shiny::observeEvent(c(results(),asmus2_rem_and_rel_subgroups()), {
    if(!is.null(results())) {
      if (any(startsWith(colnames(results()$sge),"FCID_complete_"))) {
        if (!is.null(asmus2_rem_and_rel_subgroups())) {
          output$continue_cont1 <- shiny::renderUI({
            list(
              shiny::tags$head(
                tags$style(HTML(paste0('#', session$ns("asmus2_continue"), '{color: #ffffff; background-color:#1eba1e;}')))
              )
            )
          })

          shinyjs::enable(id = "asmus2_continue")

        }
      }
    }
  })

  output$asmus2_rel_and_rem_number_text <- shiny::renderUI({
    if (!is.null(asmus2_rem_and_rel_subgroups())) {
      shiny::HTML(
        paste0(
        "<p> Number of subgroups which are remarkable and reliable regarding your selection:
        <b style='font-size: 200%;color: #ff8a8a'>",
        length(asmus2_rem_and_rel_subgroups()),"</b>"
        )
      )
    }
  })


  shiny::observe({
    if (
      is.null(asmus2_rem_and_rel_subgroups()) ||
      is.null(input$asmus2_remarkability_1) || is.null(input$asmus2_remarkability_2) ||
      is.null(input$asmus2_reliability_1) || is.null(input$asmus2_reliability_2)|
      is.na(input$asmus2_remarkability_1) || is.na(input$asmus2_remarkability_2) |
      is.na(input$asmus2_reliability_1) || is.na(input$asmus2_reliability_2)
    ) {
      output$cont1 <- shiny::renderUI({
        list(
          shiny::tags$head(
            tags$style(HTML(paste0('#',session$ns("asmus2_calculate"),'{color: #ffffff; background-color:#e3e3e3;}')))
          )
        )
      })
      shinyjs::disable(id = "asmus2_continue")
    } else {
      output$continue_cont1 <- shiny::renderUI({
        list(
          shiny::tags$head(
            tags$style(HTML(paste0('#',session$ns("asmus2_continue"),'{color: #ffffff; background-color:#1eba1e;}')))
          )
        )
      })
      shinyjs::enable(id = "asmus2_continue")
    }
  })

  shiny::observeEvent(
    c(
      input$asmus2_remarkability_1, input$asmus2_remarkability_2,
      input$asmus2_reliability_1, input$asmus2_reliability_2,
      input$asmus2_y, input$asmus_include_pseudo, input$asmus_direction,
      input$fuzzy_multiplicity_value, input$asmus2_calculate
    ), {

    if (is.na(input$fuzzy_multiplicity_value)) {
      txt3 <- paste0("Warning: Multiplicity value is missing! Please select a value between 0 and 1!")
    } else {txt3 <- ""}
    output$cont1_text <- shiny::renderUI({
      HTML(paste0("<b style='color: #ff8a8a; border-color: #f78300'>", paste(txt3, sep ="<br>"),"</b>"))
    })

    if (!is.na(input$asmus2_remarkability_1) &
      !is.na(input$asmus2_remarkability_2) &
      !is.na(input$asmus2_reliability_1) &
      !is.na(input$asmus2_reliability_2)) {

      if (input$asmus2_remarkability_1 < input$asmus2_remarkability_2 &
        input$asmus2_reliability_1 < input$asmus2_reliability_2
      ) {
        output$cont2_text <- shiny::renderUI({
          HTML("")
        })
      } else {

        if (input$asmus2_remarkability_1 >= input$asmus2_remarkability_2) {
          txt1 <- paste0("Warning: The upper value for remarkability is less equal than the lower value! Please make another selection!")
        } else {txt1 <- ""}
        if (input$asmus2_reliability_1 >= input$asmus2_reliability_2) {
          txt2 <- paste0("Warning: The upper value for reliability is less equal than the lower value! Please make another selection!")
        } else {txt2 <- ""}

        output$cont2_text <- shiny::renderUI({
          HTML(paste0("<b style='color: #ff8a8a; border-color: #f78300'>", paste(txt1, txt2, sep ="<br>"),"</b>"))
        })
      }
    }
  },ignoreNULL = FALSE)

    output$asmus2_rel_and_rem_number_text <- shiny::renderUI({
      if (!is.null(asmus2_rem_and_rel_subgroups())) {
        shiny::HTML(
          paste0(
          "<p> Number of subgroups which are remarkable and reliable regarding your selection:
          <b style='font-size: 200%;color: #ff8a8a'>",
          length(asmus2_rem_and_rel_subgroups()),"</b>"
          )
        )
      }
    })

    asmus_rem_and_rel <- shiny::reactiveValues(val = NULL)
    asmus_rem_and_rel2 <- shiny::reactiveValues(val = NULL)

    shiny::observeEvent(input$asmus2_continue, {
      col_nam <- colnames(results()$sge[,which(colnames(results()$sge) %in% results()$factors)])
      tmp <- results()$sge[results()$sge$SGID %in% asmus2_rem_and_rel_subgroups(), ]
      tmp$'Factor Level' <- apply(tmp[,col_nam], 1, function(x) { paste0("Factors(", length(which(x!="Not used")),"): ", paste(paste0(col_nam[which(x!="Not used")], " = ", x[which(x!="Not used")]), collapse = ", ")) })
      tmp <- tmp %>%
        dplyr::select(SGID, 'Factor Level', input$asmus2_y)
      colnames(tmp)[3] <- paste(substr(input$asmus2_y,1,8),"...")
      asmus_rem_and_rel$val <- tmp
    })

    #### Page 2 ####
    output$helptext_graph2 <- shiny::renderUI({
      if(!is.null(results())) {
        if (any(startsWith(colnames(results()$sge),"FCID_complete_"))) {
          HTML("<p style ='color:white'> Please select a row in the left side table for further investigation of the reliable and remarkable subgroups.</p>")
        }
      }
    })

    shiny::observeEvent(c(asmus2_rem_and_rel_subgroups(),asmus_rem_and_rel$val, ColorBGplot()), {
      if (!is.null(asmus_rem_and_rel$val)) {
        df_rem_rel_ <- asmus_rem_and_rel$val

        shinyInput <- function(FUN, len, id, ...) {
          inputs <- character(len)
          for (i in seq_len(len)) {
            inputs[i] <- as.character(FUN(paste0(id, df_rem_rel_$SGID[i]), ...))
          }
          inputs
        }
        Memorize = shinyInput(
          actionButton,
          dim(df_rem_rel_)[1],
          'button_',
          label = "Memorize",
          onclick = 'Shiny.onInputChange(\"select_button\",  this.id)'
        )
        tmp <- DT::datatable(
          selection = 'single',
          data = cbind(Memorize,df_rem_rel_),
          extensions = 'Buttons',
          escape = FALSE,
          options = list(
            columnDefs = list(list(targets = 1, sortable = FALSE)),

            initComplete = DT::JS(
              "function(settings, json) {",
              paste0("$(this.api().table().header()).css({'background-color': '",
                     ColorBGplot(),
                     "', 'color': '",
                     font_color(different_hues(ColorBGplot())),
                     "'});"
              ),"}"
            ),
            dom = 'Brtip',
           buttons = c('copy','csv','print','pageLength', I('colvis')),
            lengthMenu = list(c(6, 12, -1), c("6", "12", "All")),
            pageLength = 6,
            rowCallback = DT::JS(
            "function(row, data) {
            \n
            // Bold cells for those >= 5 in the first column\n
            if (parseFloat(data[1]) >= 15.0)\n
            $(\"td:eq(1)\", row).css(\"font-weight\", \"bold\");\n
            }"
          )
          ),
          class = 'cell-border stripe',
          rownames = FALSE,
          caption = 'Table of reliable and remarkable subgroups.',
          filter = 'top'
        )
        col.tabFont <- font_color(different_hues(ColorBGplot()))
        tmp <- DT::formatStyle(
          table = tmp,
          columns = 1:(ncol(df_rem_rel_) + 1),
          target = "cell",
          color = col.tabFont,
          backgroundColor = different_hues(ColorBGplot()),
          border = paste0('.5px solid ', ColorBGplot())
        )
        output$asmus2_table_rem_and_rel <- DT::renderDataTable(tmp)
      }
    })

    shiny::observeEvent(c(asmus_rem_and_rel2$val, input$deletePressed), {
      if (!is.null(asmus_rem_and_rel2$val)) {
        df_rem_rel_ <- asmus_rem_and_rel2$val


        tmp <- DT::datatable(
          selection = 'single',
          data = df_rem_rel_,
          extensions = 'Buttons',
          escape = FALSE,
          options = list(
            columnDefs = list(list(targets = 1, sortable = FALSE)),

            initComplete = DT::JS(
              "function(settings, json) {",
              paste0("$(this.api().table().header()).css({'background-color': '",
                     ColorBGplot(),
                     "', 'color': '",
                     font_color(different_hues(ColorBGplot())),
                     "'});"
              ),"}"
            ),
            dom = 'Brtip',
           buttons = c('copy','csv','print','pageLength', I('colvis')),
            lengthMenu = list(c(6, 12, -1), c("6", "12", "All")),
            pageLength = 6,
            rowCallback = DT::JS(
            "function(row, data) {
            \n
            // Bold cells for those >= 5 in the first column\n
            if (parseFloat(data[1]) >= 15.0)\n
            $(\"td:eq(1)\", row).css(\"font-weight\", \"bold\");\n
            }"
          )
          ),
          class = 'cell-border stripe',
          rownames = FALSE,
          caption = 'Table of reliable and remarkable subgroups.',
          filter = 'top'
        )
        col.tabFont <- font_color(different_hues(ColorBGplot()))
        tmp <- DT::formatStyle(
          table = tmp,
          columns = 1:(ncol(df_rem_rel_) + 1),
          target = "cell",
          color = col.tabFont,
          backgroundColor = different_hues(ColorBGplot()),
          border = paste0('.5px solid ', ColorBGplot())
        )
        output$asmus2_table_rem_and_rel2 <- DT::renderDataTable(tmp)
      }
    })

    shiny::observeEvent(input$asmus2_table_rem_and_rel_rows_selected, {
    if (!is.null(input$asmus2_table_rem_and_rel_rows_selected)) {

      tmp <- results()$sge[results()$sge$SGID %in% asmus2_rem_and_rel_subgroups(), ]

      tmp2 <- tmp[tmp$SGID == asmus_rem_and_rel$val$SGID[input$asmus2_table_rem_and_rel_rows_selected],]
      fact_cont <- results()$sge[results()$sge$FCID_all == tmp2$FCID_all,]

      # if complete :
      if (all(fact_cont[[paste0("FCID_incomplete_",input$asmus2_y)]] == "Complete")) {

      } else {
        # use only the pseudo factorial context
        fact_cont <- fact_cont[fact_cont[[paste0("FCID_pseudo_",input$asmus2_y)]] != "No Pseudo",]
      }

      col_names <- colnames(tmp2[colnames(tmp2) %in% results()$factors][which(tmp2[colnames(tmp2) %in% results()$factors] != "Not used")])


      fac_1 <- col_names[1]
      if (is.na(col_names[2])) {
        fac_2 <- NULL
      } else {
        fac_2 <- col_names[2]
      }
      if (is.na(col_names[3])) {
        fac_3 <- NULL
      } else {
        fac_3 <- col_names[3]
      }

    output$asmus2_interaction <- shiny::renderPlot({

      interaction_plot2(
        df_data = fact_cont,
        fac1 = fac_1,
        fac2 = fac_2,
        fac3 = fac_3,
        response = input$asmus2_y,
        bg.col = ColorBGplot(),
        bg.col2 = different_hues(ColorBGplot()),
        font.col = font_color(ColorBGplot()),
        box.col = font_color(ColorBGplot())
      )
    })


    output$graph2_asmus2 <- shiny::renderPlot({

      if (!is.na(input$fuzzy_multiplicity_value)) {
        if (input$fuzzy_multiplicity_value < 0.0001) {
          fuzzy_multiplicity_value <- 0.000000001
        } else if (input$fuzzy_multiplicity_value > 1) {
          fuzzy_multiplicity_value <- 1
        } else {
          fuzzy_multiplicity_value <- input$fuzzy_multiplicity_value
        }
      } else {
        fuzzy_multiplicity_value <- input$fuzzy_multiplicity_value
      }

      if(!is.null(results())) {

        if (any(startsWith(colnames(results()$sge),"FCID_complete_"))) {

        graphics::par(
        oma = c(0, 0, 0, 0),
        mar = c(0, 3, 0, 0),
        bg = ColorBGplot()
      )
      plot_point <- plot_points_asmus2()

      plot(
        NULL,
        NULL,
        xlab = "",
        ylab = "",
        ylim = input$asmus2_slider,
        xlim=c(0,max(results()$sge[,c("N.of.subjects")],na.rm=TRUE)),
         log = ifelse(input$asmus2_plot_type == "log", "y", ""),
        cex.axis = 1.5,
        cex.lab = 1.5,
        type = "n",
        axes = FALSE
      )
      graphics::rect(
      xleft = graphics::grconvertX(0,'ndc','user') - ifelse(input$asmus2_plot_type == "lin", 1000, 0),
      xright = graphics::grconvertX(1,'ndc','user') + ifelse(input$asmus2_plot_type == "lin", 1000, 0),
      ybottom = graphics::grconvertY(0,'ndc','user') - ifelse(input$asmus2_plot_type == "lin", 1000, 0),
      ytop = graphics::grconvertY(1,'ndc','user') + ifelse(input$asmus2_plot_type == "lin", 1000, 0),
      border = NA,
      col = ColorBGplot(),
      xpd = TRUE
    )

    if (ifelse(shiny::isolate(input$asmus2_plot_type) == "log", "y", "") == "y") {
      miniy <- 10^graphics::par("usr")[3]
      maxiy <- 10^graphics::par("usr")[4]
      lowy <- 10^(graphics::par("usr")[3] + (graphics::par("usr")[4] - graphics::par("usr")[3])/40)
      lowyp <- 10^(graphics::par("usr")[3] + (graphics::par("usr")[4] - graphics::par("usr")[3])/20)
      minplustinyy <- 10^(graphics::par("usr")[3] + (graphics::par("usr")[4] -
                                                       graphics::par("usr")[3])/1400)
    } else {
      miniy <- graphics::par("usr")[3]
      maxiy <- graphics::par("usr")[4]
      lowy <- miniy + (maxiy - miniy)/40
      lowyp <- miniy + (maxiy - miniy)/20
      minplustinyy <- miniy + (maxiy - miniy)/1400
    }
      minix <- roundDownNice(graphics::par("usr")[1], nice = nice_Numbers)
      maxix <- roundUpNice(graphics::par("usr")[2], nice = nice_Numbers)

      nr <- 7
      stepx <- roundUpNice((maxix - minix)/(nr +  1))
      if (minix < stepx)
        minix <- 0
      stripesx <- 0:(nr + 1)
      stripesx <- lapply(stripesx, function(x) x * stepx)
      stripesx <- lapply(stripesx, function(x) x + minix)

      for (i in seq(1, nr, 2)) graphics::rect(stripesx[i],miniy, stripesx[i + 1], maxiy, col = different_hues(ColorBGplot()), border = NA)

      graphics::text(stripesx, lowy, stripesx, cex = 1.2, col = font_color(ColorBGplot()))

      graphics::box(col = font_color(ColorBGplot()))

      graphics::axis(
        2,
        col = font_color(ColorBGplot()),
        col.ticks = font_color(ColorBGplot()),
        col.axis = font_color(ColorBGplot()),
        cex.axis = 1
      )

      if (input$asmus_use_ref) {
        graphics::points(
          results()$sge[, "N.of.subjects"],
          results()$sge[, input$asmus2_y],
          pch = 19,
          cex = 1,
          col = "#ffffff20"
        )
      }

      # draw background for remarkability if values are entered
      if (
        !is.na(input$asmus2_remarkability_1) &
        !is.na(input$asmus2_remarkability_2)
      ) {

        graphics::polygon(
          x = c(
            -100,
            -100,
            max(results()$sge[,c("N.of.subjects")], na.rm = TRUE) ,
            max(results()$sge[,c("N.of.subjects")], na.rm = TRUE)
          ),
          y = c(
            input$asmus2_remarkability_1,
            input$asmus2_remarkability_2,
            input$asmus2_remarkability_2,
            input$asmus2_remarkability_1
          ),
          col = "#ff8a8a50",
          border = NA
        )
      }
      # draw background for reliability if values are entered
      if (
        !is.na(input$asmus2_reliability_1) &
        !is.na(input$asmus2_reliability_2)
      ) {

        graphics::polygon(
          x = c(
            input$asmus2_reliability_1,
            input$asmus2_reliability_2,
            input$asmus2_reliability_2,
            input$asmus2_reliability_1
          ),
          y = c(
            -100,
            -100,
            max(results()$sge[,c("N.of.subjects")], na.rm = TRUE) ,
            max(results()$sge[,c("N.of.subjects")], na.rm = TRUE)
          ),
          col = "#ff8a8a50",
          border = NA
        )
      }


      if(!is.na(input$asmus2_reliability_1) &
         !is.na(input$asmus2_reliability_2) &
         !is.na(input$asmus2_remarkability_1) &
         !is.na(input$asmus2_remarkability_2) &
        !is.na(input$fuzzy_multiplicity_value)
      ) {


        calc_y = function(
          x,
          a_rem1,
          a_rem2,
          a_rel1,
          a_rel2,
          multiplicity_val = 0.25,
          direction_up = TRUE
        ) {
          if (direction_up) {
            return(
              (multiplicity_val * (((a_rel2*a_rem2) - (a_rel2*a_rem1) - (a_rel1*a_rem2) + (a_rel1*a_rem1))) - (a_rel1*a_rem1) + (a_rem1*x)) / (x-a_rel1)
            )
          } else {
            return(
              (multiplicity_val * (((a_rel2*a_rem2) - (a_rel2*a_rem1) - (a_rel1*a_rem2) + (a_rel1*a_rem1))) + (a_rel1*a_rem2) - (a_rem2*x)) / (-x + a_rel1)
            )
          }
        }
        lower_y_value <- roundDownNice(min(results()$sge[, input$asmus2_y], na.rm = TRUE), nice = nice_Numbers)
        upper_y_value <- roundUpNice(max(results()$sge[, input$asmus2_y], na.rm = TRUE), nice = nice_Numbers)

        # (5) fuzzy_multiplicity_value
        graphics::polygon(
          x = c(input$asmus2_reliability_1 + ((input$asmus2_reliability_2-input$asmus2_reliability_1) * fuzzy_multiplicity_value),
            input$asmus2_reliability_1 + ((input$asmus2_reliability_2-input$asmus2_reliability_1) * fuzzy_multiplicity_value),
            input$asmus2_reliability_2,
            input$asmus2_reliability_2),
          y = c(ifelse(input$asmus_direction, input$asmus2_remarkability_2,  lower_y_value),
            ifelse(input$asmus_direction, upper_y_value, input$asmus2_remarkability_1),
            ifelse(input$asmus_direction, upper_y_value, input$asmus2_remarkability_1),
            ifelse(input$asmus_direction, input$asmus2_remarkability_2, lower_y_value)
          ),
          col = "#80008080",
          border = NA
        )

        # (6) fuzzy_multiplicity_value
        graphics::polygon(
          x = c(input$asmus2_reliability_2, input$asmus2_reliability_2, max(results()$sge[,c("N.of.subjects")],na.rm=TRUE) , max(results()$sge[,c("N.of.subjects")],na.rm=TRUE)),
          y = c(
            ifelse(input$asmus_direction, input$asmus2_remarkability_1+((input$asmus2_remarkability_2-input$asmus2_remarkability_1)*fuzzy_multiplicity_value), lower_y_value),
            ifelse(input$asmus_direction, upper_y_value, input$asmus2_remarkability_1+((input$asmus2_remarkability_2-input$asmus2_remarkability_1)*(1-fuzzy_multiplicity_value))),
            ifelse(input$asmus_direction, upper_y_value, input$asmus2_remarkability_1+((input$asmus2_remarkability_2-input$asmus2_remarkability_1)*(1-fuzzy_multiplicity_value))),
            ifelse(input$asmus_direction, input$asmus2_remarkability_1+((input$asmus2_remarkability_2-input$asmus2_remarkability_1)*fuzzy_multiplicity_value), lower_y_value)
          ),
          col = "#80008080",
          border = NA
        )

        # (7) fuzzy_multiplicity_value
        tmp_x <- seq((input$asmus2_reliability_1+((input$asmus2_reliability_2-input$asmus2_reliability_1)*fuzzy_multiplicity_value)), input$asmus2_reliability_2, length = 100)
        tmp_y <- calc_y(
          x = seq((input$asmus2_reliability_1+((input$asmus2_reliability_2-input$asmus2_reliability_1)*fuzzy_multiplicity_value)), input$asmus2_reliability_2, length = 100) ,
          a_rem1 = input$asmus2_remarkability_1,
          a_rem2 = input$asmus2_remarkability_2,
          a_rel1 = input$asmus2_reliability_1,
          a_rel2 = input$asmus2_reliability_2,
          multiplicity_val = fuzzy_multiplicity_value,
          direction_up = input$asmus_direction
        )

        graphics::polygon(
          x = c(tmp_x,input$asmus2_reliability_2),
          y = c(tmp_y,ifelse(input$asmus_direction, input$asmus2_remarkability_2, input$asmus2_remarkability_1)),
          col = "#80008080",
          border = NA
        )
      }

      graphics::points(
        fact_cont[,c("N.of.subjects")],
        fact_cont[,c(input$asmus2_y)],
        pch = 19,
        cex = 1,
        col = ifelse(all(fact_cont$FCID_complete == "Not complete"), "#5CA9FF", "#0350E0")
      )

      graphics::points(
        tmp2[,"N.of.subjects"],
        tmp2[,c(input$asmus2_y)],
        pch = 19,
        cex = 1,
        col = "#ff8a8a"
      )

      graphics::abline(h = results()$results_total[, c(input$asmus2_y)], lwd = 3, col = ColorReference)
      graphics::text(
        x = graphics::grconvertX(0.97, from = 'nfc', to = 'user'),
        y = results()$results_total[, c(input$asmus2_y)] + diff(input$asmus2_slider)/50,
        paste0(shiny::isolate(results()$results_total[, c(input$asmus2_y)])),
        col = ColorReference
      )

      df_parent <- parents(results(),tmp2$SGID)$Parents
      if (!is.null(df_parent)){
        graphics::points(
        df_parent[,"N.of.subjects"],
        df_parent[,c(input$asmus2_y)],
        pch = 19,
        cex = 1,
        col = "orange"
      )
      }
      if (!is.null(df_parent)) {
        df_parent <- df_parent %>%
          dplyr::select(SGID, nfactors, N.of.subjects, input$asmus2_y, results()$factors)
        df_parent <- df_parent[,apply(df_parent,2,function(x){any(x != "Not used")})]
        df_parent_ <- DT::datatable(
        selection = 'single',
        data = df_parent,
        extensions = 'Buttons',
        options = list(
          initComplete = DT::JS(
          "function(settings, json) {",
          paste0("$(this.api().table().header()).css({'background-color': '",
                 ColorBGplot(),
                 "', 'color': '",
                 font_color(different_hues(ColorBGplot())),
                 "'});"
          ),"}"
        ),
        dom = 'Brtip',
         pageLength = 6, lengthChange = FALSE
        ),
        class = 'cell-border stripe',
        rownames = FALSE,
        caption = 'Table of parent subgroups for selected subgroup.',
        filter = 'top'
      )

      col.tabFont <- font_color(different_hues(ColorBGplot()))
      df_parent_ <- DT::formatStyle(
        table = df_parent_,
        columns = 1:(ncol(tmp2) + 1),
        target = "cell",
        color = col.tabFont,
        backgroundColor = different_hues(ColorBGplot()),
        border = paste0('.5px solid ', ColorBGplot())
      )
        output$parents_asmus2 <- DT::renderDataTable(df_parent_)
      } else {
        output$parents_asmus2 <- DT::renderDataTable(NULL)
      }
      }
      }
    })


    tmp3 <- tmp2 %>%
      dplyr::select(SGID, nfactors, N.of.subjects, input$asmus2_y, results()$factors)
    tmp3 <- tmp3[,apply(tmp3, 2, function(x){any(x != "Not used")})]
    tmp_2 <- DT::datatable(
        selection = 'single',
        data = tmp3,
        extensions = 'Buttons',
        options = list(
          initComplete = DT::JS(
          "function(settings, json) {",
          paste0("$(this.api().table().header()).css({'background-color': '",
                 ColorBGplot(),
                 "', 'color': '",
                 font_color(different_hues(ColorBGplot())),
                 "'});"
          ),"}"
        ),
        dom = 'Brtip',
         pageLength = 6, lengthChange = FALSE
        ),
        class = 'cell-border stripe',
        rownames = FALSE,
        caption = 'Table of selected subgroup.',
        filter = 'top'
      )

      col.tabFont <- font_color(different_hues(ColorBGplot()))
      tmp_2 <- DT::formatStyle(
        table = tmp_2,
        columns = 1:(ncol(tmp2) + 1),
        target = "cell",
        color = col.tabFont,
        backgroundColor = different_hues(ColorBGplot()),
        border = paste0('.5px solid ', ColorBGplot())
      )
      output$selectedSG_asmus2 <- DT::renderDataTable(tmp_2)

       fact_cont2 <- fact_cont %>%
          dplyr::select(SGID, nfactors, N.of.subjects, input$asmus2_y, results()$factors)
        fact_cont2 <- fact_cont2[,apply(fact_cont2,2,function(x){any(x != "Not used")})]

      fact_cont_ <- DT::datatable(
        selection = 'single',
        data = fact_cont2,
        extensions = 'Buttons',
        options = list(
          initComplete = DT::JS(
          "function(settings, json) {",
          paste0("$(this.api().table().header()).css({'background-color': '",
                 ColorBGplot(),
                 "', 'color': '",
                 font_color(different_hues(ColorBGplot())),
                 "'});"
          ),"}"
        ),
        dom = 'Brtip',
         pageLength = 6, lengthChange = FALSE
        ),
        class = 'cell-border stripe',
        rownames = FALSE,
        caption = 'Table of factorial context of selected subgroup.',
        filter = 'top'
      )

      col.tabFont <- font_color(different_hues(ColorBGplot()))
      fact_cont_ <- DT::formatStyle(
        table = fact_cont_,
        columns = 1:(ncol(tmp2) + 1),
        target = "cell",
        color = col.tabFont,
        backgroundColor = different_hues(ColorBGplot()),
        border = paste0('.5px solid ', ColorBGplot())
      )


      output$factorial_asmus2 <- DT::renderDataTable(fact_cont_)
    } else {
      #draw empty plots
      output$asmus2_interaction <- shiny::renderPlot({
       plot(
        NULL,
        xlim = c(0, 1),
        ylim = c(0, 1),
        axes = FALSE,
        xlab = "",
        ylab = ""
      )

      graphics::rect(
        xleft = graphics::grconvertX(0, 'ndc', 'user') - 1000,
        xright = graphics::grconvertX(1, 'ndc', 'user') + 1000,
        ybottom = graphics::grconvertY(0,'ndc','user') - 1000,
        ytop = graphics::grconvertY(1, 'ndc', 'user') + 1000,
        border = NA,
        col = ColorBGplot(),
        xpd = TRUE
      )
      })
      output$graph2_asmus2 <- shiny::renderPlot({
      plot(
        NULL,
        xlim = c(0, 1),
        ylim = c(0, 1),
        axes = FALSE,
        xlab = "",
        ylab = ""
      )

      graphics::rect(
        xleft = graphics::grconvertX(0, 'ndc', 'user') - 1000,
        xright = graphics::grconvertX(1, 'ndc', 'user') + 1000,
        ybottom = graphics::grconvertY(0,'ndc','user') - 1000,
        ytop = graphics::grconvertY(1, 'ndc', 'user') + 1000,
        border = NA,
        col = ColorBGplot(),
        xpd = TRUE
      )
      })
      output$factorial_asmus2 <- DT::renderDataTable(NULL)
      output$selectedSG_asmus2 <- DT::renderDataTable(NULL)
      output$parents_asmus2 <- DT::renderDataTable(NULL)
    }
  }, ignoreNULL = FALSE)

  firstPageASMUS2 <- shiny::reactiveValues(val = TRUE)

  observeEvent(input$asmus2_continue, {
    firstPageASMUS2$val <- FALSE
  })

  observeEvent(input$asmus2_back, {
    firstPageASMUS2$val <- TRUE
  })

  output$PageASMUS <- shiny::reactive({
    firstPageASMUS2$val
  })

  outputOptions(output, "PageASMUS", suspendWhenHidden = FALSE)

    shiny::observeEvent(input$deletePressed, {

    # Handle delete pressed
    arar <- asmus_rem_and_rel$val
    rem_id <- as.numeric(input$deletePressed)

    tmp <- rbind(
      asmus_rem_and_rel2$val,
      arar %>%
        dplyr::filter(SGID == rem_id)
    )
    tmp <- tmp %>% dplyr::select(-delete)
    addCol <- unlist(lapply(tmp$SGID, fun2_))
    asmus_rem_and_rel2$val  <- cbind(add = addCol, tmp)


    arar_rem <- arar %>%
      dplyr::filter(SGID != rem_id)

    asmus_rem_and_rel$val <- arar_rem
  })
}
