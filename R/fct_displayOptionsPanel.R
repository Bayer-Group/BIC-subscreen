#' shiny widgets of display option panel
#'
#' @keywords internal

displayOptionsPanel <- function(
    custom_ref_line_at_start,
    custom_ref_line_value,
    favour_label_at_start,
    favour_direction
  ) {
  shiny::tagList(
    conditionalPanel(condition = "output.funnelenabled == true",
      shiny::checkboxInput(
        inputId = "add_funnel",
        label = "Draw reference funnel",
        value = FALSE
      )
    ),
    shiny::conditionalPanel(condition = "input.add_funnel == true",
      shiny::checkboxInput(
        inputId = "exclude_funnel",
        label = "Dots in funnel",
        value = FALSE
      )
    ),
    shiny::conditionalPanel(condition = "input.add_funnel == true",
      shiny::radioButtons(
        inputId = "alpha_funnel",
        label = "Select alpha",
        choices = c(0.1,0.01),
        selected = 0.1,
        inline = TRUE
      )
    ),
    # shiny::div(style = "position:absolute;right:2em;",
    #   bsplus::bs_embed_tooltip(
    #     tag = bsplus::shiny_iconlink("question"),
    #     title = "Maximum distance to the click dot (in pixel).",
    #     placement = "top",
    #     expanded = TRUE
    #   )
    # ),
    # shiny::sliderInput(
    #   inputId = "pickradius",
    #   label = "Choose distance to the click point",
    #   min = 1,
    #   max = 30,
    #   value = 5,
    #   step = 1 ,
    #   ticks = FALSE
    # ),


    shiny::div(style = "position:absolute;right:2em;",
      bsplus::bs_embed_tooltip(
        tag = bsplus::shiny_iconlink("question"),
        title = "Change the dot size.
        Combinable with dot style option.",
        placement = "top",
        expanded = TRUE
      )
    ),
    shiny::conditionalPanel(
      condition = "input.circlestyle == 'standard'",
      shiny::sliderInput(
        inputId = "pointsize",
        label = "Choose dot size" ,
        min = 0.1,
        max = 4,
        value = 2,
        step = 0.2
      )
    ),
    shiny::div(style = "position:absolute;right:2em;",
      bsplus::bs_embed_tooltip(
        tag = bsplus::shiny_iconlink("question"),
        title = "Use the Subgroup size as given size or display
        all dots with equal size.",
        placement = "top",
        expanded = TRUE
      )
    ),
    shiny::radioButtons(
      inputId = "circlestyle",
      label = "Point Style",
      choiceNames = list("Standard", "Subgroup size"),
      choiceValues = c("standard", "groupsize"),
      selected = "standard",
      inline = TRUE
    ),
    # shiny::radioButtons(
    #   inputId = "pch_value",
    #   label = "Plotting character",
    #   choiceNames = list("Circles"),
    #   choiceValues = c(19),
    #   selected = 19,
    #   inline = TRUE
    # ),
    # shiny::div(style = "position:absolute;right:2em;",
    #   bsplus::bs_embed_tooltip(
    #     tag = bsplus::shiny_iconlink("question"),
    #     title = "Adjust brightness of unmarked dots.",
    #     placement = "top",
    #     expanded = TRUE
    #   )
    # ),
    # shiny::sliderInput(
    #   inputId = "point_brightness",
    #   label = "Adjust dot brightness",
    #   min = 0.5,
    #   max = 1,
    #   value = 0.95,
    #   step = 0.05
    # ),
    bsplus::use_bs_popover(),
    bsplus::use_bs_tooltip(),
    shiny::checkboxInput(
      inputId = "xlabel",
      label = "Show percentages on x-axis",
      value = TRUE
    ),
    shiny::checkboxInput(
      inputId = "grid",
      label = "Display a grid",
      value = FALSE
    ),
    shiny::checkboxInput(
      inputId ="add_ref_line",
      label ="Show reference line",
      value = TRUE
    ),
    shiny::fluidRow(
      shiny::column(6,
        shiny::checkboxInput(
          inputId ="add_custom_ref_line",
          label ="Add custom reference line",
          value = custom_ref_line_at_start
        )
      ),
      column(6,
        shiny::conditionalPanel(condition = "input.add_custom_ref_line == true",
          shiny::numericInput(
            inputId = "custom_ref_line",
            label = "Value for custom reference line",
            value = custom_ref_line_value
          )
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(6,
        shiny::checkboxInput(
          inputId = "add_favour_arrows",
          label = "Add favour labels",
          value = favour_label_at_start
        )
      ),
      shiny::column(6,
        shiny::conditionalPanel(condition = "input.add_favour_arrows == true",
          shinyWidgets::prettyToggle(
            inputId = "favour_direction",
            label_on = "High values favour verum",
            icon_on = icon("arrow-up"),
            status_on = "success",
            value = favour_direction,
            status_off = "success",
            label_off = "Smaller values favour verum",
            icon_off = icon("arrow-down")
          )
        )
      )
    ),
    bsplus::use_bs_popover(),
    bsplus::use_bs_tooltip()
  )
}
