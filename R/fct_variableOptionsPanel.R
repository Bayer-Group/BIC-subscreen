#' shiny widgets of variable option panel
#'
#' @keywords internal

variableOptionsPanel <- function(){
  shiny::tagList(
    shiny::div(style = "position:absolute;right:2em;",
      bsplus::bs_embed_tooltip(
        tag = bsplus::shiny_iconlink("question"),
        title = "Variable plotted on the y-axis.",
        placement = "top"
      )
    ),
    shiny::selectInput(
      inputId = "y",
      label = "Target variable",
      choices = NULL,
      selected = NULL
    ),
    shiny::div(style = "position:absolute;right:2em;",
      bsplus::bs_embed_tooltip(
        tag = bsplus::shiny_iconlink("question"),
        title = "Variable plotted on the x-axis.",
        placement = "top",
        expanded = TRUE
      )
    ),
    shiny::selectInput(
      inputId = "x",
      label = "Reference variable",
      choices = NULL,
      selected = NULL
    ),
    shiny::fluidRow(
      shiny::column(6,
        shiny::div(style = "position:absolute;right:2em;",
          bsplus::bs_embed_tooltip(
            tag = bsplus::shiny_iconlink("question"),
            title = "Select a filter variable. Subgroups containing this variable are displayed in green (default color).",
            placement = "top"
          )
        ),
        shiny::selectInput(
          inputId = "filter",
          label = "Subgroup Filter (1)",
          choices = c("no selection"),
          selected = c("no selection")
        ),
        bsplus::use_bs_popover(),
        bsplus::use_bs_tooltip()
      ),
      shiny::column(6,
        shiny::conditionalPanel(
          condition = "input.filter != 'no selection'",
          shiny::uiOutput("VarChosen"),
          selectize = FALSE
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(6,
        shiny::conditionalPanel(condition = "input.filter != 'no selection'",
          shiny::selectInput(
          inputId = "filter2",
          label = "Subgroup Filter (2)",
          choices = c("no selection"),
          selected = c("no selection")
          )
        )
      ),
      shiny::column(6,
        shiny::conditionalPanel(condition = "input.filter2 != 'no selection' & input.filter != 'no selection'",
          shiny::uiOutput("VarChosen2"),
          selectize = FALSE
        )
      )
    ),
    shiny::div(style = "position:absolute;right:2em;",
      bsplus::bs_embed_tooltip(
        tag = bsplus::shiny_iconlink("question"),
        title = "Subgroups containing selected number(s) of factor(s) are displayed in the plot.",
        placement = "top",
        expanded = TRUE
      )
    ),
    shiny::fluidRow(
      shiny::column(8,
        shiny::sliderInput(
          inputId = "key",
          label = "Subgroup level(s)",
          min = 1,
          max = 3,
          ticks = FALSE,
          value = c(1, 3),
          step = 1
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(8,
        shiny::uiOutput('YRange'),
      ),
      shiny::column(4,
        shiny::radioButtons(
          inputId = "plot_type",
          label = "",
          selected = "lin",
          inline = FALSE,
          choiceNames = list("lin", "log"),
          choiceValues = c("lin", "log")
        )
      )
    ),
      shiny::fluidRow(
        shiny::column(8,
          shiny::uiOutput("XRange")
        )
    )
  )
}
