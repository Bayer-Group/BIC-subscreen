#' color UI Function
#'
#' @description A shiny Module for the color options in Subscreen.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_color_ui <- function(id) {
  ns <- NS(id)
  tagList(
    shiny::fluidRow(
      shiny::column(6,
        colourpicker::colourInput(
          inputId = ns("ColorTabClicked"),
          label = "Choose a colour for the selected subgroup",
          value = "#D30F4B",
          allowTransparent = TRUE
        )
      ),
      shiny::column(6,
        colourpicker::colourInput(
          inputId = ns("ColorSelected"),
          label = "Choose a colour for the filtered subgroup(s)",
          value  = "#89D329",
          allowTransparent = TRUE
        )
      )
    ),
    shiny::fluidRow(
       shiny::column(6,
        shiny::checkboxInput(
          inputId = ns("LabelTabClicked"),
          label = "Labels",
          value = FALSE
        )
      ),
      shiny::column(6,
      )
    ),
    shiny::fluidRow(
      shiny::column(6,
        colourpicker::colourInput(
          inputId = ns("ColorParents"),
          label = "Choose a colour for the parent subgroup(s)",
          value = "#ff6c00",
          allowTransparent = TRUE
        )
      ),
      shiny::column(6,
        colourpicker::colourInput(
          inputId = ns("ColorMemorized"),
          "Choose a colour for the memorized subgroups",
          value = "#57D48B"
        )
      ),
    ),
    shiny::fluidRow(
       shiny::column(6,
        shiny::checkboxInput(
          inputId = ns("LabelParents"),
          label = "Labels",
          value = FALSE
        )
      ),
      shiny::column(6,
        shiny::checkboxInput(
          inputId = ns("LabelMemorized"),
          label = "Labels",
          value = FALSE
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(6,
        colourpicker::colourInput(
          inputId = ns("ColorImportance"),
          label = "Choose a colour for the subgroup(s) with important variable(s) ",
          value = "#FA1BDC",
          allowTransparent = TRUE
        )
      ),
      shiny::column(6,
        colourpicker::colourInput(
          inputId = ns("ColorReference"),
          label = "Choose a colour for the reference line",
          value = "#0091DF",
          allowTransparent = TRUE
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(6,
        colourpicker::colourInput(
          inputId = ns("ColorFactCont"),
          label = "Choose a colour for the factorial context",
          value = "#0350E0",
          allowTransparent = TRUE
        )
      ),
      shiny::column(6,
        colourpicker::colourInput(
          inputId = ns("ColorPoints"),
          "Choose a colour for the points",
          value = "#FFFFFF5F",
          allowTransparent = TRUE
        )
      )
    ),
    shiny::fluidRow(
       shiny::column(6,
        shiny::checkboxInput(
          inputId = ns("LabelFactCont"),
          label = "Labels",
          value = FALSE
        )
      ),
      shiny::column(6,
        colourpicker::colourInput(
          inputId = ns("ColorCustomReference"),
          label = "Choose a colour for the custom reference line",
          value = "#00BCFFFF",
          allowTransparent = TRUE
        )
      )
    ),
    shiny::fluidRow(
      shiny::column(12,
        shiny::selectInput(
          inputId = ns('select_col'),
          label = "Select standard color theme:",
          choices = list('app version', 'print version'),#, 'bay version'),
          selected = 'app version'
        )
      )
    ),
    bsplus::use_bs_popover(),
    bsplus::use_bs_tooltip()
  )
}


#' color module server-side
#'
#' @param input,output,session Internal parameters for shiny.
#'
#' @noRd

mod_color_server <- function(input, output, session) {

   colthemeCol <- shiny::reactiveValues(
    col.bg = '#383838',
    font.col = '#ffffff',
    panel.col = '#6b6b6b',
    #ColorClicked = "#D30F4B",
    ColorSelected = "#89D329",
    ColorParents = "#ff6c00",
    ColorTabClicked = "#e2b007",
    ColorImportance = "#FA1BDC",
    ColorReference = "#0091DF60",
    ColorFactCont = "#0350E0",
    ColorBGplot = "#383838",
    ColorPoints = "#FFFFFF5F",
    ColorMemorized = "#57D48B",
    ColorCustomReference = "#00BCFFFF"
  )

  shiny::observeEvent(input$select_col, {
    if (input$select_col == 'app version') {
      colthemeCol$col.bg <- '#383838'
      colthemeCol$ColorBGplot <- "#383838"
      colthemeCol$ColorPoints <- "#FFFFFF5F"
    } else if (input$select_col == 'print version') {
      colthemeCol$col.bg <- '#ffffff'
      colthemeCol$ColorReference <- "#0091DF"
      colthemeCol$ColorBGplot <- "#ffffff"
      colthemeCol$ColorPoints <- "#000000"
    }
  })

  shiny::observeEvent(input$ColorBGplot,{
      colthemeCol$col.bg <- input$ColorBGplot
  })
  shiny::observeEvent(input$ColorSelected, {
      colthemeCol$ColorSelected <- input$ColorSelected
  })
  shiny::observeEvent(input$ColorFactCont, {
      colthemeCol$ColorFactCont <- input$ColorFactCont
  })
  shiny::observeEvent(input$ColorParents, {
      colthemeCol$ColorParents <- input$ColorParents
  })
  shiny::observeEvent(input$ColorTabClicked, {
      colthemeCol$ColorTabClicked <- input$ColorTabClicked
  })
  shiny::observeEvent(input$ColorImportance, {
      colthemeCol$ColorImportance <- input$ColorImportance
  })

  shiny::observeEvent(input$ColorReference, {
      colthemeCol$ColorReference <- input$ColorReference
  })
  shiny::observeEvent(input$ColorCustomReference, {
      colthemeCol$ColorCustomReference <- input$ColorCustomReference
  })
  shiny::observeEvent(input$ColorBGplot, {
      colthemeCol$ColorBGplot <- input$ColorBGplot
  })
  shiny::observeEvent(input$ColorPoints, {
      colthemeCol$ColorPoints <- input$ColorPoints
  })
  shiny::observeEvent(input$ColorMemorized, {
      colthemeCol$ColorMemorized <- input$ColorMemorized
  })

  ColorBGplotlight <- shiny::reactiveValues(
    col = grDevices::adjustcolor(
      "#383838",
      red.f = 1.3,
      green.f = 1.3,
      blue.f = 1.3
    )
  )

  shiny::observeEvent(input$ColorBGplot, {
    ColorBGplotlight$col <- grDevices::adjustcolor(
      colthemeCol$ColorBGplot,
      red.f = 1.3,
      green.f = 1.3,
      blue.f = 1.3
    )
  })

  return(
    list(
      colthemeCol = shiny::reactive({colthemeCol}),
      button = shiny::reactive({input$select_col}),
      LabelTabClicked = shiny::reactive({input$LabelTabClicked}),
      LabelParents = shiny::reactive({input$LabelParents}),
      LabelMemorized = shiny::reactive({input$LabelMemorized}),
      LabelFactCont = shiny::reactive({input$LabelFactCont})
    )
  )
}
