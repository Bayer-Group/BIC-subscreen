#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @noRd
app_ui <- function(request) {
  app_options <- golem::get_golem_options()

  explorer_tables <- if (app_options$showTables) {
    shiny::fluidRow(
      col_12(
        shiny::tabsetPanel(
          type = "tabs",
          shiny::tabPanel(
            "Selected Subgroups",
            DT::DTOutput("selectedSG"),
            icon = shiny::tags$i(class = "fa-solid fa-circle")
          ),
          shiny::tabPanel(
            title = "Filtered Subgroups",
            DT::DTOutput("filteredSG"),
            icon = shiny::icon("filter")
          ),
          shiny::tabPanel(
            title = "Parent Subgroups",
            value = "ParentSubgroup",
            DT::DTOutput("parents"),
            icon = shiny::icon("sitemap")
          ),
          shiny::tabPanel(
            title = "Factorial Contexts",
            value = "FactorialSubgroup",
            DT::DTOutput("factorial"),
            icon = shiny::icon("list")
          ),
          shiny::tabPanel(
            title = "Subgroup Complement",
            value = "ComplementSubgroup",
            DT::DTOutput("complement"),
            icon = shiny::tags$i(class = "fa-solid fa-times-circle")
          ),
          shiny::tabPanel(
            title = "Memorized Subgroups",
            col_12(DT::DTOutput("memorizedSG")),
            icon = shiny::icon("edit")
          )
        )
      )
    )
  } else {
    NULL
  }

  shiny::tagList(
    golem_add_external_resources(),
    shinyjs::useShinyjs(debug = TRUE),
    shinyjs::extendShinyjs(
      script = "www/tabs.js",
      functions = c("disableTab", "enableTab")
    ),
    bslib::page_navbar(
      title = shiny::uiOutput("logofile"),
      id = "navpanel",
      window_title = "Subgroup Explorer",
      theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
      sidebar = bslib::sidebar(
        title = NULL,
        id = "sidebar",
        width = 380,
        bslib::accordion(
          id = "app_sidebar_accordion",
          multiple = TRUE,
          bslib::accordion_panel(
            title = "Variable Options",
            shiny::wellPanel(variableOptionsPanel())
          ),
          bslib::accordion_panel(
            title = "Display Options",
            shiny::wellPanel(
              displayOptionsPanel(
                custom_ref_line_at_start = app_options$reference_line_at_start,
                custom_ref_line_value = app_options$reference_value,
                favour_label_at_start = app_options$favour_label_at_start,
                favour_direction = app_options$favour_direction
              )
            )
          ),
          bslib::accordion_panel(
            title = "Colour Options",
            shiny::wellPanel(mod_color_ui("color")),
            bsplus::use_bs_popover(),
            bsplus::use_bs_tooltip()
          )#,
          # bslib::accordion_panel(
          #   title = "Importance Tab",
          #   value = "ImportanceTab",
          #   mod_variable_importance_ui("vi")
          # )
        )
      ),
      bslib::nav_panel(
        title = "Explorer",
        value = "SubscreenExplorer",
        icon = shiny::icon("braille"),
        shiny::tagList(
          bslib::layout_sidebar(
            height = "auto",
            fillable = TRUE,
            sidebar = bslib::sidebar(
              title = "Interaction plot",
              width = 420,
              position = "right",
              shiny::uiOutput("interaction_panel")
            ),
            bslib::card(
              full_height = TRUE,
              mod_graph_ui(
                "graph1",
                plotHeight = 700,
                plotWidth = "100%"
              )
            )
          ),
          shiny::fluidRow(col_12(mod_legend_ui("legend1"))),
          explorer_tables
        )
      ),
      bslib::nav_panel(
        title = "Comparer",
        value = "SubscreenComparer",
        icon = shiny::icon("object-group"),
        bslib::layout_sidebar(
          height = "auto",
          fillable = TRUE,
          sidebar = bslib::sidebar(
            title = NULL,
            width = 380,
            position = "left",
            mod_comparer_sidebar_ui("comparer")
          ),
          bslib::card(
            full_height = TRUE,
            mod_comparer_tabs_ui("comparer")
          )
        )
      ),
      bslib::nav_panel(
        title = "Mosaic",
        value = "SubscreenMosaic",
        icon = shiny::icon("th-list"),
        bslib::layout_sidebar(
          height = "auto",
          fillable = TRUE,
          sidebar = bslib::sidebar(
            title = NULL,
            width = 380,
            position = "left",
            mod_mosaic_sidebar_ui("mosaic")
          ),
          bslib::card(
            full_height = TRUE,
            mod_mosaic_main_ui("mosaic")
          )
        )
      ),
      # bslib::nav_panel(
      #   title = "ASMUS",
      #   value = "SubscreenAsmus",
      #   icon = shiny::icon("tasks"),
      #   bslib::layout_sidebar(
      #     height = "auto",
      #     fillable = TRUE,
      #     sidebar = bslib::sidebar(
      #       title = NULL,
      #       width = 300,
      #       position = "left",
      #       shiny::helpText(
      #         "ASMUS: use the target variable, plot type, and factorial",
      #         "selection controls in the main panel."
      #       )
      #     ),
      #     bslib::card(
      #       full_height = TRUE,
      #       asmus2_module_ui("asmus2")
      #     )
      #   )
      # ),
      bslib::nav_panel(
        title = "Upload",
        value = "SubscreenUpload",
        icon = shiny::icon("upload"),
        upload_tab_ui("upload_tab_ui_1")
      ),
      bslib::nav_spacer(),
      bslib::nav_item(
        shinyWidgets::dropdownButton(
          inputId = "app_theme_dropdown",
          label = "",
          icon = shiny::icon("palette"),
          status = "secondary",
          size = "sm",
          circle = TRUE,
          inline = TRUE,
          margin = "0 8px 0 0",
          shiny::tags$div(
            class = "px-2 py-1",
            shiny::radioButtons(
              inputId = "app_theme_mode",
              label = "Appearance",
              choiceNames = c("Light", "Dark"),
              choiceValues = c("light", "dark"),
              selected = "light"
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @noRd
golem_add_external_resources <- function() {
  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  shiny::tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "subscreen"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
