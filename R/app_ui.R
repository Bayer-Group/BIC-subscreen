#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @noRd
app_ui <- function(request) {
  app_options <- golem::get_golem_options()
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    ##### Options #####
    shinydashboard::dashboardPage(
      header = shinydashboard::dashboardHeader(
        title = shiny::uiOutput('logofile')
      ),
      sidebar = 
        shinydashboard::dashboardSidebar(
          id = "Options",
          shinydashboard::menuItem(
            text="Variable Options",
            variableOptionsPanel(),
            icon = shiny::icon("wrench")
          ),
          shinydashboard::menuItem(
            text="Importance Tab",
            mod_variable_importance_ui("vi"),
            icon = shiny::icon("exclamation")
          ),
          shinydashboard::menuItem(
            text="Display Options",
            displayOptionsPanel(
              custom_ref_line_at_start = app_options$reference_line_at_start,
              custom_ref_line_value = app_options$reference_value,
              favour_label_at_start = app_options$favour_label_at_start,
              favour_direction = app_options$favour_direction
            ),
            icon = shiny::icon('eye')
          ),
          shinydashboard::menuItem(
            text="Colour Options",
            mod_color_ui("color"),
            bsplus::use_bs_popover(),
            bsplus::use_bs_tooltip(),
            icon = shiny::icon("paint-brush")
          ), 
          collapsed = TRUE
        ),
      body =
        ##### Plots #####
      shinydashboard::dashboardBody(
          shinyjs::useShinyjs(debug = TRUE),
          shinyjs::extendShinyjs(
                  script = "www/tabs.js",
                  functions = c("disableTab", "enableTab")
                ),
        dashboardthemes::shinyDashboardThemes(
          theme = "grey_dark"
        ),
        shiny::navbarPage(
          title = "Subgroup Explorer",
          id = "navpanel",
          ##### 1. EXPLORER (UI)####
          shiny::tabPanel(
            "Explorer",
            value = "SubscreenExplorer",
            shiny::fluidPage(
              shiny::fluidRow(
                shiny::uiOutput('logo'),
                #### Explorer-graph ####
                col_9(
                  mod_graph_ui("graph1",
                               plotHeight = 700,
                               plotWidth = "100%"
                  )
                ),
                shiny::uiOutput('interactionPanel'),
                col_12(offset = 3, mod_legend_ui("legend1"))
              ),
              #### Tables  ####
              if (app_options$showTables) {
                shiny::fluidRow(
                  col_12(
                    shiny::tabsetPanel(
                      type = "tabs",
                      shiny::tabPanel(
                        "Selected Subgroups",
                        DT::DTOutput("selectedSG"),
                        icon = shiny::icon("circle")
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
                        title ="Subgroup Complement",
                        value = "ComplementSubgroup",
                        DT::DTOutput("complement"),
                        icon = shiny::icon("circle")
                      ),
                      shiny::tabPanel(
                        title = "Memorized Subgroups",
                        col_12(
                          DT::DTOutput("memorizedSG")
                        ),
                        icon = shiny::icon("edit")
                      )
                    )
                  )
                )
              }
            ), fluid = FALSE, position = c("static-top"), inverse = FALSE, icon = shiny::icon("braille")
          ),
          #### 2. COMPARER (UI)####
          shiny::tabPanel(
            title = "Comparer",
            value = "SubscreenComparer",
            mod_comparer_ui("comparer"),
            icon = shiny::icon("object-group")
          ),
          #### 3. MOSAIC (UI)####
          shiny::tabPanel(
            title = "Mosaic",
            value = "SubscreenMosaic",
            mod_mosaic_ui("mosaic"),
            icon = shiny::icon("th-list")
          ),
          #### 4. ASMUS (UI) ####
          shiny::tabPanel(
            title = "ASMUS",
            value = "SubscreenAsmus",
            asmus2_module_ui("asmus2"),
            icon = shiny::icon("tasks")
          ),
          #### 5.UPLOAD (UI) ####
          shiny::tabPanel(
            title = "Upload",
            value = "SubscreenUpload",
            upload_tab_ui("upload_tab_ui_1", bg.col = "#383838"),
            icon = shiny::icon("upload")
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
