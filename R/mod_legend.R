#' legend UI Function
#'
#' @description A shiny Module for the legend plot in Subscreen.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_legend_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::uiOutput(ns('legend'))
  )
}

#' legend Server Function
#'
#' @noRd
mod_legend_server <- function(
  input,
  output,
  session,
  rowwise = FALSE,
  plot_color,
  colthemeCol,
  complement = FALSE
  ) {

  ns <- session$ns

  output$legend <- shiny::renderUI({

      p.col <- colthemeCol()$ColorPoints
      colour <- as.character(
        c(
          grDevices::adjustcolor(p.col, alpha = 1 ),
          grDevices::adjustcolor(p.col, alpha = 0.75 ),
          grDevices::adjustcolor(p.col, alpha = 0.5 ),
          grDevices::adjustcolor(p.col, alpha = 0.25 ),
          grDevices::adjustcolor(p.col, alpha = 0.1 ),
          grDevices::adjustcolor(p.col, alpha = 0.1 ),
          grDevices::adjustcolor(p.col, alpha = 0.1 ),
          grDevices::adjustcolor(p.col, alpha = 0.1 )
        )
      )
    if (rowwise == FALSE) {
      shiny::req(plot_color())
      shiny::tagList(
      if (colour[1] %in% plot_color()) {
        shiny::p(
           shiny::span(
            shiny::tagList(
              tags$i(class = "fa-solid fa-circle",
                style = paste0("color: rgba(",
                  grDevices::col2rgb(colour[2])[1],",", grDevices::col2rgb(colour[2])[2],",",grDevices::col2rgb(colour[2])[3],",", 0.75 ,")"
                )
              ),
              "1-factor level subgroups"
            )
           )
         )
      },
      if (colour[2] %in% plot_color()) {
        shiny::p(
           shiny::span(
            shiny::tagList(
              tags$i(class = "fa-solid fa-circle",
                 style = paste0("color: rgba(",
                  grDevices::col2rgb(colour[2])[1],",", grDevices::col2rgb(colour[2])[2],",",grDevices::col2rgb(colour[2])[3],",", 0.75 ,")"
                )
              ),"2-factor level subgroups"
            )
           )
         )
      },
      if (colour[3] %in% plot_color()) {
        shiny::p(
           shiny::span(
            shiny::tagList(
              shiny::tags$i(
                class = "fa-solid fa-circle",
                 style = paste0("color: rgba(",
                  grDevices::col2rgb(colour[3])[1],",", grDevices::col2rgb(colour[3])[2],",",grDevices::col2rgb(colour[3])[3],",", 0.5 ,")"
                )
              ),"3-factor level subgroups"
            )
           )
         )
      },
      if (colour[4] %in% plot_color()) {
        shiny::p(
           shiny::span(
            shiny::tagList(
              shiny::tags$i(
                class = "fa-solid fa-circle",
                 style = paste0("color: rgba(",
                  grDevices::col2rgb(colour[4])[1],",", grDevices::col2rgb(colour[4])[2],",",grDevices::col2rgb(colour[4])[3],",", 0.25 ,")"
                )
              ),"4-factor level subgroups"
            )
           )
         )
        },
        if (colour[5] %in% plot_color()) {
        shiny::p(
           shiny::span(
            shiny::tagList(
              shiny::tags$i(
                class = "fa-solid fa-circle",
                 style = paste0("color: rgba(",
                  grDevices::col2rgb(colour[5])[1],",", grDevices::col2rgb(colour[5])[2],",",grDevices::col2rgb(colour[5])[3],",", 0.1 ,")"
                )
              ), "5 (or more)-factor level subgroups"
            )
           )
         )
      },
      if (colthemeCol()$ColorTabClicked %in% plot_color()) {
        shiny::p(
          shiny::span(
            shiny::tagList(
              shiny::tags$i(
                class = "fa-solid fa-circle",
                style = paste0("color: ", colthemeCol()$ColorTabClicked)
              ),"Selected Subgroup"
            )
          )
        )
      },
      if (colthemeCol()$ColorSelected %in% plot_color()) {
        shiny::p(
          shiny::span(
            shiny::tagList(
              shiny::tags$i(
              class = "fa-solid fa-circle",
              style = paste0("color: ", colthemeCol()$ColorSelected)
              ),"Filtered Subgroup(s)"
            )
          )
        )
      },
      if (colthemeCol()$ColorImportance %in% plot_color()) {
        shiny::p(
         shiny::span(
          shiny::tagList(
            shiny::tags$i(
              class = "fa-solid fa-circle",
              style = paste0("color: ", colthemeCol()$ColorImportance)
            ),"Importance"
          )
         )
       )
      },
      if (colthemeCol()$ColorParents %in% plot_color()) {
        shiny::h5(
         shiny::span(
          shiny::tagList(
            shiny::tags$i(
              class = "fa-solid fa-circle",
              style = paste0("color: ", colthemeCol()$ColorParents)
            ), "Parent Subgroup(s)"
          )
         )
        )
      },
      if (colthemeCol()$ColorMemorized %in%  plot_color()) {
        tag = shiny::p(
         shiny::span(
          shiny::tagList(
            shiny::tags$i(
              class = "fa-solid fa-circle",
              style = paste0("color: ", colthemeCol()$ColorMemorized)
            ),"Memorized subgroup(s)"
          )
         )
        )
      },
      if (colthemeCol()$ColorFactCont %in% plot_color()) {
        tag = shiny::p(
         shiny::span(
          shiny::tagList(
            shiny::tags$i(
              class = "fa-solid fa-circle",
              style = paste0("color: ", colthemeCol()$ColorFactCont)
            ),"Factorial Context"
          )
         )
        )
      },
      if (complement()) {
               tag = shiny::p(
                 shiny::span(
                  shiny::tagList(
                    shiny::tags$i(
                      class = "fa-solid fa-times-circle",
                      style = paste0("color: #fffb00")
                    ),"Subgroup Complement"
                  )
                 )
             )
        },
        if (different_hues(colthemeCol()$ColorFactCont, value = 50) %in% plot_color()) {
          tag = shiny::p(
            shiny::span(
              shiny::tagList(
                shiny::tags$i(
                  class = "fa-solid fa-circle",
                  style = paste0("color: ", different_hues(colthemeCol()$ColorFactCont, value = 50))
                ),"Pseudo factorial Context"
              )
           )
          )
        },
        if (different_hues(colthemeCol()$ColorFactCont, value = 150) %in% plot_color()) {
          tag = shiny::p(
            shiny::span(
              shiny::tagList(
                shiny::tags$i(
                  class = "fa-solid fa-circle",
                  style = paste0("color: ", different_hues(colthemeCol()$ColorFactCont, value = 150))
                ),"Removed Subgroup to repair Context"
              )
           )
          )
        },
        if (different_hues(colthemeCol()$ColorFactCont, value = 100) %in% plot_color()) {
          tag = shiny::p(
            shiny::span(
              shiny::tagList(
                shiny::tags$i(
                  class = "fa-solid fa-circle",
                  style = paste0("color: ", different_hues(colthemeCol()$ColorFactCont, value = 100))
                ),"Incomplete factorial Context"
              )
           )
          )
        }
      )
    } else {
  shiny::req(plot_color())
      shiny::tagList(

        if (colour[1] %in% plot_color()) {
           shiny::span(
            shiny::tagList(
              shiny::tags$i(
                class = "fa-solid fa-circle",
                style = paste0("color: rgba(",
                  grDevices::col2rgb(colour[1])[1],",", grDevices::col2rgb(colour[1])[2],",",grDevices::col2rgb(colour[1])[3],",", 1 ,")"
                )
              ),"1-factor level subgroups"
            )
           )
      },
      if (colour[2] %in% plot_color()) {
           shiny::span(
            shiny::tagList(
              shiny::tags$i(
                class = "fa-solid fa-circle",
                 style = paste0("color: rgba(",
                  grDevices::col2rgb(colour[2])[1],",", grDevices::col2rgb(colour[2])[2],",",grDevices::col2rgb(colour[2])[3],",", 0.75 ,")"
                )
              ),"2-factor level subgroups"
            )
           )
      },
      if (colour[3] %in% plot_color()) {
           shiny::span(
            shiny::tagList(
              shiny::tags$i(
                class = "fa-solid fa-circle",
                 style = paste0("color: rgba(",
                  grDevices::col2rgb(colour[3])[1],",", grDevices::col2rgb(colour[3])[2],",",grDevices::col2rgb(colour[3])[3],",", 0.5 ,")"
                )
              ),"3-factor level subgroups"
            )
           )
      },
      if (colour[4] %in% plot_color()) {
           shiny::span(
            shiny::tagList(
              shiny::tags$i(
                class = "fa-solid fa-circle",
                 style = paste0("color: rgba(",
                  grDevices::col2rgb(colour[4])[1],",", grDevices::col2rgb(colour[4])[2],",",grDevices::col2rgb(colour[4])[3],",", 0.25 ,")"
                )
              ),"4-factor level subgroups"
            )
           )
        },
        if (colour[5] %in% plot_color()) {
           shiny::span(
            shiny::tagList(
              shiny::tags$i(
                class = "fa-solid fa-circle",
                style = paste0("color: rgba(",
                  grDevices::col2rgb(colour[5])[1],",", grDevices::col2rgb(colour[5])[2],",",grDevices::col2rgb(colour[5])[3],",", 0.1 ,")"
                )
              ), "5 (or more)-factor level subgroups"
            )
           )
      },
      if (colthemeCol()$ColorTabClicked %in% plot_color()) {
          shiny::span(
            shiny::tagList(
              shiny::tags$i(
                class = "fa-solid fa-circle",
                style = paste0("color: ", colthemeCol()$ColorTabClicked)
              ),"Selected Subgroup"
            )
        )
      },
      if (colthemeCol()$ColorSelected %in% plot_color()) {
          shiny::span(
            shiny::tagList(
              shiny::tags$i(
              class = "fa-solid fa-circle",
              style = paste0("color: ", colthemeCol()$ColorSelected)
              ),"Filtered Subgroup(s)"
            )
        )
      },
      if (colthemeCol()$ColorImportance %in% plot_color()) {
         shiny::span(
          shiny::tagList(
            shiny::tags$i(
              class = "fa-solid fa-circle",
              style = paste0("color: ", colthemeCol()$ColorImportance)
            ),"Importance"
          )
         )
      },
      if (colthemeCol()$ColorParents %in% plot_color()) {
         shiny::span(
          shiny::tagList(
            shiny::tags$i(
              class = "fa-solid fa-circle",
              style = paste0("color: ", colthemeCol()$ColorParents)
            ), "Parent Subgroup(s)"
          )
         )
      },
      if (colthemeCol()$ColorFactCont %in% plot_color()) {
         shiny::span(
          shiny::tagList(
            shiny::tags$i(
              class = "fa-solid fa-circle",
              style = paste0("color: ", colthemeCol()$ColorFactCont)
            ),"Factorial Context"
          )
        )
      },
      if (complement()) {
                 shiny::span(
                  shiny::tagList(
                    shiny::tags$i(
                      class = "fa-solid fa-times-circle",
                      style = paste0("color: #fffb00")
                    ),"Subgroup Complement"
                  )
             )
        },
        if (colthemeCol()$ColorMemorized %in%  plot_color()) {
            shiny::span(
              shiny::tagList(
                shiny::tags$i(
                  class = "fa-solid fa-circle",
                  style = paste0("color: ", colthemeCol()$ColorMemorized)
                ),"Memorized subgroup(s)"
              )
           )
        },
        if (different_hues(colthemeCol()$ColorFactCont, value = 50) %in% plot_color()) {
            shiny::span(
              shiny::tagList(
                shiny::tags$i(
                  class = "fa-solid fa-circle",
                  style = paste0("color: ", different_hues(colthemeCol()$ColorFactCont, value = 50))
                ),"Pseudo factorial Context"
              )
           )
        },
        if (different_hues(colthemeCol()$ColorFactCont, value = 150) %in% plot_color()) {
          tag = shiny::p(
            shiny::span(
              shiny::tagList(
                shiny::tags$i(
                  class = "fa-solid fa-circle",
                  style = paste0("color: ", different_hues(colthemeCol()$ColorFactCont, value = 150))
                ),"Removed subgroup(s) to repair context"
              )
           )
          )
        },
        if (different_hues(colthemeCol()$ColorFactCont, value = 100) %in% plot_color()) {
          tag = shiny::p(
            shiny::span(
              shiny::tagList(
                shiny::tags$i(
                  class = "fa-solid fa-circle",
                  style = paste0("color: ", different_hues(colthemeCol()$ColorFactCont, value = 100))
                ),"Incomplete factorial Context"
              )
           )
          )
        }
      )
    }
  })
}
