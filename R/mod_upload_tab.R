# helper functions user-interface
uploadButton <- function(id, label, icon) {
  shiny::actionButton(
    inputId = id,
    label = label,
    icon = icon(icon),
    style = "color: #fff; background-color: #5cb85c; border-color: #fff"
  )
}

uploadFileInput <- function(id, label) {
  shiny::fileInput(
    inputId = id,
    label = label,
    multiple = TRUE,
    accept = c(".RData",".rds")
  )
}

#' upload_tab UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param bg.col background color
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @importFrom graphics text
#'


upload_tab_ui <- function(id, bg.col) {
  ns <- NS(id)
  shiny::tagList(
    list(
      shiny::tags$head(
        shiny::tags$style(
          paste(
            "body { color: ",
            font_color(bg.col),
            "}",
            sep = ""
          )
        )
      )
    ),
    shiny::h3("Welcome to"),
    shiny::uiOutput(ns("myImage")),
    shiny::column(3,
      shiny::h4("Please upload your prepared data or use the demo data set."),
      shiny::uiOutput(ns("mode")),
      shiny::fluidPage(
        shiny::conditionalPanel(condition = "input.mode == 'rdata'", ns = ns,
          uploadFileInput(ns("results_file"), "Choose results data file (created with subscreencalc())"),
          shinyWidgets::materialSwitch(
            inputId = ns("switch_vi_file"),
            label = HTML("<span style = 'color: white;'> Add variable importance file or press 'Upload data' </span>"),
            status = "success"
          ),
          shiny::conditionalPanel(condition = "input.switch_vi_file == true", ns = ns,
            uploadFileInput(ns("vi_file"), "Choose variable importance file (optional/created with subscreenvi())")
          ),
          #internal function
          uploadButton(ns('apply_rdata_files'),'Upload data',"upload")
        ),
        shiny::conditionalPanel(condition = "input.mode == 'demo'", ns = ns,
          uploadButton(ns('apply_demo_data'),'Use demo data',"hdd")
        ),
        shiny::conditionalPanel(condition = "input.mode == 'uploaded'", ns = ns,
          uploadButton(ns('apply_uploaded_data'),'Use uploaded data',"download")
        )
      )
    ),
    shiny::column(3,
      uiOutput(ns("variable_filter_selection"))#,
      #actionButton(ns('apply_filter'),'Update selection')
    ),
    shiny::column(6,
      shiny::uiOutput(ns("list_output"))
    )
  )
}

# helper functions server
uploadInformationOutput <- function(
    previewScresults = preview_scresults_tmp$dat,
    mode = input$mode,
    resultsFile = input$results_file,
    dataSetName = dat_name,
    font = font_col
  ) {
  preview_scresults_tmp <- input <- dat_name <- font_col <- NULL
  if (!is.null(previewScresults)) {
      if(is(previewScresults) == "SubScreenResult") {
      shinyjs::enable("apply_rdata_files")
      shiny::HTML(
        paste0("
        <p style = 'color: ",font,"'>
          Dataset: <b style='font-size: 130%; color: #428bca'> ",
          if (mode == "demo") {
            "results_factorial_complement_true.rda"
          } else if (mode == "rdata") {
            resultsFile$name
          } else if (mode == "uploaded") {
            dataSetName
          }
          ,"</b><br>
          Number of subjects: <b style='font-size: 130%; color: #428bca'>", previewScresults$results_total$N.of.subjects," </b><br>
          Number of subgroups: <b style='font-size: 130%; color: #428bca'>",max(previewScresults$sge$SGID),"</b><br>
          Number target variables: <b style='font-size: 130%; color: #428bca'>",length(previewScresults$results_total)-1,"</b>  <b style='font-size: 100%; color: ",font,"'>(",paste(names(previewScresults$results_total)[names(previewScresults$results_total)!="N.of.subjects"], collapse = ", "),")</b><br>
          Number factors: <b style='font-size: 130%; color: #428bca'>",length(previewScresults$factors),"</b> <b style='font-size: 100%; color: ",font,"'>(",paste(previewScresults$factors, collapse = ", "),")</b><br>
          Number factor combinations: <b style='font-size: 130%; color: #428bca'>",length(previewScresults$min_comb:previewScresults$max_comb)," </b> (",previewScresults$min_comb,"-",previewScresults$max_comb,") <br>

          <br>

          Factorial context calculation performed: ",
          if (any(startsWith(colnames(previewScresults$sge),"FCID_complete_"))) {
            "<i class='fa-solid fa-check' style ='color: #5cb85c; font-size: 150%'></i>"
          } else if (any(colnames(previewScresults$sge) == "FCID_complete")) {
            "<i class='fa-solid fa-exclamation' style ='color: #ffffff; font-size: 150%'></i> (Results structure outdated! Please use subscreencalc version >4.0.0)"
          } else {
            "<i class='fa-solid fa-times' style ='color: #ffffff; font-size: 150%'></i>"
          } ,"<br>
          Subgroup complement calculation performed: ",
          if (any(startsWith(colnames(previewScresults$sge),"Complement_"))) {
            "<i class='fa-solid fa-check' style ='color: #5cb85c; font-size: 150%'></i>"
          } else {
            "<i class='fa-solid fa-times' style ='color: #fffff; font-size: 150%'></i>"
          } ,"<br>

          <br>

          Check for list input: <b style='font-size: 150%;'>",
          ifelse(
            is.list(previewScresults),
            "<i class='fa-solid fa-check' style ='color: #5cb85c'></i>",
            "<i class='fa-solid fa-times'></i>"
          )
          ,"</b><br>
          Check for non-empty list input sge: <b style='font-size: 150%;'>",
          ifelse(
            dim(previewScresults$sge)[1]>0,
            "<i class='fa-solid fa-check' style ='color: #5cb85c'></i>",
            "<i class='fa-solid fa-times'></i>"
          )
          ,"</b><br>
          Check for class SubScreenResult: <b style='font-size: 150%;'>",
            "<i class='fa-solid fa-check' style ='color: #5cb85c'></i>
            </b><br>
        </p>
      ")
      )
    } else {
      shinyjs::disable("apply_rdata_files")
      shiny::HTML(
        paste0("
          <p style = 'color: ",font,"'>
            Check for class SubScreenResult:
            <b style='font-size: 150%;'>",
              "<i class='fa-solid fa-times' style ='color: #f71b4b'></i>
            </b>
          </p>"
        )
      )
    }
  }
}

#' upload_tab Server Function
#'
#' @param input internal shiny parameter.
#' @param output internal shiny parameter.
#' @param session internal shiny parameter.
#' @param dat results data set.
#' @param dat_name name of data set.
#' @param vi variable importance data set.
#' @importFrom methods is
#'
#' @noRd
upload_tab_server <- function(input, output, session, dat, dat_name, vi, font_col = "#e3e3e3") {
  ns <- session$ns

  output$mode <- shiny::renderUI({
    if (!is.null(dat)) {
      choices <- c(
        ".RData file(s) (from Disc)" = "rdata",
        "Demo data" = "demo",
        "Uploaded data via function call" = "uploaded"
      )
    } else {
      choices <- c(
        ".RData file(s) (from Disc)" = "rdata",
        "Demo data" = "demo"
      )
    }

    if(exists("studies")) {
      choices <- c(choices, "Upload from server" = "server")
    }

    shiny::radioButtons(
      inputId = ns("mode"),
      label = "Input mode:",
      choices = choices
    )
  })

  preview_scresults_tmp <- reactiveValues(dat = NULL)
  preview_variable_importance_tmp <- reactiveValues(dat = NULL)
  # buttons_clicked <- reactiveValues(dat = 0)
  # shiny::observeEvent(c(input$apply_rdata_files,input$apply_demo_data, input$apply_uploaded_data),{
  #   buttons_clicked$dat <- buttons_clicked$dat + 1
  # })

  shiny::observeEvent(c(input$mode,input$results_file), {
    if (input$mode == "rdata") {
      if (!is.null(input$results_file$datapath)) {
        if (utils::tail(strsplit(input$results_file$datapath,"/.")[[1]], n = 1) %in% c(".rdata",".RData")) {
          preview_scresults_tmp$dat <- get(load(input$results_file$datapath))
        }
        if (utils::tail(strsplit(input$results_file$datapath,"/.")[[1]], n = 1) == ".rds") {
          preview_scresults_tmp$dat <- readRDS(input$results_file$datapath)
        }
      } else {
        preview_scresults_tmp$dat <- NULL
      }
      if (!is.null(input$vi_file$datapath)) {
        if (utils::tail(strsplit(input$vi_file$datapath,"/.")[[1]], n = 1) %in% c(".rdata",".RData")) {
          preview_variable_importance_tmp$dat <- get(load(input$vi_file$datapath))
        }
        if (utils::tail(strsplit(input$vi_file$datapath,"/.")[[1]], n = 1) == ".rds") {
          preview_variable_importance_tmp$dat <- readRDS(input$vi_file$datapath)
        }
      } else {
        preview_variable_importance_tmp$dat <- NULL
      }
    } else if (input$mode == "demo") {
      preview_scresults_tmp$dat <- get(load(paste0(getwd(),"/data/results_factorial_complement_true.rda")))
      preview_variable_importance_tmp$dat <- get(load(paste0(getwd(),"/data/importance.rda")))

    } else if (input$mode == "uploaded") {
      preview_scresults_tmp$dat <- dat
      preview_variable_importance_tmp$dat <- vi
    }
  })

  output$list_output <- shiny::renderUI({
    shiny::req(preview_scresults_tmp$dat)
    input$results_file
    uploadInformationOutput(preview_scresults_tmp$dat, input$mode, input$results_file, dat_name, font = font_col())
  })
  output$variable_filter_selection <- shiny::renderUI({
    shinyWidgets::pickerInput(
      inputId =ns("variable_filter_selection"),
      label = "Select/Deselect Factors",
      choices = preview_scresults_tmp$dat$factors,
      selected = preview_scresults_tmp$dat$factors,
      options = list(
      `actions-box` = TRUE
      ),
      multiple = TRUE
    )
  })
  output$myImage <- shiny::renderUI({
    list(shiny::HTML("<img src = 'www/subscreen_logo.png' alt = 'Subgroup Explorer Logo' width = '423' height = '140'>"))
  })

  scresults_tmp <- shiny::reactiveValues(
    dat = dat
  )

   variable_importance_tmp <- shiny::reactiveValues(
    dat = vi
  )

  #### Press demo data button ####
  shiny::observeEvent(input$apply_demo_data, {
    scresults_tmp$dat <- get(load(paste0(getwd(),"/data/results_factorial_complement_true.rda")))
    variable_importance_tmp$dat <- get(load(paste0(getwd(),"/data/importance.rda")))
  })

  # shiny::observeEvent(input$apply_demo_data, {
  #
  # })

  #### Press uploaded data button ####
  shiny::observeEvent(input$apply_uploaded_data, {
    scresults_tmp$dat <- dat
  })

  shiny::observeEvent(input$apply_rdata_files, {
    if (!is.null(input$results_file$datapath)) {
      if (utils::tail(strsplit(input$results_file$datapath,"/.")[[1]], n = 1) %in% c(".rdata",".RData")) {
        scresults_tmp$dat <- get(load(input$results_file$datapath))
      }
      if (utils::tail(strsplit(input$results_file$datapath,"/.")[[1]], n = 1) == ".rds") {
        scresults_tmp$dat <- readRDS(input$results_file$datapath)
      }
      if (!is.null(input$vi_file$datapath)) {
        if (utils::tail(strsplit(input$vi_file$datapath,"/.")[[1]], n = 1) %in% c(".rdata",".RData")) {
          variable_importance_tmp$dat <- get(load(input$vi_file$datapath))
        }
        if (utils::tail(strsplit(input$vi_file$datapath,"/.")[[1]], n = 1) == ".rds") {
          variable_importance_tmp$dat <- readRDS(input$vi_file$datapath)
        }
      }
    }
  })


  filtered_scresults_tmp <- shiny::eventReactive(c(input$apply_demo_data,input$apply_uploaded_data,input$apply_rdata_files), {
    scresults_tmp_copy <- preview_scresults_tmp$dat
    if(!is.null(input$variable_filter_selection)){
      remove_factor_vector <- preview_scresults_tmp$dat$factors[!preview_scresults_tmp$dat$factors%in%input$variable_filter_selection]
      if(length(remove_factor_vector)!=0){
        for(i in remove_factor_vector) {
          scresults_tmp_copy$sge <- scresults_tmp_copy$sge %>% dplyr::filter(!!rlang::sym(i) == "Not used")
          scresults_tmp_copy$factors <- scresults_tmp_copy$factors[-which(scresults_tmp_copy$factors == i)]
        }
      }
    }
    scresults_tmp_copy
  })
  return(
    list(
      parameter1 = filtered_scresults_tmp,
      parameter2 = shiny::reactive({variable_importance_tmp$dat})#,
      #parameter3 = shiny::reactive({buttons_clicked$dat})
    )
  )
}
