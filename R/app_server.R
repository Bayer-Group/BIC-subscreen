#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#' @noRd
app_server <- function(input, output, session) {
  app_options <- golem::get_golem_options()
  shinyjs::logjs("Welcome to Subscreen Explorer!")

  options(shiny.maxRequestSize = 300*1024^2)

  #disable tab until data are observed (scresults_tmp$dat)
  shinyjs::js$disableTab("SubscreenExplorer")
  shinyjs::js$disableTab("SubscreenComparer")
  shinyjs::js$disableTab("SubscreenMosaic")
  shinyjs::js$disableTab("SubscreenAsmus")

  #### OBSERVEEVENTS ####

  #open interaction plot when subgroup is clicked and is (pseudo-)complete
  shiny::observeEvent(new_selected_ids$val, {

    if (!is.null(scresults_tmp$dat)) {
      #if factorial context calc was performed
      if(any(startsWith(colnames(scresults_tmp$dat$sge),"FCID_complete_"))) {
      tmp <- scresults_tmp$dat$sge[
        scresults_tmp$dat$sge$SGID == as.numeric(new_selected_ids$val) ,
        c(paste0("FCID_complete_",input$y),paste0("FCID_incomplete_",input$y),paste0("FCID_pseudo_",input$y))
      ]
      if(
        tmp[paste0("FCID_complete_",input$y)] != "Not complete" |
        tmp[paste0("FCID_pseudo_",input$y)] != "No Pseudo"
      ) {
        shinyWidgets::updatePrettyToggle(
          session,
          inputId = 'showPanel2',
          value = TRUE
        )
      } else {

      }
      }
    }
  })
  #### ObserveEvent: scresults_tmp$dat ####
  ## If the upload tab is actived, update TabsetPanel to start with Explorer-tab
  shiny::observeEvent(scresults_tmp$dat, {
    if (is.null(scresults_tmp$dat)) {
      shiny::updateTabsetPanel(
        session,
        inputId = "navpanel",
        selected = "SubscreenUpload"
      )
    } else {
      shiny::updateTabsetPanel(
        session,
        inputId = "navpanel",
        selected = "SubscreenExplorer"
      )
    }
  }, ignoreNULL = FALSE)

  #### ObserveEvent: upload_data$parameter1() ####
  shiny::observeEvent(upload_data$parameter1(), {
    scresults_tmp$dat <- upload_data$parameter1()
  })

  #### ObserveEvent: scresults_tmp$dat ####
  # update slider when data are available
  # (app can than start without data)
  shiny::observeEvent(scresults_tmp$dat, {

    choi <- setdiff(
      names(scresults_tmp$dat$results_total[ ,!is.na(scresults_tmp$dat$results_total)]),
      names(which(apply(scresults_tmp$dat$sge[,names(scresults_tmp$dat$results_total)],2,function(x) {(!all(is.finite(x[!is.na(x)])))})))
    )

    shiny::updateSelectInput(
      session,
      inputId = "y",
      choices = choi,
      selected = choi[1]
    )
    shiny::updateSelectInput(
      session,
      inputId = "x",
      choices = choi,
      selected = "N.of.subjects"
    )
    shiny::updateSelectInput(
      session,
      inputId = "filter",
      choices = c("no selection", scresults_tmp$dat$factors)
    )
    shiny::updateSelectInput(
      session,
      inputId = "filter2",
      choices = c("no selection", scresults_tmp$dat$factors)
    )
    if (is.na(app_options$subgroup_levels_at_start)) {
      key_value <-  c(1, min(c(3, scresults_tmp$dat$max_comb), na.rm = TRUE))
    } else {
      key_value <- c(1, app_options$subgroup_levels_at_start)
    }
    shiny::updateSliderInput(
      session,
      inputId = "key",
      min = scresults_tmp$dat$min_comb,
      max = scresults_tmp$dat$max_comb,
      value = key_value
      #value = c(1, min(c(3, scresults_tmp$dat$max_comb), na.rm = TRUE))
      #value = c(1, min(c(3, scresults_tmp$dat$max_comb), na.rm = TRUE))#,
      #selected = ifelse(!is.na(app_options$subgroup_level_at_start),app_options$subgroup_level_at_start,)
    )
  })

  shiny::observeEvent(input$filter2, {
    shiny::updateSelectInput(
      session,
      inputId = "filter",
      selected = input$filter,
      choices = c("no selection", scresults_tmp$dat$factors[!scresults_tmp$dat$factors %in% input$filter2])
    )
  })

  shiny::observeEvent(input$filter, {
    shiny::updateSelectInput(
      session,
      inputId = "filter2",
      selected = input$filter2,
      choices = c("no selection", scresults_tmp$dat$factors[!scresults_tmp$dat$factors %in% input$filter])
    )
  })
  #### ObserveEvent: input$y ####
  shiny::observeEvent(input$y, {
    shiny::req(input$y)
    shiny::req(scresults_tmp$dat)
    if (roundDownNice(min(scresults_tmp$dat$sge[, input$y], na.rm = TRUE), nice = app_options$nice_numbers) <= 0) {
      shiny::updateRadioButtons(
        inputId = "plot_type",
        label = "Type",
        choices = c(linear = "lin"),
        selected = "lin",
        inline = TRUE
      )
    } else {
      shiny::updateRadioButtons(
        inputId = "plot_type",
        label = "Type",
        choices = c(linear = "lin", log = "log"),
        selected = app_options$yaxis_type,
        inline = TRUE
      )
    }
  })
  # Get %>% click information of the 3 graphs and save the last click

  # shinyjs::js$disableTab("ImportanceTab")
  #
  # #### Observe: enable ImportanceTab
  # shiny::observe({
  #   if (!is.null(variable_importance_tmp$dat)) {
  #     shinyjs::js$enableTab("ImportanceTab")
  #   }
  # })

  shinyjs::js$disableTab("ComplementSubgroup")

  #### Observe: enable ComplementSubgroup
  shiny::observe({
    if(!is.null(scresults_tmp$dat)) {
      if (any(startsWith(colnames(scresults_tmp$dat$sge), "Complement_"))) {
        shinyjs::js$enableTab("ComplementSubgroup")
      }
    }
  })

  shinyjs::js$disableTab("ParentSubgroup")

  #### Observe: enable ParentSubgroup ####
  shiny::observe({
    if(!is.null(scresults_tmp$dat)) {
      if (scresults_tmp$dat$max_comb > 1) {
        shinyjs::js$enableTab("ParentSubgroup")
      }
    }
  })

  shinyjs::useShinyjs(debug = TRUE)
  shinyjs::disable("ColorImportance")

  #### Observe: enable ColorImportance ####
  shiny::observe({
    if (!is.null(variable_importance_tmp$dat)) {
      shinyjs::enable("ColorImportance")
    }
  })

  shinyjs::disable("ColorParents")

  #### Observe: enable ColorParents ####
  shiny::observe({
    if (!is.null(scresults_tmp$dat)) {
      if (scresults_tmp$dat$max_comb > 1) {
        shinyjs::enable("ColorParents")
      }
    }
  })

  shiny::observeEvent(input$navpanel, {
    if (input$navpanel == "SubscreenUpload") {
      new_selected_ids$val <- NULL
    }
  })

  #### ObserveEvent: mod_graph_vars1$selected_SGIDs() ####
  shiny::observeEvent(mod_graph_vars1$selected_SGIDs(), {
    new_selected_ids$val <- mod_graph_vars1$selected_SGIDs()
  }, ignoreNULL = FALSE)

  #### ObserveEvent: mod_comparer_vars$selected_SGIDs2() ####
  shiny::observeEvent(mod_comparer_vars$selected_SGIDs2(), {
    new_selected_ids$val <- mod_comparer_vars$selected_SGIDs2()
  }, ignoreNULL = FALSE)

  #### ObserveEvent: mod_comparer_vars$selected_SGIDs3() ####
  shiny::observeEvent(mod_comparer_vars$selected_SGIDs3(), {
    new_selected_ids$val <- mod_comparer_vars$selected_SGIDs3()
  }, ignoreNULL = FALSE)

  #### ObserveEvent: mod_comparer_vars$selected_SGIDs3() ####
  shiny::observeEvent(mod_comparer_vars$selected_SGIDs4(), {
    new_selected_ids$val <- mod_comparer_vars$selected_SGIDs4()
  }, ignoreNULL = FALSE)

  #### ObserveEvent: click_points_data$xy, scresults_tmp$dat, backgroundColor() ####

  if (app_options$showTables) {
  shiny::observeEvent(c(
    new_selected_ids$val,
    scresults_tmp$dat, backgroundColor()), {
    shiny::req(scresults_tmp$dat)
    if(!is.null(new_selected_ids$val)) {
      table_row <- scresults_tmp$dat$sge[scresults_tmp$dat$sge$SGID == new_selected_ids$val ,]
    } else{
      table_row <- scresults_tmp$dat$sge[0,]
    }
    Memorize = shinyInput(
      shiny::actionButton,
      1,
      'button_',
      label = "Memorize",
      onclick = 'Shiny.onInputChange(\"select_button\",  this.id)',
      click_data = new_selected_ids$val
    )
    if (dim(table_row)[1] == 0) {
      if (!is.null(scresults_tmp$dat)) {
        empty_data <- scresults_tmp$dat$sge[0,c("SGID", colnames(scresults_tmp$dat$results_total))]

        if (dim(empty_data)[2] > 5) {
          empty_data <- empty_data[,1:5]
        }
      } else {
        empty_data <- NULL
      }

      tmp <- DT::datatable(
        data = empty_data,
        selection = 'single',
        extensions = 'Buttons',
        escape = FALSE,
        options = list(
          language = list(emptyTable = 'Select a subgroup by clicking on a point in the graph above!'),
          columnDefs = list(list(targets = 1, visible = TRUE)),
          initComplete = DT::JS(
            "function(settings, json) {",
            paste0("$(this.api().table().header()).css({'background-color': '",
               backgroundColor(),
               "', 'color': '",
               font_color(different_hues(backgroundColor())),
               "'});"
            ),"}"
          ),
          dom = 'Brtip',
          buttons = c('copy','print','pageLength', I('colvis')),
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
        caption = 'Table of Selected Subgroups',
        filter='top'
      )

    if (!is.null(empty_data)) {
      tmp <- DT::formatStyle(
        table = tmp,
        columns = seq_len(ncol(empty_data)),
        target = "cell",
        backgroundColor = different_hues(backgroundColor()),
        border = paste0('.5px solid ', backgroundColor())
      )

    }


    output$selectedSG <- DT::renderDataTable(tmp)

    }

    if (dim(table_row)[1] > 0) {

      col2hide <- which(sapply(table_row, FUN = function(x){all(x == 'Not used')})) - 1

      names(col2hide) <- NULL

      tmp <- DT::datatable(
        data = cbind(Memorize, table_row),
        selection = 'single',
        extensions = 'Buttons',
        escape = FALSE,
        options = list(
          columnDefs = list(list(targets = col2hide + 1, visible = FALSE)),
          initComplete = DT::JS(
            "function(settings, json) {",
            paste0("$(this.api().table().header()).css({'background-color': '",
                   backgroundColor(),
                   "', 'color': '",
                   font_color(different_hues(backgroundColor())),
                   "'});"
            ),"}"
          ),
          dom = 'Brtip',
          buttons = c('copy','print','pageLength', I('colvis')),
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
        caption = 'Table of Selected Subgroup',
        filter='top'
      )

      tmp <- DT::formatStyle(
        table = tmp,
        columns = 1:(ncol(table_row) + 1),
        target = "cell",
        backgroundColor = different_hues(backgroundColor()),
        border = paste0('.5px solid ', backgroundColor())
      )
      tmp.sglev <- levels(
        stats::relevel(
          factor(unlist(lapply(table_row[, scresults_tmp$dat$factors], as.character))),
                ref = 'Not used'
        )
      )
      colXY <- which(colnames(table_row) %in% c('SGID', names(scresults_tmp$dat$results_total), 'nfactors')) + 1

      col.tabFont <- font_color(different_hues(backgroundColor()))
      col.tabBack <- backgroundColor()

      tmp <- DT::formatStyle(
        table = tmp,
        columns = names(table_row),
        color = col.tabFont
      )

      tmp <- DT::formatStyle(
        table = tmp,
        columns = scresults_tmp$dat$factors,
        color = DT::styleEqual(
          tmp.sglev,
          c(col.tabBack, rep(col.tabFont, length(tmp.sglev) - 1))
        )
      )
      #### Output: selectedSG ####
      output$selectedSG <- DT::renderDataTable(tmp)
    }
  })
  }

  #### ObserveEvent: upload_data$parameter1(), mod_color_vars$colthemeCol()$... ####
  shiny::observeEvent(
    c(upload_data$parameter1(),
      mod_color_vars$colthemeCol()$ColorSelected,
      mod_color_vars$colthemeCol()$ColorFactCont,
      mod_color_vars$colthemeCol()$ColorParents,
      mod_color_vars$colthemeCol()$ColorTabClicked,
      mod_color_vars$colthemeCol()$ColorImportance,
      mod_color_vars$colthemeCol()$ColorReference,
      mod_color_vars$colthemeCol()$ColorPoints,
      mod_color_vars$colthemeCol()$ColorMemorized,
      mod_color_vars$colthemeCol()$ColorCustomReference
    ), {
    colthemeCol$ColorSelected <- mod_color_vars$colthemeCol()$ColorSelected
    colthemeCol$ColorFactCont <- mod_color_vars$colthemeCol()$ColorFactCont
    colthemeCol$ColorParents <- mod_color_vars$colthemeCol()$ColorParents
    colthemeCol$ColorTabClicked <- mod_color_vars$colthemeCol()$ColorTabClicked
    colthemeCol$ColorImportance <- mod_color_vars$colthemeCol()$ColorImportance
    colthemeCol$ColorReference <- mod_color_vars$colthemeCol()$ColorReference
    colthemeCol$ColorPoints <- mod_color_vars$colthemeCol()$ColorPoints
    colthemeCol$ColorMemorized <- mod_color_vars$colthemeCol()$ColorMemorized
    colthemeCol$ColorCustomReference <- mod_color_vars$colthemeCol()$ColorCustomReference
  })

  #### Observe: Point color ####
  shiny::observe({
    if(!is.null(scresults_tmp$dat)) {
    shiny::req(input$y)
    new_selected_ids$val
    # input$selectedSG_row_last_clicked
    # input$selectedSG_rows_selected
    # update color if number of factor level is changed
    input$key
    create_color_function <- createColorVector(
      results = scresults_tmp$dat,
      y = input$y,
      point_color = colthemeCol$ColorPoints,
      selected_subgroup = new_selected_ids$val,
      selected_subgroup_color = colthemeCol$ColorTabClicked,
      filter1 = input$filter,
      filter2 = input$filter2,
      variableChosen1 = input$VarChosen,
      variableChosen2 = input$VarChosen2,
      filter_color = colthemeCol$ColorSelected,
      parent_subgroups_color = colthemeCol$ColorParents,
      factorial_context_color = colthemeCol$ColorFactCont,
      importance_values = importance_$val(),
      importance_values_color = colthemeCol$ColorImportance,
      memorized_data = df_m$data,
      memorized_color = colthemeCol$ColorMemorized,
      key = input$key
    )
    plot_color$val <- create_color_function
    }
  })

  # plot_filter_group <-reactiveValues(val = NULL)
  #### Observe (enable tabs) ####
  shiny::observe({
    if(!is.null(scresults_tmp$dat)) {
      shinyjs::js$enableTab("SubscreenExplorer")
      shinyjs::js$enableTab("SubscreenComparer")
      shinyjs::js$enableTab("SubscreenMosaic")
      shinyjs::js$enableTab("SubscreenAsmus")
    }
  })

  #### ObserveEvent: upload_data$parameter2() ####
  shiny::observeEvent(upload_data$parameter2(), {
    variable_importance_tmp$dat <- upload_data$parameter2()
  })


  #### Observe (Interaction) ####
  shiny::observe({
    if (!is.null(scresults_tmp$dat)) {

    #### Output: interaction ####
    output$interaction <- shiny::renderPlot({

      shiny::req(y_axe_Int())
      y_axe <- y_axe_Int()
      df_factorial <- df_factorial()

      # if no subgroup is selected, return a help text
       if (is.null(new_selected_ids$val)) {
        plot(
          NULL,
          xlim = c(0, 1),
          ylim = c(0, 1),
          axes = FALSE,
          xlab = "",
          ylab = ""
        )
        graphics::rect(
          xleft = graphics::grconvertX(0,'ndc','user') - 1000,
          xright = graphics::grconvertX(1,'ndc','user') + 1000,
          ybottom = graphics::grconvertY(0,'ndc','user') - 1000,
          ytop = graphics::grconvertY(1,'ndc','user') + 1000,
          border = NA,
          col = backgroundColor(),
          xpd = TRUE
        )
        graphics::text(
          0.5,
          0.5,
          "Please select a Subgroup!",
          col = font_color(backgroundColor()),
          cex = 1.4
        )
        graphics::text(
          0.5,
          0.4,
          "(Click on a point in the graphic",
          col = font_color(backgroundColor()),
          cex = 0.9
        )
        graphics::text(
          0.5,
          0.3,
          "and then select a subgroup in the",
          col = font_color(backgroundColor()),
          cex = 0.9
        )
        graphics::text(
          0.5,
          0.2,
          "'Selected Subgroup'-table by clicking on)",
          col = font_color(backgroundColor()),
          cex = 0.9
        )

      # if old data (< 4.0.0 subscreencalc) are uploaded
      } else if(!any(startsWith(colnames(scresults_tmp$dat$sge),"FCID_complete_"))
           & ("FCID_complete" %in% colnames(scresults_tmp$dat$sge))){
          plot(
            NULL,
            xlim = c(0, 1),
            ylim = c(0, 1),
            axes = FALSE,
            xlab = "",
            ylab = ""
          )
          graphics::rect(
            xleft = graphics::grconvertX(0,'ndc','user') - 1000,
            xright = graphics::grconvertX(1,'ndc','user') + 1000,
            ybottom = graphics::grconvertY(0,'ndc','user') - 1000,
            ytop = graphics::grconvertY(1,'ndc','user') + 1000,
            border = NA,
            col = backgroundColor(),
            xpd = TRUE
          )
          graphics::text(
            0.5,
            0.5,
           "Please use package version (> 4.0.0)",
            col = font_color(backgroundColor()),
            cex = 1.4
          )
          graphics::text(
            0.5,
            0.4,
            " of subscreencalc to use the Interaction Plot.",
            col = font_color(backgroundColor()),
            cex = 1.4
          )

        # if df_factorial is null
        } else if (is.null(df_factorial)) {

        plot(
          NULL,
          xlim = c(0, 1),
          ylim = c(0, 1),
          axes = FALSE,
          xlab = "",
          ylab = ""
        )
        graphics::rect(
          xleft = graphics::grconvertX(0,'ndc','user') - 1000,
          xright = graphics::grconvertX(1, 'ndc', 'user') + 1000,
          ybottom = graphics::grconvertY(0,'ndc','user') - 1000,
          ytop = graphics::grconvertY(1, 'ndc', 'user') + 1000,
          border = NA,
          col = backgroundColor(),
          xpd = TRUE
        )
        graphics::text(
          0.5,
          0.5,
          "Incomplete factorial context!",
          col = font_color(backgroundColor()),
          cex = 1.4
        )
        graphics::text(
          0.5,
          0.4,
          "(This graphic is not available",
          col = font_color(backgroundColor()),
          cex = 0.9
        )
        graphics::text(
          0.5,
          0.3,
          "for incomplete factorial contexts)",
          col = font_color(backgroundColor()),
          cex = 0.9
        )

      } else if (all(df_factorial$nfactors > 3)) {

        plot(
          NULL,
          xlim = c(0, 1),
          ylim = c(0, 1),
          axes = FALSE,
          xlab = "",
          ylab = ""
        )

        graphics::rect(
          xleft = graphics::grconvertX(0,'ndc','user') - 1000,
          xright = graphics::grconvertX(1, 'ndc', 'user') + 1000,
          ybottom = graphics::grconvertY(0,'ndc','user') - 1000,
          ytop = graphics::grconvertY(1, 'ndc', 'user') + 1000,
          border = NA,
          col = backgroundColor(),
          xpd = TRUE
        )
        graphics::text(
          0.5,
          0.5,
          "Too many factors!",
          col = font_color(backgroundColor()),
          cex = 1.4
        )
        graphics::text(
          0.5,
          0.4,
          "(This graphic is not available",
          col = font_color(backgroundColor()),
          cex = 0.9
        )
        graphics::text(
          0.5,
          0.3,
          "for 4 or more subgroup levels)",
          col = font_color(backgroundColor()),
          cex = 0.9
        )

      } else {

        factor_used <- names(which(apply(df_factorial[,scresults_tmp$dat$factors], 2, function(x){all(x != "Not used")})))

        if (input$plot_type == "log") {
          type <- "y"
        } else {
          type <- ""
        }

        interaction_plot2(
          df_data = df_factorial,
          fac1 = factor_used[1],
          fac2 = factor_used[2],
          fac3 = factor_used[3],
          response = input$y,
          bg.col = backgroundColor(),
          bg.col2 = different_hues(backgroundColor()),
          font.col = font_color(backgroundColor()),
          y.min = y_axe[1],
          y.max = y_axe[2],
          box.col = font_color(backgroundColor()),
          plot_type = type
        )

      }
    })
    }
  })

   output$interactionPanel <- shiny::renderUI({
    shiny::absolutePanel(
      id = "interactionPanel",
      class = "modal-content",
      fixed = TRUE,
      draggable = TRUE,
      shiny::HTML(paste0(
        "<div style='background-color: #424242'>"
      )),
      shiny::HTML('
        <button style =
        "background: #424242;
        color:#ffffff",
        data-toggle="collapse" data-target="#demo" style="color:white;">
        <i class="fa-solid fa-chart-line"></i> Interaction plot </button>'
      ),
      top = 85,
      left = "auto",
      right = 100,
      bottom = "auto",
      width = 600,
      height = "auto",
      shiny::tags$div(
        id = 'demo',
        class = "collapse",
        shiny::fluidRow(
          shiny::uiOutput("interaction_panel")
        )
      ),
      style = "z-index: 10;"
    )
  })

  #### ObserveEvent: input$filter, input$filter2, input$VarChosen, input$VarChosen2, scresults_tmp$dat, input$y,backgroundColor() ####
  #create filtered subgroups table in subgroup explorer-tab
  shiny::observeEvent(c(input$filter, input$filter2, input$VarChosen, input$VarChosen2, scresults_tmp$dat, input$y,backgroundColor()), {
    #requirements
    shiny::req(input$key)
    shiny::req(input$x)
    #create table via createFilteredTable-function
    tmp <- createFilteredTable(
      filter1 = input$filter,
      filter2 = input$filter2,
      variableChosen1 = input$VarChosen,
      variableChosen2 =input$VarChosen2,
      results = scresults_tmp$dat,
      y = input$y,
      x = input$x,
      bg.color = backgroundColor(),
      key = input$key
    )
    # render table with DT
    output$filteredSG <- DT::renderDataTable(tmp)
  })

  if (app_options$showTables) {
   shiny::observeEvent(c(new_selected_ids$val), ignoreNULL = FALSE, {

    if (!is.null(scresults_tmp$dat)) {

      SGID_clicked <- new_selected_ids$val

        if (!is.null(SGID_clicked)) {
          if(!is.integer0(SGID_clicked)) {
            if(!is.na(SGID_clicked)) {
                df_factorial <- scresults_tmp$dat$sge[scresults_tmp$dat$sge$FCID_all == scresults_tmp$dat$sge[scresults_tmp$dat$sge$SGID == new_selected_ids$val,]$FCID_all,]
            } else { df_factorial <- scresults_tmp$dat$sge[0,]}
          } else { df_factorial <- scresults_tmp$dat$sge[0,]}
        } else { df_factorial <- scresults_tmp$dat$sge[0,]}


      if (dim(df_factorial)[1] == 0) {
        empty_data <- scresults_tmp$dat$sge[0,c("SGID", colnames(scresults_tmp$dat$results_total))]
        if (dim(empty_data)[2] > 5) {
          empty_data <- empty_data[,1:5]
        }
        tmp <- DT::datatable(
          data = empty_data,
          extensions = 'Buttons',
          options = list(
            language = list(emptyTable = 'Select a subgroup by clicking on a row in the "Selected Subgroups"-list!'),
            initComplete = DT::JS(
             "function(settings, json) {",
             paste0("$(this.api().table().header()).css({'background-color': '",
                    backgroundColor(),
                    "', 'color': '",
                    font_color(different_hues(backgroundColor())),
                    "'});"
             ),
             "}"
            ),
            dom = 'Brtip',
            buttons = c('copy','print','pageLength',I('colvis')),
            lengthMenu = list(c(6, 12, -1), c("6", "12", "All")),
            pageLength = 6
          ),
          class = 'cell-border stripe',
          rownames = FALSE,
          caption = 'Table of Factorial Contexts',
          filter = 'top'
        )
      } else {

        tmp.sglev <- levels(
          stats::relevel(
            factor(
              unlist(
                lapply(df_factorial[, scresults_tmp$dat$factors], as.character)
              )
            ), ref = 'Not used'
          )
        )
        col.tabFont <- font_color(different_hues(backgroundColor()))
        col.tabBack <- backgroundColor()

        if(input$navpanel == "SubscreenExplorer") {
          curr_x <- shiny::req(input$x)
        } else if (input$navpanel == "SubscreenComparer") {
          curr_x <- shiny::req(input$x2)
        }

        df_fac <- subset(
          df_factorial,
          select = c("SGID", x = curr_x, y = input$y, "nfactors", scresults_tmp$dat$factors)
        )

        colXY <- which(colnames(df_fac) %in% c('SGID', names(scresults_tmp$dat$results_total), 'nfactors'))

        tmp <- DT::datatable(
          data = df_fac,
          extensions = 'Buttons',
          options = list(
            initComplete = DT::JS(
             "function(settings, json) {",
             paste0("$(this.api().table().header()).css({'background-color': '",
                    backgroundColor(),
                    "', 'color': '",
                    font_color(different_hues(backgroundColor())),
                    "'});"
             ),
             "}"
            ),
            dom = 'Brtip',
            buttons = c('copy','print','pageLength',I('colvis')),
            lengthMenu = list(c(6, 12, -1), c("6", "12", "All")),
            pageLength = 6
          ),
          class = 'cell-border stripe',
          rownames = FALSE,
          caption = 'Table of Factorial Contexts',
          filter = 'top'
        )

        tmp <- DT::formatStyle(
          table = tmp,
          columns = colXY,
          color = col.tabFont
        )

        tmp <- DT::formatStyle(
          table = tmp,
          columns = scresults_tmp$dat$factors,
          color = DT::styleEqual(
            tmp.sglev,
            c(col.tabBack, rep(col.tabFont,length(tmp.sglev) - 1))
          )
        )

        tmp <- DT::formatStyle(
          table = tmp,
          columns = seq_len(ncol(df_fac)),
          target = "cell",
          backgroundColor = different_hues(backgroundColor()),
          border = paste0('.5px solid ', backgroundColor())
        )
      }
      output$factorial <- DT::renderDataTable(tmp)

      y_axe_Int <- shiny::reactive({
        shiny::req(input$y_Interaction_Button)

        if (input$y_Interaction_Button == "Synchron") {
          tmp <- c(input$YRange[1], input$YRange[2])
        }
        if (input$y_Interaction_Button == "Optimal") {
          tmp <- c("NA", "NA")
        }
        tmp
      })
    }
  })
}
  #### ObserveEvent: new_selected_ids$val, input$selectedSG_rows_selected, plot_points_data_complement() ####

if (app_options$showTables) {
  shiny::observeEvent(c(new_selected_ids$val,
                        # input$selectedSG_rows_selected,
                        plot_points_data_complement()),{

    if(!is.null(scresults_tmp$dat)) {
      if (shiny::req(input$y) != "N.of.subjects" & !is.null(plot_points_data_complement())) {

       dat <- data.frame(
          plot_points_data_complement()["ID"],
          plot_points_data_complement()["N.of.subjects.complement"],
          plot_points_data_complement()[paste0("Complement_",input$y)]
        )

        tmp <- DT::datatable(
          data = dat,
          extensions = 'Buttons',
          options= list(
            initComplete = DT::JS(
              "function(settings, json) {",
              paste0("$(this.api().table().header()).css({'background-color': '",
                backgroundColor(),
                "', 'color': '",
                font_color(different_hues(backgroundColor())),
                "'});"
              ),
              "}"
            ),
            dom = 'Brtip',
            buttons=c('copy','print','pageLength',I('colvis')),
            lengthMenu = list(c(6, 12, -1), c("6", "12", "All")), pageLength = 6
          ),
          class = 'cell-border stripe', rownames = FALSE,
          caption = 'Table of Complement Subgroup(s)', filter = 'top'
        )
        tmp <- DT::formatStyle(
          table = tmp,
          columns = 1:(ncol(dat) + 1),
          target = "cell",
          backgroundColor = different_hues(backgroundColor()),
          border = paste0('.5px solid ', backgroundColor())
        )
        col.tabFont <- font_color(different_hues(backgroundColor()))
        col.tabBack <- backgroundColor()

        tmp <- DT::formatStyle(
          table = tmp,
          columns = colnames(dat),
          color = col.tabFont
        )
      } else {
         empty_data <- scresults_tmp$dat$sge[0,c("SGID", colnames(scresults_tmp$dat$results_total))]
          if (dim(empty_data)[2] > 5) {
           empty_data <- empty_data[,1:5]
          }
         tmp <- DT::datatable(
          data = empty_data,
          extensions = 'Buttons',
          options = list(
            language = list(emptyTable = 'Select a subgroup by clicking on a row in the "Selected Subgroups"-list!'),
            initComplete = DT::JS(
              "function(settings, json) {",
              paste0("$(this.api().table().header()).css({'background-color': '",
                     backgroundColor(),
                     "', 'color': '",
                     font_color(different_hues(backgroundColor())),
                     "'});"
              ),
              "}"
            ),
            dom = 'Brtip',
            buttons=c('copy','print','pageLength',I('colvis')),
            lengthMenu = list(c(6, 12, -1), c("6", "12", "All")), pageLength = 6
          ),
          class = 'cell-border stripe', rownames = FALSE,
          caption = 'Table of Complement Subgroup(s)', filter = 'top'
        )
      }
      output$complement <- DT::renderDataTable(tmp)
    }
  },ignoreNULL = FALSE)
}
  #### ObserveEvent: scresults_tmp$dat ####
  shiny::observeEvent(scresults_tmp$dat, {
    df_m$data <- NULL
    df_m$data <- shiny::req(scresults_tmp$dat$sge)[0,]
  })

  #### ObserveEvent: input$select_button ####
  shiny::observeEvent(input$select_button, {
    select_button_reac$val <- input$select_button
  })

  #### ObserveEvent: scresults_tmp$dat ####
   shiny::observeEvent(scresults_tmp$dat, {
    select_button_reac$val <- NULL
  })

 #### ObserveEvent: input$remove_button ####
  shiny::observeEvent(c(input$remove_button), {
    selectedRow <- as.numeric(strsplit(input$remove_button, "_")[[1]][2])
    df_m$data <- df_m$data[df_m$data$SGID != selectedRow,]
    selectRow <- NULL
  })

  #### ObserveEvent: select_button_reac$val, scresults_tmp$dat ####
   if (app_options$showTables) {
  shiny::observeEvent(c(select_button_reac$val,scresults_tmp$dat), {
    if (!is.null(shiny::req(select_button_reac$val))){

      selectedRow <- as.numeric(strsplit(select_button_reac$val, "_")[[1]][2])

      del <- cbind(
        data.frame(
          Delete = shinyInput_remove(
            shiny::actionButton,
            1,
            'button_',
            label = "Remove",
            onclick = 'Shiny.onInputChange(\"remove_button\",  this.id)',
            remove_data = select_button_reac$val
          )
        ),
        scresults_tmp$dat$sge[scresults_tmp$dat$sge$SGID == selectedRow, ]
      )
      df_m$data <- rbind(df_m$data, del)
    } else {
      df_m$data <- NULL
    }
  })
   }

  #### ObserveEvent: select_button_reac$val, input$remove_button, scresults_tmp$dat ####
  if (app_options$showTables) {
  shiny::observeEvent(c(select_button_reac$val, input$remove_button, scresults_tmp$dat), {
    if (!is.null(scresults_tmp$dat)) {
      if (dim(df_m$data)[1] == 0) {
        empty_data <- scresults_tmp$dat$sge[0,c("SGID", colnames(scresults_tmp$dat$results_total))]
        if (dim(empty_data)[2] > 5) {
          empty_data <- empty_data[,1:5]
        }
        tmp <- DT::datatable(
        data = empty_data,
        extensions = 'Buttons',
        escape = FALSE,
        selection = 'none',
        options = list(
          language = list(emptyTable = 'Select a subgroup by clicking on a row in the "Selected Subgroups"-list!'),
          initComplete = DT::JS(
            "function(settings, json) {",
            paste0("$(this.api().table().header()).css({'background-color': '",
              backgroundColor(),
              "', 'color': '",
              font_color(different_hues(backgroundColor())),
              "'});"
            ),
            "}"
          ),
          dom = 'Brtip',
          buttons = c('copy', 'print', 'pageLength', I('colvis')),
          lengthMenu = list(c(6, 12, -1), c("6", "12", "All")),
          pageLength = 6
        ),
        class = 'cell-border stripe',
        rownames = FALSE,
        caption = 'Table of Memorized Subgroups',
        filter = 'top'
      )
      } else {

      col2hide <- which(sapply(df_m$data[,-1], FUN = function(x){all(x == 'Not used')})) - 1
      names(col2hide) <- NULL

      tmp <- DT::datatable(
        data = df_m$data,
        extensions = 'Buttons',
        escape = FALSE,
        #selection = 'none',
        options = list(
          initComplete = DT::JS(
            "function(settings, json) {",
            paste0("$(this.api().table().header()).css({'background-color': '",
              backgroundColor(),
              "', 'color': '",
              font_color(different_hues(backgroundColor())),
              "'});"
            ),
            "}"
          ),
          dom = 'Brtip',
          columnDefs = list(list(targets = col2hide, visible = FALSE)),
          buttons = c('copy', 'print', 'pageLength', I('colvis')),
          lengthMenu = list(c(6, 12, -1), c("6", "12", "All")),
          pageLength = 6
        ),
        class = 'cell-border stripe',
        rownames = FALSE,
        caption = 'Table of Memorized Subgroups',
        filter = 'top'
      )

      if (dim(df_m$data)[1] != 0) {

        tmp <- DT::formatStyle(
          table = tmp,
          columns = 1:(ncol(df_m$data[, -1]) + 1),
          target = "cell",
          backgroundColor = different_hues(backgroundColor()),
          border = paste0('.5px solid ',backgroundColor())
        )


        tmp.sglev <- levels(
          stats::relevel(
            factor(
              unlist(
                lapply(df_m$data[, scresults_tmp$dat$factors], as.character)
              )
            ),
            ref = 'Not used'
          )
        )
        colXY <- which(colnames(df_m$data[, -1]) %in% c('SGID', names(scresults_tmp$dat$results_total), 'nfactors')) + 1

        col.tabFont <- font_color(different_hues(backgroundColor()))
        col.tabBack <- backgroundColor()

        tmp <- DT::formatStyle(
          table = tmp,
          columns = names(df_m$data[, -1]),
          color = col.tabFont
        )

        tmp <- DT::formatStyle(
          table = tmp,
          columns = scresults_tmp$dat$factors,
          color = DT::styleEqual(
            tmp.sglev,
            c(col.tabBack, rep(col.tabFont, length(tmp.sglev) - 1))
          )
        )
      }
    }
    #### Output: memorizedSG
    output$memorizedSG <- DT::renderDataTable(tmp)
    }
  }, ignoreNULL = FALSE)
  }

  #### ObserveEvent: new_selected_ids$val, click_points_data$xy ####

if (app_options$showTables) {
  shiny::observeEvent(c(new_selected_ids$val#, click_points_data$xy
                        ), ignoreNULL = FALSE, {
    SGID_clicked <- new_selected_ids$val

    if (!is.null(SGID_clicked)) {
      if (!is.integer0(SGID_clicked)) {
        if (!is.na(SGID_clicked)) {
          df_parent <- parents(scresults_tmp$dat, SGID_clicked)
        } else { df_parent <- NULL}
      } else { df_parent <- NULL}
    } else { df_parent <- NULL}

    tmp <- createParentTable(
      results = scresults_tmp$dat,
      parents = df_parent,
      y = input$y,
      x = input$x,
      x2 = input$x2,
      bg.color = backgroundColor(),
      navpanel = input$navpanel
    )
    #### Output: parents ####
    output$parents<- DT::renderDataTable(tmp)
  })
}
  #### REACTIVES ####
  #### Reactive: plot_points_data_complement() ####
  plot_points_data_complement <- shiny::reactive({
    shiny::req(input$y)
    tmp <- createPlot_points_data_complement(
      results_tmp = scresults_tmp$dat,
      y = input$y,
      sel_ids = new_selected_ids$val
    )
    tmp
  })

  #### Reactive: plot_points_data() ####
  plot_points_data <- shiny::reactive({
    shiny::req(input$x, input$y, input$key)
    data.frame(
      x = scresults_tmp$dat$sge[, c(input$x)][scresults_tmp$dat$sge$nfactors >= input$key[1] & scresults_tmp$dat$sge$nfactors <= input$key[2]],
      y = scresults_tmp$dat$sge[, c(input$y)][scresults_tmp$dat$sge$nfactors >= input$key[1] & scresults_tmp$dat$sge$nfactors <= input$key[2]],
      ID = scresults_tmp$dat$sge[, "SGID"][scresults_tmp$dat$sge$nfactors >= input$key[1] & scresults_tmp$dat$sge$nfactors <= input$key[2]]
    )
  })


  #### Reactive: ref_line() ####
  #reference line value
  ref_line <- shiny::reactive({
    shiny::req(scresults_tmp$dat)
    scresults_tmp$dat$results_total[, c(input$y)]
  })


  #### Reactive: backgroundColor() ####
  backgroundColor <- shiny::reactive({
    ifelse(mod_color_vars$button() == "app version","#383838","#f2f2f2")
  })

  #### Reactive: y_axe_Int() ####
  y_axe_Int <- shiny::reactive({
    shiny::req(input$y_Interaction_Button)

    if (input$y_Interaction_Button == "Synchron") {
      tmp <- c(input$YRange[1], input$YRange[2])
    }
    if (input$y_Interaction_Button == "Optimal") {
      tmp <- c("NA","NA")
    }
    tmp
  })

  #### Reactive: df_factorial() ####
  df_factorial <- shiny::reactive({
    shiny::req(input$y)
    SGID_clicked <- new_selected_ids$val
    if (!is.null(SGID_clicked)) {
      if(!is.integer0(SGID_clicked)) {
        if(!is.na(SGID_clicked)) {
          if(input$y != "N.of.subjects") {
           tmp <- scresults_tmp$dat$sge[scresults_tmp$dat$sge$FCID_all == scresults_tmp$dat$sge[scresults_tmp$dat$sge$SGID == SGID_clicked,]$FCID_all,]
          } else { tmp <- scresults_tmp$dat$sge[0,]}
        } else {tmp <- scresults_tmp$dat$sge[0,]}
      } else { tmp <- scresults_tmp$dat$sge[0,]}
    } else { tmp <- scresults_tmp$dat$sge[0,]}

    if (!any(startsWith(colnames(tmp),"FCID_complete_")) & dim(tmp)[1] != 0) {
      tmp <- pseudo_contexts(tmp, input$y, scresults_tmp$dat$factors)
    } else {df_factorial <- NULL}
    if (dim(tmp)[1] == 0) {
      df_factorial <- NULL
    } else {
    # 1. case: Context complete:
    if (all(tmp[paste0("FCID_incomplete_", input$y)] == "Complete")) {
      df_factorial <- tmp
    }
    #2. case: Context incomplete:
    if (all(tmp[paste0("FCID_incomplete_", input$y)] == "Incomplete")) {
      df_factorial <- NULL
    }
    #3. case: Context pseudo complete
      if (all(tmp[paste0("FCID_incomplete_", input$y)] == "Pseudo complete")) {
        df_factorial <- tmp[tmp[paste0("FCID_pseudo_", input$y)] != "No Pseudo",]
      }
    }

    df_factorial
  })

  #### Reactive: vi_names() ####
  vi_names <- shiny::reactive({
    if (is.data.frame(variable_importance_tmp$dat)) {
      "NULL"
    } else if (is.list(variable_importance_tmp$dat)) {
      names(variable_importance_tmp$dat)
    } else  {
      "NULL"
    }
  })

  #### REACTIVEVALUES ####
  #### ReactiveValues: scresults_tmp$dat ####
  #SubscreenResult object
  scresults_tmp <- shiny::reactiveValues(
    dat = app_options$scresults
  )

  #### ReactiveValues: selected_ids$val ####
  selected_ids <- shiny::reactiveValues(val = NULL)

  #### ReactiveValues: pare$val ####
  # pare <- shiny::reactiveValues(val = NULL)

  #### ReactiveValues: new_selected_ids ####
  new_selected_ids <- shiny::reactiveValues(val = NULL)

  #### ReactiveValues: click_points_data$xy ####
  click_points_data <- shiny::reactiveValues(xy = data.frame(x = NULL, y = NULL))


  #### ReactiveValues: variable_importance_tmp$dat ####
  variable_importance_tmp <- shiny::reactiveValues(
    dat = app_options$variable_importance
  )

  #### ReactiveValues: colthemeCol$... ####
  colthemeCol <- shiny::reactiveValues(
    font.col = '#ffffff',
    panel.col = '#6b6b6b',
    # ColorClicked = "#D30F4B",
    ColorSelected = "#89D329",
    ColorParents = "#ff6c00",
    ColorTabClicked = "#e2b007",
    ColorImportance = "#FA1BDC",
    ColorReference = "#0091DF60",
    ColorFactCont = "#0350E0",
    ColorPoints = "#FFFFFF",
    ColorCustomReference = "#00BCFFFF"
  )

  #### ReactiveValues: select_button_reac$val ####
  select_button_reac <- shiny::reactiveValues(val = NULL)

  #### ReactiveValues: plot_color$val ####
  plot_color <- shiny::reactiveValues(val = NULL)

  #### ReactiveValues: df_m$data ####
  # memorized data frame
  df_m <- shiny::reactiveValues(data = NULL)

  #### ReactiveValues: df_parent$data ####
  # reactive parent subgroup data frame (start value: NULL)
  df_parent <- shiny::reactiveValues(data = data.frame(NULL))

  #### Reactive: logofile ####
  logofile <- shiny::reactive({
   ifelse(mod_color_vars$button() == "app version","www/images/subscreen_logo.png","www/images/subscreen_logo_inverted.png")
  })

  #### OUTPUTS ####
  #### Output: interaction_panel ####
  output$interaction_panel <- shiny::renderUI({
    shiny::wellPanel(
      style = paste0("background:" , backgroundColor()),
      shiny::fluidRow(
        shiny::plotOutput(outputId = 'interaction')
      ),
      shiny::fluidRow(
        col_12(
          shiny::radioButtons(
            inputId = 'y_Interaction_Button',
            label = 'Synchronise y-axes with main plot',
            selected = ("Synchron"),
            choices = c("Synchron","Optimal"),
            inline = TRUE
          )
        )
      )
    )
  })

  #### Output: VarChosen ####
  output$VarChosen <- shiny::renderUI({
    if (input$filter != 'no selection') {
      choices <- c(as.character(unique(scresults_tmp$dat$sge[, input$filter])))
      choices <- choices[-which(choices == "Not used")]
      selected <- choices[1]
      shiny::selectInput(
        inputId = "VarChosen",
        label = "Choose a value (1)",
        choices = choices,
        selected = selected
      )
    }
  })

  #### Output: VarChosen2 ####
  output$VarChosen2 <- shiny::renderUI({
    if (input$filter2 != 'no selection') {
      choices <- c(as.character(unique(scresults_tmp$dat$sge[, input$filter2])))
      choices <- choices[-which(choices == "Not used")]
      selected <- choices[1]
      shiny::selectInput(
        inputId = "VarChosen2",
        label = "Choose a value (2)",
        choices = choices,
        selected = selected
      )
    }
  })

  #### Output: YRange ####
  output$YRange <- shiny::renderUI({
    shiny::req(input$y)
     if (input$plot_type == "lin") {

      shiny::sliderInput(
        inputId = "YRange",
        label = "Y Range",
        min = roundDownNice(min(scresults_tmp$dat$sge[, input$y], na.rm = TRUE), nice = app_options$nice_numbers),
        max = roundUpNice(max(scresults_tmp$dat$sge[, input$y], na.rm = TRUE), nice = app_options$nice_numbers),
        value = c(roundDownNice(min(scresults_tmp$dat$sge[, input$y], na.rm = TRUE), nice = app_options$nice_numbers), roundUpNice(max(scresults_tmp$dat$sge[, input$y], na.rm = TRUE), nice = app_options$nice_numbers)),
        step = roundUpNice((max(scresults_tmp$dat$sge[, input$y], na.rm = TRUE) - min(scresults_tmp$dat$sge[, input$y], na.rm = TRUE))/100, nice = app_options$nice_numbers)
      )
     } else {
      rg.z <- log(
        range(
          roundDownNice(
            min(scresults_tmp$dat$sge[, input$y], na.rm = TRUE), nice = app_options$nice_numbers
          ),
          roundUpNice(
            max(scresults_tmp$dat$sge[, input$y], na.rm = TRUE), nice = app_options$nice_numbers
          )
        )
      )
      choices <- unique(unlist(lapply(exp(seq(rg.z[1], rg.z[2], length.out = 20)), function(x){signif(x, 2)})))
      shinyWidgets::sliderTextInput(
        inputId = "YRange",
        label = "Log Y Range:",
        hide_min_max = TRUE,
        choices = choices,
        selected = c(choices[1],choices[length(choices)]),
        grid = TRUE
      )
     }
  })

  #### Output: XRange ####
  output$XRange <- shiny::renderUI({
    shiny::req(input$x)
      mini <- roundDownNice(min(scresults_tmp$dat$sge[, input$x], na.rm = TRUE) ,nice = app_options$nice_numbers)
      maxi <- roundUpNice(max(scresults_tmp$dat$sge[, input$x], na.rm = TRUE) ,nice = app_options$nice_numbers)
      twentyPercent <- roundUpNice(abs(diff(c(maxi,mini))/5), nice = app_options$nice_numbers)
      mini_20percent <- mini - twentyPercent
      maxi_20percent <- maxi + twentyPercent
      shiny::sliderInput(
        inputId = "XRange",
        label = "X Range",
        min = mini_20percent,
        max = maxi_20percent,
        value = c(mini, maxi),
        step = 1#roundUpNice(diff(c(mini,maxi))/100,nice = app_options$nice_numbers)
      )
  })

  #### Output: Logo file ####
  output$logofile <- shiny::renderUI({
    shiny::img(
      src = logofile(),
      style = "margin-top: 3px; padding-right:10px;padding-bottom:10px",
      height = 55
    )
  })

  #### Output: Importance Availability ####
  output$importanceenabled <- shiny::reactive({
    !is.null(variable_importance_tmp$dat)
  })
  shiny::outputOptions(output, "importanceenabled", suspendWhenHidden = FALSE)

  #### Output: Funnel Availability ####
  output$funnelenabled <- shiny::reactive({
    !is.null(scresults_tmp$dat$funnel_quantiles)
  })
  shiny::outputOptions(output, "funnelenabled", suspendWhenHidden = FALSE)

  shiny::observeEvent(scresults_tmp$dat, {
    if (!is.null(scresults_tmp$dat$funnel_quantiles)) {
      val <- sort(2*unique(scresults_tmp$dat$funnel_quantiles[,"alpha"][scresults_tmp$dat$funnel_quantiles[,"alpha"] < 0.5]))

      shiny::updateCheckboxInput(
        inputId = "add_funnel",
        label = "Draw reference funnel",
        value = app_options$add_funnel_at_start
      )

      shiny::updateRadioButtons(
        session,
        inputId = "alpha_funnel",
        choices = val,
        selected = val[1],
        inline = TRUE
      )
    }
  })

  #### MODULES ####
  #### Module call: graph_server ####
  # 1.Explorer
  add_custom_line <- shiny::reactiveValues(val = FALSE)
  shiny::observeEvent(input$add_custom_ref_line,{
   add_custom_line$val <- input$add_custom_ref_line
  })

  mod_graph_vars1 <- shiny::callModule(
      mod_graph_server,
      "graph1",
      results = shiny::reactive({scresults_tmp$dat}),
      plot_point = shiny::reactive({shiny::req(plot_points_data())}),
      YRange = shiny::reactive({input$YRange}),
      XRange = shiny::reactive({input$XRange}),
      plot_type = shiny::reactive({input$plot_type}),
      point_size = shiny::reactive({input$pointsize}),
      #pch_value = shiny::reactive({input$pch_value}),
      LabelParent = shiny::reactive({mod_color_vars$LabelParent()}),
      LabelMemorized = shiny::reactive({mod_color_vars$LabelMemorized()}),
      LabelTabClicked = shiny::reactive({mod_color_vars$LabelTabClicked()}),
      LabelFactCont = shiny::reactive({mod_color_vars$LabelFactCont()}),
      color = shiny::reactive({plot_color$val}),
      ColorBGplot = shiny::reactive({backgroundColor()}),
      ColorTabClicked = shiny::reactive({colthemeCol$ColorTabClicked}),
      ColorPoints = shiny::reactive({colthemeCol$ColorPoints}),
      ColorReference = shiny::reactive({colthemeCol$ColorReference}),
      ColorCustomReference = shiny::reactive({colthemeCol$ColorCustomReference}),
      ColorMemorized = shiny::reactive({colthemeCol$ColorMemorized}),
      ColorFactorial = shiny::reactive({colthemeCol$ColorFactCont}),
      ColorParents = shiny::reactive({colthemeCol$ColorParents}),
      x = shiny::reactive({input$x}),
      y = shiny::reactive({input$y}),
      plot_points_data_complement = shiny::reactive({plot_points_data_complement()}),
      key = shiny::reactive({input$key}),
      nice_Numbers = app_options$nice_numbers,
      xlabel = shiny::reactive({input$xlabel}),
      grid = shiny::reactive({input$grid}),
      circlestyle = shiny::reactive({input$circlestyle}),
      memorized_Data = shiny::reactive({df_m$data}),
      memorized_labels_on_off = shiny::reactive({input$memorized_labels_on_off}),
      subTitle = app_options$graphSubtitle,
      remove_levels = shiny::reactive({input$remove_levels}),
      show_ref_line = shiny::reactive({input$add_ref_line}),
      add_custom_ref_line  = shiny::reactive({add_custom_line$val}),
      value_custom_ref_line  = shiny::reactive({input$custom_ref_line}),
      show_favour_arrows = shiny::reactive({input$add_favour_arrows}),
      favour_direction = shiny::reactive({input$favour_direction}),
      favour_verum_name = app_options$favour_label_verum_name,
      favour_comparator_name = app_options$favour_label_comparator_name,
      add_funnel = shiny::reactive({input$add_funnel}),
      exclude_funnel = shiny::reactive({input$exclude_funnel}),
      alpha_funnel = shiny::reactive({input$alpha_funnel})
  )

  #### Module call: legend_server ####
  shiny::observe({
    shiny::callModule(
      mod_legend_server,
      "legend1",
      plot_color = shiny::reactive({plot_color$val}),
      rowwise = TRUE,
      colthemeCol = shiny::reactive({colthemeCol}),
      complement = shiny::reactive({ifelse(!is.null(plot_points_data_complement()),TRUE,FALSE)})
    )
  })

  #### Module call: color_server ####
  mod_color_vars <- shiny::callModule(
    mod_color_server,
    "color"
  )

  #### Module call: variable_importance_server ####
  importance_ <- shiny::callModule(
    mod_variable_importance_server,
    "vi",
    variable_importance = shiny::reactive({variable_importance_tmp$dat}),
    results = shiny::reactive({scresults_tmp$dat})
  )

  #### Module call: comparer_server ####
  # 2. Comparer
  mod_comparer_vars <- shiny::callModule(
    mod_comparer_server,
    "comparer",
    results = shiny::reactive({scresults_tmp$dat}),
    YRange = shiny::reactive({input$YRange}),
    XRange = shiny::reactive({input$XRange}),
    plot_type = shiny::reactive({input$plot_type}),
    point_size = shiny::reactive({input$pointsize}),
    #pch_value = shiny::reactive({input$pch_value}),
    color = shiny::reactive({plot_color$val}),
    ColorFactorial = shiny::reactive({colthemeCol$ColorFactCont}),
    LabelParent = shiny::reactive({mod_color_vars$LabelParent()}),
    LabelMemorized = shiny::reactive({mod_color_vars$LabelMemorized()}),
    LabelTabClicked = shiny::reactive({mod_color_vars$LabelTabClicked()}),
    LabelFactCont = shiny::reactive({mod_color_vars$LabelFactCont()}),
    ColorParents = shiny::reactive({colthemeCol$ColorParents}),
    colthemeCol = shiny::reactive({colthemeCol}),
    ColorBGplot = shiny::reactive({backgroundColor()}),
    ColorTabClicked = shiny::reactive({colthemeCol$ColorTabClicked}),
    ColorReference = shiny::reactive({colthemeCol$ColorReference}),
    ColorCustomReference = shiny::reactive({colthemeCol$ColorCustomReference}),
    ColorPoints = shiny::reactive({colthemeCol$ColorPoints}),
    ColorMemorized = shiny::reactive({colthemeCol$ColorMemorized}),
    x = shiny::reactive({input$x}),
    y = shiny::reactive({input$y}),
    plot_points_data_complement = shiny::reactive({plot_points_data_complement()}),
    key = shiny::reactive({input$key}),
    nice_Numbers = app_options$nice_numbers,
    xlabel = shiny::reactive({input$xlabel}),
    grid = shiny::reactive({input$grid}),
    circlestyle = shiny::reactive({input$circlestyle}),
    memorized_Data = shiny::reactive({df_m$data}),
    #point_brightness = shiny::reactive({input$point_brightness}),
    show_ref_line = shiny::reactive({input$add_ref_line}),
    add_custom_ref_line  = shiny::reactive({add_custom_line$val}),
    value_custom_ref_line  = shiny::reactive({input$custom_ref_line}),
    show_favour_arrows = shiny::reactive({input$add_favour_arrows}),
    favour_direction = shiny::reactive({input$favour_direction}),
    favour_verum_name = app_options$favour_label_verum_name,
    favour_comparator_name = app_options$favour_label_comparator_name,
    add_funnel = shiny::reactive({input$add_funnel}),
    exclude_funnel = shiny::reactive({input$exclude_funnel}),
    alpha_funnel = shiny::reactive({input$alpha_funnel})
  )

  #### Module call: mosaic_server ####
  # 3. Mosaic
  shiny::callModule(
    mod_mosaic_server,
    "mosaic",
    results = shiny::reactive({scresults_tmp$dat}),
    ColorBGplot = shiny::reactive({backgroundColor()}),
    nice_Numbers = app_options$nice_numbers
  )

  #### Module call: asmus2_module_server ####
  # 4. ASMUS
  shiny::callModule(
    asmus2_module_server,
    "asmus2",
    results = shiny::reactive({scresults_tmp$dat}),
    ColorReference = colthemeCol$ColorReference,
    ColorBGplot = shiny::reactive({backgroundColor()}),
    ColorPoints = shiny::reactive({colthemeCol$ColorPoints}),
    nice_Numbers = app_options$nice_numbers
  )

  #### Module call: upload_tab_server ####
  # 5.Upload
  upload_data <- shiny::callModule(
    upload_tab_server,
    "upload_tab_ui_1",
    dat = app_options$scresults,
    dat_name = app_options$scresults_name,
    vi = app_options$variable_importance,
    font_col = shiny::reactive({ifelse(mod_color_vars$button() == "app version","#f2f2f2","#383838")})
  )
}
