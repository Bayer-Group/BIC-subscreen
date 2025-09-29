#' mosaic UI Function
#'
#' @description A shiny Module for the Mosaic Plot in Subscreen.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_mosaic_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidPage(
      shiny::fluidRow(
          col_3(
          # Option and variable panel
          shiny::uiOutput(ns('PanelMosaic'))
        ),
        col_8(
          shiny::div(style = "position:relative",
            shiny::uiOutput(ns('helptext_mosaic')),
            # Mosaic plot

            shiny::plotOutput(
              outputId = ns("mosaic"),
              # hover options
              hover = shiny::hoverOpts(id = ns('plot_hover'), delay = 200, delayType = 'debounce'),
              height = 600,
              width = 1000
            ),
            shiny::uiOutput(ns("hover_info"))
          )
        )
      )
    )
  )
}

#' mosaic Server Function
#'
#' @noRd
mod_mosaic_server <- function(
    input, output, session,
    results,
    ColorBGplot,
    nice_Numbers
) {

  ns <- session$ns

  output$PanelMosaic <- shiny::renderUI({

    choi <- setdiff(
      setdiff(names(results()$results_total[ ,!is.na(results()$results_total)]),'N.of.subjects'),
      names(which(apply(results()$sge[,names(results()$results_total)],2,function(x) {(!all(is.finite(x[!is.na(x)])))})))
    )

    shiny::wellPanel(
      shiny::selectInput(
        inputId = ns("var1"),
        label = "First subgroup variable (x)",
        choices = results()$factors,
        selected = results()$factors[1]
      ),

      shiny::conditionalPanel(condition = paste0('output[\'', ns('show_var21'), "\'] == true"),
        shiny::selectInput(
          inputId = ns("var2"),
          label = "Second subgroup variable (y)",
          choices = c('no selection', results()$factors),
          selected = 'no selection'
        )
      ),
      shiny::conditionalPanel(condition = paste0('output[\'', ns('show_var22'), "\'] == true"),
        shiny::selectInput(
          inputId = ns("var22"),
          label = "Third subgroup variable (y2)",
          choices = c('no selection', results()$factors),
          selected = 'no selection'
        )
      ),
      shiny::selectInput(
        inputId = ns("var3"),
        label = "Reference variable (color)",
        choices = choi,
        selected = input$y
      ),
      shiny::radioButtons(
        inputId = ns("logmosaic"),
        label = "Type",
        choices = c(linear = "lin", log = "log"),
        selected = "lin",
        inline = TRUE
      ),
      "Use mouse hover to get further information about the subgroup(s)!"
    )
  })

  shiny::observeEvent(c(input$var1, input$var22), {
    shiny::updateSelectInput(
      inputId ="var2",
      selected = input$var2,
      choices = c('no selection', results()$factors[!c(results()$factors) %in% c(input$var1, input$var22)])
    )
  })

  shiny::observeEvent(c(input$var1, input$var2), {
    shiny::updateSelectInput(
      inputId ="var22",
      selected = input$var22,
      choices = c('no selection', results()$factors[!c(results()$factors) %in% c(input$var1, input$var2)])
    )
  })

  shiny::observeEvent(c(input$var2, input$var22), {
    shiny::updateSelectInput(
      inputId ="var1",
      selected = input$var1,
      choices = c(results()$factors[!c(results()$factors) %in% c(input$var2,input$var22)])
    )
  })


  show_var21_val <- shiny::reactiveValues(val = FALSE)
  output$show_var21 <- shiny::reactive({
    show_var21_val$val
  })
  shiny::observeEvent(results()$max_comb,{
    if (results()$max_comb > 1) {
      show_var21_val$val <- TRUE
    } else {
    shiny::updateSelectInput(
      inputId ="var21",
      selected = 'no selection'
    )
      show_var21_val$val <- FALSE
    }
  })
  shiny::outputOptions(output, "show_var21", suspendWhenHidden = FALSE)


  show_var22_val <- shiny::reactiveValues(val = FALSE)

  output$show_var22 <- shiny::reactive({
    show_var22_val$val
  })

  shiny::observeEvent(input$var2, {
    if (input$var2 != "no selection" & results()$max_comb > 2) {
      show_var22_val$val <- TRUE
    } else {
    shiny::updateSelectInput(
      inputId ="var22",
      selected = 'no selection'
    )
      show_var22_val$val <- FALSE
    }
  })

  shiny::outputOptions(output, "show_var22", suspendWhenHidden = FALSE)

  #### renderPlot mosaic ####
  output$mosaic <- shiny::renderPlot({
    shiny::req(results())
    # use subscreen_mosaicPlot to draw mosaic plot
    if (!is.null(results())) {
      if (shiny::req(results())$min_comb > 1) {
        output$helptext_mosaic <- shiny::renderUI({
          shiny::HTML("<p style ='color:#DE0043'> Please set parameter min_comb in subscreencalc to 1 to use the mosaic plot.</p>")
        })
      }
    }
    if (results()$min_comb == 1) {
      subscreen_mosaicPlot(
        res = results(),
        mos.x = shiny::req(input$var1),
        mos.y = shiny::req(input$var2),
        mos.y2 = shiny::req(input$var22),
        mos.z = shiny::req(input$var3),
        col.bg = ColorBGplot(),
        col.txt = font_color(ColorBGplot()),
        #colrange.z = c("#443247FF","#00BCFFFF","white","#fad56eFF","#004422FF"),
        colrange.z = c("#10384FFF","#00BCFFFF","white","#89D329FF","#004422FF"),
        scale = input$logmosaic
      )
    }
  }, bg = "transparent")

  hoverlabel <- shiny::reactiveValues(value = NULL)

  shiny::observeEvent(c(input$plot_hover$x, input$plot_hover$y, input$var1, input$var2, input$var22,
    input$var3), ignoreNULL = FALSE, {
    shiny::req(results())
    if (results()$min_comb == 1) {
      if (!is.null(input$plot_hover$x) & !is.null(input$plot_hover$y)) {

      mos.x <- shiny::req(input$var1)
      mos.y <- shiny::req(input$var2)
      mos.y2 <- shiny::req(input$var22)
      mos.z <- shiny::req(input$var3)
      col.bg <- ColorBGplot()
      col.txt <- font_color(ColorBGplot())
      colrange.z <- c("#10384FFF","#00BCFFFF","white","#89D329FF","#004422FF")
      #colrange.z <- c("#443247FF","#00BCFFFF","white","#fad56eFF","#004422FF")
      not.used <- 'Not used'

      if (mos.y == 'no selection') {
        mos.y <- NULL
      }
      if (mos.y2 == 'no selection' | is.null(mos.y)) {
        mos.y2 <- NULL
      }
      if (!is.null(mos.y)) {
        if (mos.x == mos.y) {
          mos.y <- NULL
        }
      }
      if (!is.null(mos.y2)) {
        if (mos.x == mos.y2 | mos.y == mos.y2) {
          mos.y2 <- NULL
        }
      }

      res <- results()$sge
      not.used <- 'Not used'
      tmp_x <- res[res$nfactors == 1 & !res[, mos.x] %in% not.used, ]
      tmp_x2 <- dplyr::arrange(tmp_x, !!rlang::sym(mos.x))
      prop.x <- cumsum(tmp_x2[, 'N.of.subjects'])
      prop.x <- c(0,prop.x) / max(prop.x)
      mid.x <- (prop.x[-length(prop.x)] + prop.x[-1])/2
      names(mid.x) <- paste0(mos.x, ' = ', tmp_x2[, mos.x])
      hov.x <- as.character(tmp_x2[, mos.x])
      prop.y <- c(0, 1)
      mid.y <- 0.5

      if (!is.null(mos.y)) {
        dim_x <- dim(tmp_x)[1]
        tmp_y_1 <- res[res$nfactors == 1 & !res[, mos.y] %in% not.used, ]
        dim_y <- dim(tmp_y_1)[1]
        tmp_y <- res[res$nfactors == 2 & !res[, mos.y] %in% not.used &
                                 !res[, mos.x] %in% not.used, ]

        tmp_y <- dplyr::arrange(tmp_y, !!!rlang::syms(c(mos.x, mos.y)))
        expected_tmp_y <- expand.grid(lapply(lapply(
          tmp_y %>% dplyr::select(dplyr::all_of(c(mos.x,mos.y))) ,levels),function(x){x[x != "Not used"]}))

        if(dim(tmp_y)[1] !=  dim(expected_tmp_y)[1]){
          expected_tmp_2 <- expected_tmp_y %>%
            dplyr::mutate(
              FCID_all = unique(tmp_y$FCID_all),
              max_level = unique(tmp_y$max_level),
              nfactors = unique(tmp_y$nfactors)
            )
          tmp_y <- tmp_y %>% dplyr::right_join(expected_tmp_2, by = c(colnames(expected_tmp_y),"FCID_all","max_level","nfactors"))
          tmp_y <- dplyr::arrange(tmp_y, !!!rlang::syms(c(mos.x, mos.y)))
        }
        prop.y <- plyr::ddply(tmp_y,mos.y,function(x){x$N.of.subjects})[,-1]
        prop.y[is.na(prop.y)] <- 0
        prop.y <- apply(prop.y,2,cumsum)
        prop.y <- apply(prop.y,2,function(x) {c(0,x)/ max(x)})
        mid.y <- apply(prop.y,2, function(x) {(x[-length(x)] + x[-1])/2})
        rownames(mid.y) <- unique(paste0(mos.y, ' = ',tmp_y[, mos.y]))
        hov.y <- tmp_y[, c(mos.y)]

        if (!is.null(mos.y2)) {

        tmp_y_1 <- res[res$nfactors == 2 & !res[, mos.y] %in% not.used &
                                 !res[, mos.y2] %in% not.used, ]
        dim_y <- dim(tmp_y_1)[1]
        tmp_y <- res[res$nfactors == 3 & !res[, mos.x] %in% not.used & !res[, mos.y] %in% not.used &
                                 !res[, mos.y2] %in% not.used, ]

        tmp_y <- dplyr::arrange(tmp_y, !!!rlang::syms(c(mos.y, mos.y2)))
        expected_tmp_y <- expand.grid(lapply(lapply(
          tmp_y %>% dplyr::select(dplyr::all_of(c(mos.x,mos.y,mos.y2))) ,levels),function(x){x[x != "Not used"]}))

        if(dim(tmp_y)[1] !=  dim(expected_tmp_y)[1]){
          expected_tmp_2 <- expected_tmp_y %>%
            dplyr::mutate(
              FCID_all = unique(tmp_y$FCID_all),
              max_level = unique(tmp_y$max_level),
              nfactors = unique(tmp_y$nfactors)
            )
          tmp_y <- tmp_y %>% dplyr::right_join(expected_tmp_2, by = c(colnames(expected_tmp_y),"FCID_all","max_level","nfactors"))
          tmp_y$N.of.subjects[is.na(tmp_y$N.of.subjects)] <- 0
        }

        tmp_y <- dplyr::arrange(tmp_y, !!!rlang::syms(c(mos.y, mos.y2)))

        prop.y <- t(plyr::ddply(tmp_y,c(mos.x),function(x){x$N.of.subjects})[,-1])
        prop.y[is.na(prop.y)] <- 0
        prop.y <- apply(prop.y,2,cumsum)
        prop.y <- apply(prop.y,2,function(x) {c(0,x)/ max(x)})
        hov.y <- tmp_y[, c(mos.y, mos.y2)]
        mid.y <- apply(prop.y,2, function(x) {(x[-length(x)] + x[-1])/2})
        rownames(mid.y) <- unique(paste0(mos.y, ' = ', tmp_y[, mos.y], ' & ', mos.y2, ' = ', tmp_y[,mos.y2]))
        }
      }
      if (shiny::req(input$logmosaic) == "lin") {
        rg.z <- range(res[, mos.z], na.rm = TRUE)
      }
      if (shiny::req(input$logmosaic) == "log") {
        rg.z <- log(
          range(
            res[, mos.z], na.rm = TRUE
          )
        )
      }

      if (is.null(mos.y)) {
        tmp_1factors <- tmp_x
      } else {
        if (is.null(mos.y2)) {
          tmp_2factors <- res[res$nfactors == 2 & !res[, mos.x] %in% not.used & !res[, mos.y] %in% not.used,]
        } else {
          tmp_3factors <- res[res$nfactors == 3 & !res[, mos.x] %in% not.used &
                                 !res[, mos.y] %in% not.used & !res[, mos.y2] %in% not.used, ]
          tmp_3factors <- dplyr::arrange(tmp_3factors, !!!rlang::syms(c(mos.x,mos.y,mos.y2)))
        }
      }

        if (!is.null(mos.y2)) {
         tmp <- results()$sge[results()$sge$nfactors == 3 & !results()$sge[, mos.x] %in% not.used &
                              !results()$sge[, mos.y] %in% not.used & !results()$sge[, mos.y2] %in% not.used, ]
         tmp <- dplyr::arrange(tmp, !!!rlang::syms(c(mos.x,mos.y,mos.y2)))
        } else if (!is.null(mos.y)) {
          tmp <- res[res$nfactors == 2 & !res[, mos.x] %in% not.used & !res[, mos.y] %in% not.used,]

        } else {
          tmp <- res[res$nfactors == 1 & !res[, mos.x] %in% not.used, ]
        }

      if (!rg.z[1] < results()$results_total[,mos.z]) {
        rg.z[1] <- results()$results_total[,mos.z] - (results()$results_total[,mos.z]/1000)
      }
      if (!rg.z[2] > results()$results_total[,mos.z]) {
        rg.z[2] <- results()$results_total[,mos.z] + (results()$results_total[,mos.z]/1000)
      }

      mean.z <- ifelse(shiny::req(input$logmosaic) == "lin",
                       results()$results_total[,mos.z],
                       log(results()$results_total[,mos.z]))
      tr.mean.z <- (mean.z-rg.z[1])/diff(rg.z)

      col.disp <- c("SGID",mos.z, mos.x, mos.y, mos.y2, "N.of.subjects")

        if (is.null(mos.y)) {
          tmp2 <- tmp_x2[hov.x == (hov.x[cut(input$plot_hover$x, prop.x, labels = FALSE)]), col.disp]

          hoverlabel$value <- tmp2
        } else {

          if (is.null(mos.y2)) {
            hoverlabel$value <- tmp[tmp[,mos.x] == (hov.x[cut(input$plot_hover$x, prop.x, labels = FALSE)]) &
                                      tmp[,mos.y] == (hov.y[cut(input$plot_hover$y, prop.y[,cut(input$plot_hover$x, prop.x, labels = FALSE)]+(1:dim(prop.y)[1])*0.0000001, labels = FALSE)]),col.disp]
          } else {
            tmp2 <- tmp[tmp[,mos.x] == (hov.x[cut(input$plot_hover$x, prop.x, labels = FALSE)]),col.disp]
            tmp3 <-  tmp2[cut(input$plot_hover$y, unique(prop.y[,cut(input$plot_hover$x, prop.x, labels = FALSE)]), labels = FALSE),]
            hoverlabel$value <- tmp3[,col.disp]
          }
        }
        hoverlabel$value <- hoverlabel$value[, !startsWith(colnames(hoverlabel$value), "FCID_")]
        hoverlabel$value <- hoverlabel$value[, !startsWith(colnames(hoverlabel$value), "Complement_")]
      }
    }
  })

  output$hover_info <- shiny::renderUI({
    shiny::req(input$plot_hover, hoverlabel$value)
    val.z.ij <- NA

    input$plot_hover
    hover <- input$plot_hover
    hover$mapping <- list(xintercept = "xintercept", x = "x", y = "y")
    #colrange.z = c("#443247FF","#00BCFFFF","white","#fad56eFF","#004422FF")
    colrange.z = c("#10384FFF","#00BCFFFF","white","#89D329FF","#004422FF")
    if (shiny::req(input$logmosaic) == "lin") {
      rg.z <- range(results()$sge[, input$var3], na.rm = TRUE)
    }
    if (shiny::req(input$logmosaic) == "log") {
      rg.z <- log(
        range(
          results()$sge[, input$var3], na.rm = TRUE
        )
      )
    }

    if (!rg.z[1] < results()$results_total[,input$var3]) {
      rg.z[1] <- results()$results_total[,input$var3] - (results()$results_total[,input$var3]/1000)
    }
    if (!rg.z[2] > results()$results_total[,input$var3]) {
      rg.z[2] <- results()$results_total[,input$var3] + (results()$results_total[,input$var3]/1000)
    }
    mean.z <- ifelse(shiny::req(input$logmosaic) == "lin",
                     mean.z <- results()$results_total[,input$var3],
                     log(results()$results_total[,input$var3]))
    tr.mean.z <- (mean.z-rg.z[1])/diff(rg.z)

    f_colZ <- grDevices::colorRamp(colrange.z, bias = log(tr.mean.z, base = 0.5))

    if (input$var3 %in% colnames(hoverlabel$value)) {
      val.z.ij <- hoverlabel$value[input$var3]

    if (shiny::req(input$logmosaic) == "log") {
     val.z.ij <- log(hoverlabel$value[input$var3])
    }

    if (dim(val.z.ij)[1] > 0 & !is.na(as.numeric(val.z.ij))) {

      hoverColor <- grDevices::rgb(f_colZ((val.z.ij - rg.z[1])/diff(rg.z)), maxColorValue = 255)

      left_px <- hover$coords_img$x
      top_px <- hover$coords_img$y

      style <- paste0("position:absolute; z-index:100; background-color: rgba(",
        grDevices::col2rgb(hoverColor)[1],",",grDevices::col2rgb(hoverColor)[2],",",grDevices::col2rgb(hoverColor)[3],",0.95); ",
                      "left:", left_px, "px; top:", top_px, "px; border-width: 1px; border-color: #424242;")

      shiny::wellPanel(
       style = style,
       shiny::p(
          shiny::HTML(
            paste0(
              "<b style = 'color: black;'>",
              paste(
                paste0(
                  colnames(hoverlabel$value),": ", data.frame(lapply(hoverlabel$value, as.character), stringsAsFactors=FALSE)
                ), collapse = "</br>"
              ),
              "</b>"
            )
          )
        )
      )
    }
    }
  })

  shiny::observeEvent(input$var3, {
    if (roundDownNice(min(results()$sge[, input$var3], na.rm = TRUE), nice = nice_Numbers) <= 0) {
      shiny::updateRadioButtons(
        inputId = "logmosaic",
        label = "Type",
        choices = c(linear = "lin"),
        selected = "lin",
        inline = TRUE
      )
    } else {
      shiny::updateRadioButtons(
        inputId = "logmosaic",
        label = "Type",
        choices = c(linear = "lin", log = "log"),
        selected = "lin",
        inline = TRUE
      )
    }
  })
}

