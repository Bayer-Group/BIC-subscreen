#' bubble UI Function
#'
#' @description A shiny Module for the bubble plot in subscreen.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
mod_bubble_ui <- function(id, plotHeight, plotWidth) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(style = "position:relative",
      shiny::plotOutput(
        outputId = ns("bubble"),
        click = ns("plot_click"),
        hover = shiny::hoverOpts(
          ns("plot_hover"),
          delay = 300,
          delayType = "debounce"
        ),
        height = plotHeight,
        width = plotWidth
      ),
      shiny::uiOutput(ns("click_info")),
      shiny::uiOutput(ns("hover_info"))
    )
  )

}





#' bubble module server-side
#'
#' @param input,output,session Internal parameters for shiny.
#' @param results SubScreenResult object with results from a subscreencalc call.
#' @param YRange Range of y-axis.
#' @param XRange Range of x-axis.
#' @param plot_type Linear or logarithmic scale for x-axis.
#' @param plot_type2 Linear or logarithmic scale for y-axis.
#' @param point_size Size of the dots.
#' @param pch_value Point shape 0-25.
#' @param color Color column with color information for each dot.
#' @param ColorBGplot Background color.
#' @param ColorTabClicked Selected dot color.
#' @param ColorPoints Dots color.
#' @param colthemeCol color theme color.
#' @param ColorReference Color reference line color.
#' @param x Endpoint for x-axis.
#' @param y Endpoint for first y-axis.
#' @param y2 Endpoint for second y-axis 2.
#' @param plot_points_data_complement Complement information.
#' @param key Number factors displayed.
#' @param nice_Numbers list of numbers used for a 'nice' scale.
#' @param xlabel Label for the x-axis.
#' @param grid Grid used in plot background.
#' @param circlestyle Appearance of circles.
#' @noRd
mod_bubble_server <- function(input, output, session,
    results,
    plot_point,
    XRange = input$YRange,
    YRange = input$YRange,
    plot_type = input$plot_type,
    plot_type2 = input$plot_type,
    point_size = input$pointsize,
    #pch_value = input$pch_value,
    color,
    ColorBGplot = ColorBGplot(),
    ColorTabClicked,
    ColorReference,
    ColorPoints,
    x = input$x,
    y = input$y,
    y2 = input$y2,
    plot_points_data_complement,
    key = input$key,
    nice_Numbers,
    xlabel = input$xlabel,
    circlestyle = input$circlestyle,
    grid = input$grid
  ) {

  SGID <- font.col <- NULL
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

  ns <- session$ns


  output$bubble <- shiny::renderPlot({

    # if logarithmic x-axis is selected
    if (plot_type() == "lin" & plot_type2() == "lin") {
      graphics::par(
        oma = c(0, 0, 0, 0),
        mar = c(3, 3, 1, 1),
        bg = ColorBGplot()
      )

      plot(
        x = 0,
        y = 0,
        xlim = XRange(),
        ylim = YRange(),
        xlab = '',
        ylab = '',
        type = 'n',
        axes = FALSE,
        log = ""
      )

      graphics::rect(
        xleft = graphics::grconvertX(0,'ndc','user'),
        xright = graphics::grconvertX(1,'ndc','user'),
        ybottom = graphics::grconvertY(0,'ndc','user') ,
        ytop = graphics::grconvertY(1,'ndc','user') ,
        border = NA,
        col = ColorBGplot(),
        xpd = TRUE
      )

      if (!is.null(plot_point()$x)) {
        if (key()[1] == key()[2]) SG_tit <- paste(key()[1], "-Factorial Subgroups (", length(plot_point()$x), ")", sep = "")
        else SG_tit <- paste(key()[1], " to ", key()[2], "-Factorial Subgroups (", length(plot_point()$x), ")", sep = "")

      suppressWarnings(
        graphics::symbols(
          main = SG_tit,
          col.main = font_color(ColorBGplot()),
          x = plot_point()$x,
          y = plot_point()$y,
          circles = sqrt((results()$sge[, c('N.of.subjects')][results()$sge$nfactors >= key()[1] & results()$sge$nfactors <= key()[2]] )/ pi ),
          inches = 1/3,
          xlim = XRange(),
          ylim = YRange(),
          fg = "grey",
          bg = color(),
          log = "",
          add = TRUE
        )
      )
      }
    }

    if (plot_type() == "log" & plot_type2() == "lin") {
      graphics::par(
        oma = c(0, 0, 0, 0),
        mar = c(3, 3, 1, 1),
        bg = ColorBGplot()
      )

      plot(
        x = 1,
        y = 0,
        xlim = XRange(),
        ylim = YRange(),
        xlab = '',
        ylab = '',
        type = 'n',
        axes = FALSE,
        log = "x"
      )

      graphics::rect(
        xleft = graphics::grconvertX(0,'ndc','user') - ifelse(plot_type() == "lin", 1000, 0),
        xright = graphics::grconvertX(1,'ndc','user') + ifelse(plot_type() == "lin", 1000, 0),
        ybottom = graphics::grconvertY(0,'ndc','user') - ifelse(plot_type() == "lin", 1000, 0),
        ytop = graphics::grconvertY(1,'ndc','user') + ifelse(plot_type() == "lin", 1000, 0),
        border = NA,
        col = ColorBGplot(),
        xpd = TRUE
      )

      suppressWarnings(
        graphics::symbols(
          main = SG_tit(),
          col.main = font_color(ColorBGplot()),
          x = plot_point()$x,
          y = plot_point()$y,
          circles = sqrt(( results()$sge[,c('N.of.subjects')][results()$sge$nfactors >= key()[1] & results()$sge$nfactors <= key()[2]] )/ pi),
          inches = 1/3,
          xlim = XRange(),
          ylim = YRange(),
          fg = "grey",
          bg = color(),
          log = "x",
          add = TRUE
        )
      )
    }

    if (plot_type() == "lin" & plot_type2() == "log") {
      graphics::par(
        oma = c(0, 0, 0, 0),
        mar = c(3, 3, 1, 1),
        bg = ColorBGplot()
      )

      plot(
        x = 0,
        y = 1,
        xlim = XRange(),
        ylim = YRange(),
        xlab = '',
        ylab = '',
        type = 'n',
        axes = FALSE,
        log = "y"
      )

      graphics::rect(
        xleft = graphics::grconvertX(0,'ndc','user'),
        xright = graphics::grconvertX(1,'ndc','user'),
        ybottom = graphics::grconvertY(0,'ndc','user'),
        ytop = graphics::grconvertY(1,'ndc','user'),
        border = NA,
        col = ColorBGplot(),
        xpd = TRUE
      )

      suppressWarnings(
        graphics::symbols(
          main = SG_tit(),
          col.main = font_color(ColorBGplot()),
          x = plot_point()$x,
          y = plot_point()$y,
          circles = sqrt(( results()$sge[,c('N.of.subjects')][results()$sge$nfactors >= key()[1] & results()$sge$nfactors <= key()[2]] )/ pi ),
          inches = 1/3,
          xlim = XRange(),
          ylim = YRange(),
          fg = "grey",
          bg = color(),
          log =  "y",
          add = TRUE
        )
      )
    }

    if (plot_type() == "log" & plot_type2() == "log") {
      graphics::par(
        oma = c(0, 0, 0, 0),
        mar = c(3, 3, 1, 1),
        bg = ColorBGplot()
      )

      plot(
        x = 1,
        y = 1,
        xlim = XRange(),
        ylim = YRange(),
        xlab = '',
        ylab = '',
        type = 'n',
        axes = FALSE,
        log = "yx"
      )

      graphics::rect(
        xleft = graphics::grconvertX(0, 'ndc', 'user'),
        xright = graphics::grconvertX(1, 'ndc', 'user'),
        ybottom = graphics::grconvertY(0, 'ndc', 'user'),
        ytop = graphics::grconvertY(1, 'ndc', 'user'),
        border = NA,
        col = ColorBGplot(),
        xpd = TRUE
      )

      suppressWarnings(
        graphics::symbols(
          main = SG_tit(),
          col.main = font_color(ColorBGplot()),
          x = plot_point()$x,
          y = plot_point()$y,
          circles = sqrt((results()$sge[, c('N.of.subjects')][results()$sge$nfactors >= key()[1] & results()$sge$nfactors <= key()[2]] )/ pi),
          inches = 1/3,
          xlim = XRange(),
          ylim = YRange(),
          fg = "grey",
          bg = color(),
          log = "yx",
          add = TRUE
        )
      )
    }

    graphics::box(col = font_color(ColorBGplot()))
    graphics::axis(
      1,
      col = font_color(ColorBGplot()),
      col.ticks = font_color(ColorBGplot()),
      col.axis = font_color(ColorBGplot()),
      cex.axis = 1
    )
    graphics::axis(
      2,
      col = font_color(ColorBGplot()),
      col.ticks = font_color(ColorBGplot()),
      col.axis = font_color(ColorBGplot()),
      cex.axis = 1
    )
    graphics::mtext(
      text = y(),
      side = 1,
      line = 3,
      col = font_color(ColorBGplot()),
      cex = 1
    )

    graphics::mtext(
      text = y2(),
      side = 2,
      line = 3,
      col = font_color(ColorBGplot()),
      cex = 1
    )
  })

  #click event handler
  click_points_data <- shiny::reactiveValues(xy = data.frame(x = NULL, y = NULL))

  shiny::observeEvent(c(input$plot_click), {
    curr_x <- shiny::req(x())

    clicked <- shiny::nearPoints(
      results()$sge[which(results()$sge$nfactors >= key()[1] & results()$sge$nfactors <= key()[2]),],
      input$plot_click,
      xvar = y(),
      yvar = y2(),
      maxpoints = NULL
    )

    clicked <- subset(
      clicked,
      select = c("SGID", x = curr_x, y = y(), "nfactors", results()$factors)
    )

    click_points_data$xy <- clicked[, unlist(lapply(clicked, function(x) !all(is.na(x))))]
  })


  # hover information window
  output$hover_info <- shiny::renderUI({
    shiny::req(input$plot_hover, plot_point())

    input$plot_hover
    all_points <- cbind(plot_point(), color = color(), stringsAsFactors = FALSE)

    colored_points <- all_points

    hover <- input$plot_hover
    hover$mapping <- list(xintercept = "xintercept", x = "x", y = "y")

    point <- shiny::nearPoints(colored_points, hover)

    if (nrow(point) == 0) return(NULL)
      # left_pct <- (hover$coords_img$x - hover$range$left) / (hover$range$right - hover$range$left)
      # top_pct <- (hover$domain$top - ifelse(plot_type() == "lin", hover$y, log10(hover$y))) / (hover$domain$top - hover$domain$bottom)
      # left_px <- (hover$range$left + left_pct * (hover$range$right - hover$range$left) / hover$img_css_ratio$x) + 3
      # top_px <- (hover$range$top + top_pct * (hover$range$bottom - hover$range$top) / hover$img_css_ratio$y) + 3
      #
      left_px <- hover$coords_css$x
      top_px <- hover$coords_css$y


    # style <- paste0("position:absolute; z-index:100;background-color: rgba(",
    #   grDevices::col2rgb(point$color)[1],",",grDevices::col2rgb(point$color)[2],",",grDevices::col2rgb(point$color)[3],",0.85); ",
    #                 "left:", left_px, "px; top:", top_px, "px; border: 0px;")
    #
      style <- paste0(
        "position:absolute;
        z-index:100;background-color: rgba(",
          grDevices::col2rgb(ColorBGplot())[1],",",
          grDevices::col2rgb(ColorBGplot())[2],",",
          grDevices::col2rgb(ColorBGplot())[3],",0.95); ",
          "left:", left_px, "px; top:", top_px, "px; border: 0px;"
      )

          res_and_color <- cbind(results()$sge[results()$sge$nfactors >= key()[1] & results()$sge$nfactors <= key()[2],], font.col = color())
    tmp <- res_and_color[res_and_color$SGID %in% point$ID, ]

    tmp$text <- apply(
      tmp[, results()$factors],
      1,
      function(x){paste(paste0(names(which(x != "Not used")),":", x[which(x != "Not used")]), collapse = ", ")}
    )
    tmp2 <- tmp %>%
      dplyr::mutate(
        text2 = paste0("ID:", SGID,", ", .data$text),
        text3 =paste("<p>",
                     ifelse(nrow(tmp) > 1,
                            paste0("<b style = 'color: ",
                     ColorPoints() ,
                    "'> List of: ",nrow(tmp)," </b></br> <ul>"),
                            paste0("")
                      ),
                      ifelse(nrow(tmp) > 1,
                         paste(
                           "<li> <b style = 'color: ",font.col,"'> SGID:", SGID, ", ",x() ,":", !!rlang::sym(x()),", ",y() ,":",!!rlang::sym(y()),
                           "</br>", .data$text, "</b> </li><br>"
                           ,collapse = ""
                         ),
                         paste(
                           "<b style = 'color: ",font.col,"'> SGID:", SGID, ", ",x() ,":", !!rlang::sym(x()),", ",y() ,":",!!rlang::sym(y()),
                           "</br>", .data$text, "</b><br>"
                           ,collapse = ""
                         )
                      ),
                    ifelse(nrow(tmp) > 1,"</ul>",""),
                    "</p>",
         collapse ="")
      )
    if(length(tmp2$text3)!= 0) {

    shiny::wellPanel(
      style = style,
       shiny::p(
        shiny::HTML(
          as.character(tmp2$text3[1])
        )
      )
    )
    }
    # point <- point[1,]
    #
    # tmp1 <- colnames(results()$sge[which(results()$sge$SGID == point$ID), results()$factors])[which(results()$sge[which(results()$sge$SGID == point$ID), results()$factors] != "Not used")]
    #
    # tmp2 <- results()$sge %>%
    #   dplyr::filter(SGID %in% point$ID) %>%
    #   dplyr::select(colnames(results()$sge[which(results()$sge$SGID == point$ID), results()$factors])[which(results()$sge[which(results()$sge$SGID == point$ID), results()$factors] != "Not used")])
    #
    # tmp2 <- data.frame(lapply(tmp2, as.character), stringsAsFactors = FALSE)
    #
    # shiny::wellPanel(
    #   style = style,
    #  shiny::p(
    #     shiny::HTML(
    #       ifelse(length(tmp1)>0,
    #       paste0(
    #         "<b style = 'color: ",
    #         font_color(point$color),
    #         "'> ",
    #         y(),
    #         ": ",
    #         point$x,
    #         "</br>",
    #         "<b style = 'color: ",
    #         font_color(point$color),
    #         "'> ",
    #         y2(),
    #         ": ",
    #         point$y,
    #         "</br>",
    #         "<b style = 'color: ",
    #         font_color(point$color),
    #         "'> Factors(",
    #         length(tmp1),
    #         "): ",
    #         paste(
    #           paste0(
    #             tmp1,": ", tmp2
    #           ), collapse = ", "
    #         ),
    #         "</br>"
    #       ),
    #       paste0(
    #         "<b style = 'color: ",
    #         font_color(point$color),
    #         "'> ",
    #         x(),
    #         ": ",
    #         point$x,
    #         "</br>",
    #         "<b style = 'color: ",
    #         font_color(point$color),
    #         "'> ",
    #         y(),
    #         ": ",
    #         point$y
    #       )
    #       )
    #     )
    #   )
    # )
  })

  shiny::observeEvent(c(input$plot_click), {
    shiny::req(input$plot_click, plot_point())
    all_points <- cbind(plot_point(), color = color(), stringsAsFactors = FALSE)

    colored_points <- all_points

    tmp_complement <- data.frame(
      plot_points_data_complement()["ID"],
      x = unname(plot_points_data_complement()["N.of.subjects.complement"]),
      y = unname(plot_points_data_complement()[paste0("Complement_",y())]),
      plot_points_data_complement()["color"]
    )

    colored_points <- rbind(colored_points,tmp_complement)

    click <- input$plot_click
    click$mapping <- list(xintercept = "xintercept", x = "x", y = "y")

    point <- shiny::nearPoints(colored_points, click)
  if (nrow(point) == 0) {
    selected_SGIDs$val <- NULL
    output$click_info <- shiny::renderUI({
      NULL
    })
  } else {
    # left_pct <- (click$coords_img$x - click$range$left) / (click$range$right - click$range$left)
    # top_pct <- (click$domain$top - ifelse(plot_type() == "lin", click$y, log10(click$y))) / (click$domain$top - click$domain$bottom)
    #
    # left_px <- (click$range$left + left_pct * (click$range$right - click$range$left) / click$img_css_ratio$x) + 3
    # top_px <- (click$range$top + top_pct * (click$range$bottom - click$range$top) / click$img_css_ratio$y) - 47

    left_px <- click$coords_css$x
    top_px <- click$coords_css$y
    style <- paste0(
      "position:absolute;
       z-index:110;background-color: rgba(",
        grDevices::col2rgb(ColorTabClicked())[1],",",
        grDevices::col2rgb(ColorTabClicked())[2],",",
        grDevices::col2rgb(ColorTabClicked())[3],",0.95); ",
        "left:", left_px, "px; top:", top_px, "px; border: 0px;"
    )

    tmp <- results()$sge[results()$sge$SGID %in% point$ID, ]

    if (length(results()$factors) == 1){
      tmp$text <- paste0(results()$factors,":",tmp[,results()$factors])
    } else {
      tmp$text <- apply(
        tmp[, results()$factors],
        1,
        function(x){paste(paste0(names(which(x != "Not used")),":", x[which(x != "Not used")]), collapse = ", ")}
      )
    }
    tmp <- tmp %>%
      dplyr::mutate(text2 = paste0("SGID:", SGID,", ", .data$text))

    if(nrow(tmp) ==1) {
      selected_SGIDs$val <- tmp$SGID
      output$click_info <- shiny::renderUI({
        NULL
      })
    } else if (nrow(tmp) > 1) {
    output$click_info <- shiny::renderUI({
      shiny::wellPanel(
        style = style,
          shiny::radioButtons(
            inputId =ns("checkbox"),
            label = "Select a subgroup:",
            choiceNames = tmp$text2,
            choiceValues = tmp$SGID,
            selected = ifelse(nrow(tmp) ==1,tmp$SGID ,"")
          )
      )
    })
    }
  }
  },ignoreNULL = FALSE)

  selected_SGIDs <- shiny::reactiveValues(val = NULL)

  shiny::observeEvent(input$checkbox,{
    selected_SGIDs$val <- input$checkbox
    output$click_info <- shiny::renderUI({
      NULL
    })
  })

 return(
    list(
      clicked_points = shiny::reactive({ click_points_data$xy }),
      plot_click = shiny::reactive({input$plot_click}),
      selected_SGIDs = shiny::reactive({selected_SGIDs$val})
    )
  )
}
