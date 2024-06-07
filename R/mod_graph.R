#' graph module User-Interface
#'
#' @param id Internal parameters for shiny.
#' @param plotHeight Plot height.
#' @param plotWidth Plot width.
#' @noRd
#'
#' @importFrom shiny NS tagList

mod_graph_ui <- function(id, plotHeight, plotWidth) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::div(style = "position:relative",
      shiny::plotOutput(
        outputId = ns("graph"),
        click = ns("plot_click"),
        hover = hoverOpts(
          ns("plot_hover"),
          delay = 50,
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

#' graph module server-side
#'
#' @param input,output,session Internal parameters for shiny.
#' @param results SubScreenResult object with results from a subscreencalc call
#' @param plot_point Selected point information.
#' @param YRange Range of y-axis.
#' @param XRange Range of x-axis.
#' @param plot_type Linear ("") or logarithmic ("log") y-axis (default: "").
#' @param point_size Dot size.
#' @param color Color column with color information for each dot.
#' @param ColorBGplot Background color.
#' @param ColorTabClicked Selected dot color.
#' @param ColorReference Color reference line color.
#' @param ColorPoints Dots color.
#' @param ColorMemorized Memorized subgroup dots color.
#' @param click_points_data Clicked point subgroup.
#' @param select_points_data Selected point subgroup.
#' @param factorial_context_data  Data frame with factorial context subgroups.
#' @param parents_data Data frame with parent subgroups.
#' @param x Endpoint for x-axis.
#' @param y Endpoint for y-axis
#' @param key Number factor level used.
#' @param nice_Numbers list of numbers used for a 'nice' scale.
#' @param xlabel Label for x-axis
#' @param circlestyle Appearance of circles.
#' @param grid Grid used in plot background.
#' @param pickradius Radius for point click.
#' @param memorized_Data Data frame with all memorized subgroups.
#' @param memorized_labels_on_off If labels of memorized subgroups should be displayed.
#' @param subTitle Subtitle for graph.
#' @param plot_points_data_complement Complement information.
#' @noRd

mod_graph_server <- function(
  input,
  output,
  session,
  results,
  plot_point,
  YRange = input$YRange,
  XRange = input$XRange,
  plot_type = input$plot_type,
  point_size = input$pointsize,
  color,
  ColorBGplot,
  ColorTabClicked,
  ColorReference,
  ColorPoints,
  ColorMemorized,
  click_points_data = NULL,
  select_points_data = NULL,
  factorial_context_data = NULL,
  parents_data = NULL,
  x = input$x,
  y = input$y,
  key = input$key,
  nice_Numbers,
  xlabel = input$xlabel,
  circlestyle = input$circlestyle,
  grid = input$grid,
  pickradius = input$pickradius,
  memorized_Data,
  memorized_labels_on_off = shiny::reactive({FALSE}),
  subTitle = NULL,
  plot_points_data_complement = NULL,
  remove_levels,
  show_ref_line,
  add_custom_ref_line = NULL,
  value_custom_ref_line,
  show_favour_arrows,
  favour_direction,
  favour_verum_name = NULL,
  favour_comparator_name = NULL
  ) {

  xmin <- xmax <- point_color <- memorizedText <- SGID <- text <- font.col <- NULL
  ns <- session$ns

  #### graph ####
  output$graph <- shiny::renderPlot({
    #require an not empty y-range value
    if (!is.null(YRange())) {
      #make reactive for all colors
      color()

      # filter data by factor levels
      data <- results()$sge[results()$sge$nfactors >= key()[1] & results()$sge$nfactors <= key()[2],]

      # create the title with subgroup and factor level information
      if (key()[1] == key()[2]) {
        title <- paste(key()[1], "-Factorial Subgroups (", length(data[[x()]]), ")", sep = "")
      } else {
        title <- paste(key()[1], " to ", key()[2], "-Factorial Subgroups (", length(data[[x()]]), ")", sep = "")
      }

      # add column for color information
      data$point_color <- color()[1:dim(data)[1]]

      #add column with memorized subgroup label ("" for no label as default)
      data$memorizedText <- ""

      if (dim(memorized_Data())[1] > 0) {
        tmp <- memorized_Data()
        tmp$memorizedText <- ""

        for (i in 1:dim(memorized_Data())[1]) {
          tmp[i,"memorizedText"] <- paste0(
            "N: ", tmp[i,x()], "\n",
            y(), ": ", tmp[i,y()],"\n",
            paste(
              names(
                tmp[,results()$factors]
              )[tmp[i,results()$factors]!="Not used"],
              ":",
              tmp[i,results()$factors][tmp[i,results()$factors]!="Not used"],
              collapse = "\n")
          )
        }
        data <- merge(tmp[,-1], data, all = TRUE)
      }

      # check for completeness of the data
      if (y() %in% colnames(data)) {
        data <- data[complete.cases(data[[y()]]),]

        minix <- roundDownNice(min(data[[x()]], na.rm=TRUE),nice = nice_Numbers)
        maxix <- roundUpNice(max(data[[x()]], na.rm=TRUE),nice = nice_Numbers)

        stepx <- roundUpNice((maxix - minix)/8, nice = nice_Numbers)

        # set stripes (8 stripes) values
        stripesxp <- stripesx <- (0:10 * stepx) #+ minix

        #add percentage values if x is Number of subjects
        if (x() == "N.of.subjects" && xlabel()) {
          tot <- results()$results_total[["N.of.subjects"]]
          perc <- round((stripesx / tot )*100)
          perc[perc <= 0 | perc >100] <- ""
          stripesxp <- paste0(stripesx, " (",perc,"%)")
          stripesxp <- stripesxp %>%
            stringr::str_remove(pattern = "[(]%[)]")
        }

        XRange <- XRange()
        if (!is.null(XRange)) {
          index <- stripesx >= XRange[1] & stripesx <= XRange[2]
          stripesx <- stripesx[index]
          stripesxp <- stripesxp[index]
        }

        if (length(stripesx) >= 2) {
          odd_list <- stripesx[seq(2, length(stripesx), by = 2)]
          rect_data <- data.frame(
            xmin = stripesx[seq(1,length(stripesx),by = 2)[1:length(odd_list)]],
            xmax = odd_list
          )
          if(!is.null(XRange)) {
            rect_data <- rect_data %>%
              dplyr::filter(xmin >= XRange[1], xmax <= XRange[2])
          }
        } else {
          rect_data <- NULL
        }

        size_tmp <- ifelse(circlestyle() == "standard", "point_size", "N.of.subjects")

        YRange <- YRange()

        if (!is.null(XRange)) {
          data <- data %>%
            dplyr::filter(
              !!rlang::sym(x()) >= XRange[1] &
              !!rlang::sym(x()) <= XRange[2]
            )
        }
        # filter data by yrange to avoid warnings in console
        data <- data %>%
          dplyr::filter(
            !!rlang::sym(y()) >= YRange[1] &
            !!rlang::sym(y()) <= YRange[2]
          )

      p <- ggplot2::ggplot(
         data %>% dplyr::arrange(desc(point_color)),
         ggplot2::aes(
           x = !!rlang::sym(x()),
           y = !!rlang::sym(y()),
           label = memorizedText
          )
      )

      if (!is.null(XRange)) {
        p <- p + ggplot2::scale_x_continuous(
          limits = XRange,
          labels = stripesxp,
          breaks = stripesx
        )
      }
      p <- p +
      ggplot2::theme_classic() +
        ggplot2::scale_y_continuous(
          limits = YRange,
          trans = ifelse(plot_type() == "lin", "identity","log10")
        )

      if (!is.null(rect_data)) {
        p <- p +
          ggplot2::geom_rect(
            data = rect_data,
            mapping = ggplot2::aes(xmin = xmin, xmax = xmax),
            ymin = -Inf,
            ymax = Inf,
            colour = ColorBGplot(),
            size = 0.5,
            alpha = 0.3,
            inherit.aes = FALSE
          )
      }

      if (is.null(subTitle) && xlabel()){
        p <- p +
          ggplot2::labs(
            title = title,
            subtitle = paste("N: ",results()$results_total[["N.of.subjects"]], " (100%)", sep="")
          )
      } else if (is.null(subTitle) && !(xlabel())){
        p <- p +
          ggplot2::labs(
            title = title,
            subtitle = paste("N = ",results()$results_total[["N.of.subjects"]], sep="")
          )
      } else {
        p <- p +
          ggplot2::labs(
            title = title,
            subtitle = subTitle
          )
      }


      if (circlestyle() == "standard") {
        p <- p +
          ggplot2::geom_point(
            data = data %>% dplyr::arrange(desc(point_color)) %>% dplyr::filter(!is.na(point_color)),
            ggplot2::aes(
              colour = point_color
            ),
            size = point_size(),
            na.rm = TRUE,
            pch = 19
          )
      } else {
        p <- p +
          ggplot2::geom_point(
            data = data %>% dplyr::arrange(desc(point_color))%>% dplyr::filter(!is.na(point_color)),
            ggplot2::aes(
              colour = point_color,
              size = !!rlang::sym(x())
            ),
            na.rm = TRUE,
            pch = 19
          )
      }

    if (memorized_labels_on_off()) {
      p <- p +
        ggrepel::geom_text_repel(
          colour = ColorMemorized(), max.overlaps = Inf,
          min.segment.length = 0,
          size = 5,
          bg.r = 0.15
          )
      }
      p <- p + ggplot2::scale_colour_identity() +
        ggplot2::theme(
          panel.background = ggplot2::element_rect(color = NA, fill = NA),
          axis.title = ggplot2::element_blank(),
          panel.border = ggplot2::element_blank(),
          axis.text = ggplot2::element_text(color = font_color(ColorBGplot()), size = 15),
          axis.line = ggplot2::element_line(color = font_color(ColorBGplot())),
          plot.background = ggplot2::element_rect(fill = ColorBGplot()),
          legend.background = ggplot2::element_rect(fill = ColorBGplot()),
          legend.position = "none",
          plot.title = ggplot2::element_text(size = 18, face = "bold", color = font_color(ColorBGplot())),
          plot.subtitle = ggplot2::element_text(size = 16, color = font_color(ColorBGplot()))
        )

      if (show_ref_line() == TRUE) {
        if (YRange[1] <= results()$results_total[, c(y())] &
            YRange[2] >= results()$results_total[, c(y())]) {
          p <- p +
          ggplot2::geom_hline(
            yintercept = results()$results_total[, c(y())],
            color = ColorReference()
          )
          # reference value label in blue for reference line
            p <- p +
            ggplot2::annotate(geom = "label",
              x = ifelse(!is.null(XRange()),XRange[2],max(rect_data)),
              y = results()$results_total[, c(y())],
              label = results()$results_total[, c(y())],
              color = font_color(ColorBGplot()),
              fill = ColorReference()
            )
        }
      }
      if (add_custom_ref_line() == TRUE) {
        if(!is.null(add_custom_ref_line())) {
          if(!is.na(value_custom_ref_line())) {
            if (YRange[1] <= value_custom_ref_line() &
                YRange[2] >= value_custom_ref_line() ) {
              p <- p +
              ggplot2::geom_hline(
                yintercept = value_custom_ref_line(),
                color = "#00BCFFFF"
              )
              # reference value label in blue for reference line
                p <- p +
                ggplot2::annotate(geom = "label",
                  x = ifelse(!is.null(XRange()),XRange[2],max(rect_data)),
                  y = value_custom_ref_line(),
                  label = value_custom_ref_line(),
                  color = "black",
                  fill = "#00BCFFFF"
                )
            }
          }
        }
      }

      if (show_favour_arrows()){

        ten_percent_x_range <- ifelse(!is.null(XRange()),XRange[2]*0.1,max(rect_data)*0.1)
        five_percent_y_range <- abs((YRange[2] - YRange[1]) * 0.05)

        p <- p +
            ggplot2::annotate(geom = "label",
              x = ifelse(!is.null(XRange()),XRange[2],max(rect_data)),
              y = YRange[2],
              label = ifelse(favour_direction(),ifelse(is.null(favour_verum_name),"Favours Verum",paste0("Favours ", favour_verum_name)),ifelse(is.null(favour_comparator_name),"Favour Comparator",paste0("Favour ",favour_comparator_name))),
              color = ifelse(favour_direction(),"black","white"),
              fill = ifelse(favour_direction(),"#89D329FF","#FF3162FF"),
              size = ggplot2::unit(6, "pt"),
              hjust = 1
            ) +
            ggplot2::annotate(geom = "label",
              x = ifelse(!is.null(XRange()),XRange[2],max(rect_data)),#-ten_percent_x_range,
              y = YRange[1],
              label = ifelse(favour_direction(),ifelse(is.null(favour_comparator_name),"Favours Comparator",paste0("Favours ",favour_comparator_name)), ifelse(is.null(favour_verum_name),"Favour Verum",paste0("Favour ", favour_verum_name))),
              color = ifelse(favour_direction(),"white", "black"),
              fill = ifelse(favour_direction(),"#FF3162FF", "#89D329FF"),
              size = ggplot2::unit(6, "pt"),
              hjust = 1
            )
      }
      # add grid when option ixs selected

      if (grid()) {
       p <- p +
        ggplot2::theme(
          panel.ontop = TRUE,
          panel.grid.minor = ggplot2::element_line(colour = font_color(ColorBGplot()), size = 0.2),
          panel.grid.major = ggplot2::element_line(colour = font_color(ColorBGplot()), size = 0.1)
        )
      }
        if (!is.null(plot_points_data_complement())) {
          if (plot_points_data_complement()[["N.of.subjects.complement"]] > 0) {
            if (!is.na(plot_points_data_complement()[[paste0("Complement_", y())]])) {
              if (YRange[1] <= plot_points_data_complement()[[paste0("Complement_", y())]] &
                  YRange[2] >= plot_points_data_complement()[[paste0("Complement_", y())]] &
                  ifelse(!is.null(XRange()),XRange[2],max(rect_data)) >= plot_points_data_complement()[["N.of.subjects.complement"]]
              ){
                p <- p +
                  ggplot2::annotate(
                    "point",
                    x = plot_points_data_complement()[["N.of.subjects.complement"]],
                    y = plot_points_data_complement()[[paste0("Complement_", y())]],
                    size =point_size(),
                    colour = "yellow",
                    pch = 13
                  )
              }
            }
          }
        }
        p
      }
    }
  })

  #click event handler
  click_points_data <- shiny::reactiveValues(xy = data.frame(x = NULL, y = NULL))

  shiny::observeEvent(c(input$plot_click), {
    curr_x <- shiny::req(x())
    start_radius <- pickradius()

    clicked <- shiny::nearPoints(
      results()$sge[which(results()$sge$nfactors >= key()[1] & results()$sge$nfactors <= key()[2]),],
      input$plot_click,
      xvar = curr_x,
      yvar = y(),
      threshold = start_radius,
      maxpoints = NULL
    )

    clicked <- subset(
      clicked,
      select = c("SGID", x = curr_x, y = y(), "nfactors", results()$factors)
    )

    click_points_data$xy <- clicked[, unlist(lapply(clicked, function(x) !all(is.na(x))))]
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

    point <- nearPoints(colored_points, click)

  if (nrow(point) == 0) {
    selected_SGIDs$val <- NULL
    output$click_info <- shiny::renderUI({
      NULL
    })
  } else {
    left_pct <- (click$coords_img$x - click$range$left) / (click$range$right - click$range$left)
    top_pct <- (click$domain$top - ifelse(plot_type() == "lin", click$y, log10(click$y))) / (click$domain$top - click$domain$bottom)

    left_px <- (click$range$left + left_pct * (click$range$right - click$range$left) / click$img_css_ratio$x) + 3
    top_px <- (click$range$top + top_pct * (click$range$bottom - click$range$top) / click$img_css_ratio$y) - 47

    style <- paste0(
      "position:absolute;
       z-index:110;background-color: rgba(",
        grDevices::col2rgb(ColorTabClicked())[1],",",
        grDevices::col2rgb(ColorTabClicked())[2],",",
        grDevices::col2rgb(ColorTabClicked())[3],",0.95); ",
        "left:", left_px, "px; top:", top_px, "px; border: 0px;"
    )

    tmp <- results()$sge[results()$sge$SGID %in% point$ID, ]

    tmp$text <- apply(
      tmp[, results()$factors],
      1,
      function(x){paste(paste0(names(which(x != "Not used")),":", x[which(x != "Not used")]), collapse = ", ")}
    )

    tmp <- tmp %>%
      dplyr::mutate(text2 = paste0("SGID:", SGID,", ", text))

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

  selected_SGIDs <- reactiveValues(val = NULL)

  observeEvent(input$checkbox,{
    selected_SGIDs$val <- input$checkbox
    output$click_info <- shiny::renderUI({
      NULL
    })
  })

  ## hover information window
  output$hover_info <- shiny::renderUI({
    shiny::req(input$plot_hover, plot_point())

    input$plot_hover
    all_points <- cbind(plot_point(), color = color(), stringsAsFactors = FALSE)

    colored_points <- all_points

    tmp_complement <- data.frame(
      plot_points_data_complement()["ID"],
      x = unname(plot_points_data_complement()["N.of.subjects.complement"]),
      y = unname(plot_points_data_complement()[paste0("Complement_",y())]),
      plot_points_data_complement()["color"]
    )

    colored_points <- rbind(colored_points,tmp_complement)

    hover <- input$plot_hover
    hover$mapping <- list(xintercept = "xintercept", x = "x", y = "y")

    point <- nearPoints(colored_points, hover)

    if (nrow(point) == 0) return(NULL)

      left_pct <- (hover$coords_img$x - hover$range$left) / (hover$range$right - hover$range$left)

      top_pct <- (hover$domain$top - ifelse(plot_type() == "lin", hover$y, log10(hover$y))) / (hover$domain$top - hover$domain$bottom)

      left_px <- (hover$range$left + left_pct * (hover$range$right - hover$range$left) / hover$img_css_ratio$x) + 3
      top_px <- (hover$range$top + top_pct * (hover$range$bottom - hover$range$top) / hover$img_css_ratio$y) + 3

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
        text2 = paste0("ID:", SGID,", ", text),
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
                           "</br>", text, "</b> </li><br>"
                           ,collapse = ""
                         ),
                         paste(
                           "<b style = 'color: ",font.col,"'> SGID:", SGID, ", ",x() ,":", !!rlang::sym(x()),", ",y() ,":",!!rlang::sym(y()),
                           "</br>", text, "</b><br>"
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
  })

 return(
    list(
      clicked_points = shiny::reactive({click_points_data$xy }),
      plot_click = shiny::reactive({input$plot_click}),
      selected_SGIDs = shiny::reactive({selected_SGIDs$val})
    )
  )
}
