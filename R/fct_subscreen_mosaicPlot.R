#' Creates an mosaic plot used in Mosaic-tab in Subgroup Explorer
#'
#' @param res results data set from subscreencalc
#' @param mos.x first endpoint variable
#' @param mos.y second endpoint variable (default:NULL)
#' @param mos.y2 third endpoint variable (default: NULL)
#' @param mos.z reference variable (mosaic size)
#' @param col.bg background color (default: '#424242')
#' @param col.txt text color font (default: '#ffffff')
#' @param colrange.z three color scale for mosaic colors (default: c('#00BCFF','gray89','#89D329'))
#' @param scale scale of endpoint values linear or logarithmic (default: 'lin')
#'
#'@keywords internal

subscreen_mosaicPlot <- function(
  res,
  mos.x,
  mos.y = NULL,
  mos.y2 = NULL,
  mos.z,
  col.bg = c("#424242"),
  col.txt = c("#ffffff"),
  colrange.z = c("#10384FFF","#00BCFFFF","white","#89D329FF","#004422FF"),
  #colrange.z = c("#443247FF","#00BCFFFF","white","#fad56eFF","#004422FF"),
  scale = "lin"
) {

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
    if (mos.x %in% colnames(res$sge)) {
    tmp_x <- res$sge[res$sge$nfactors == 1 & !res$sge[, mos.x] %in% not.used, ]
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
      tmp_y_1 <- res$sge[res$sge$nfactors == 1 & !res$sge[, mos.y] %in% not.used, ]
      dim_y <- dim(tmp_y_1)[1]
      tmp_y <- res$sge[res$sge$nfactors == 2 & !res$sge[, mos.y] %in% not.used &
                               !res$sge[, mos.x] %in% not.used, ]

      tmp_y <- dplyr::arrange(tmp_y, !!!rlang::syms(c(mos.x, mos.y)))
      expected_tmp_y <- expand.grid(lapply(lapply(
        tmp_y %>% dplyr::select(c(mos.x,mos.y)) ,levels),function(x){x[x != "Not used"]}))

      if(dim(tmp_y)[1] !=  dim(expected_tmp_y)[1]) {
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

      tmp_y_1 <- res$sge[res$sge$nfactors == 2 & !res$sge[, mos.y] %in% not.used &
                               !res$sge[, mos.y2] %in% not.used, ]
      dim_y <- dim(tmp_y_1)[1]
      tmp_y <- res$sge[res$sge$nfactors == 3 & !res$sge[, mos.x] %in% not.used & !res$sge[, mos.y] %in% not.used &
                               !res$sge[, mos.y2] %in% not.used, ]

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
    if (shiny::req(scale) == "lin") {
      rg.z <- range(res$sge[, mos.z], na.rm = TRUE)
    }
    if (shiny::req(scale) == "log") {
      rg.z <- log(
        range(
          res$sge[, mos.z], na.rm = TRUE
        )
      )
    }

    if (is.null(mos.y)) {
      tmp_1factors <- tmp_x
    } else {
      if (is.null(mos.y2)) {
        tmp_2factors <- res$sge[res$sge$nfactors == 2 & !res$sge[, mos.x] %in% not.used & !res$sge[, mos.y] %in% not.used,]
      } else {
        tmp_3factors <- res$sge[res$sge$nfactors == 3 & !res$sge[, mos.x] %in% not.used &
                               !res$sge[, mos.y] %in% not.used & !res$sge[, mos.y2] %in% not.used, ]
        tmp_3factors <- dplyr::arrange(tmp_3factors, !!!rlang::syms(c(mos.x,mos.y,mos.y2)))
      }
    }

      if (!is.null(mos.y2)) {
       val.z <- matrix(dplyr::arrange(tmp_y, !!!rlang::syms(c(mos.x, mos.y, mos.y2))) %>%
                         dplyr::pull(mos.z),dim(mid.y)[1] ,length(mid.x))
       colnames(val.z) <- names(mid.x)
       rownames(val.z) <- rownames(mid.y)
      } else if (!is.null(mos.y)) {
        val.z <- data.frame(matrix(NA, nrow = nrow(mid.y), ncol = length(mid.x)))
        colnames(val.z) <- names(mid.x)
        rownames(val.z) <- rownames(mid.y)

        for (i in 1:length(mid.x)) {
          tmp <- tmp_2factors %>% dplyr::filter(!! rlang::sym(mos.x) == tmp_x2[i, mos.x])
          for (j in 1:nrow(mid.y)) {
            level <- tmp_y[j, mos.y]
            if (dim(dplyr::filter(tmp,!! rlang::sym(mos.y) == level))[1] > 0) {
              tmp1 <- dplyr::filter(tmp,!! rlang::sym(mos.y) == level)
              tmp1 <- ifelse(shiny::req(scale) == "lin", tmp1[, mos.z], log(tmp1[, mos.z]))
              val.z [j,i] <- tmp1
            } else {
              val.z [j,i] <- NA
            }
          }
        }

        tmp <- res$sge[res$sge$nfactors == 2 & !res$sge[, mos.x] %in% not.used & !res$sge[, mos.y] %in% not.used,]

      } else {
        tmp <- res$sge[res$sge$nfactors == 1 & !res$sge[, mos.x] %in% not.used, ]
        if(shiny::req(scale) == "lin") {
            val.z <- matrix(tmp_x2[, mos.z], ncol = length(prop.x) - 1, byrow = FALSE)
        } else if (shiny::req(scale) == "log") {
           val.z <- matrix(log(tmp_x2[, mos.z]), ncol = length(prop.x) - 1, byrow = FALSE)
        }
      }

    if (!rg.z[1] < res$results_total[,mos.z]) {
      rg.z[1] <- res$results_total[,mos.z] - (res$results_total[,mos.z]/1000)
    }
    if (!rg.z[2] > res$results_total[,mos.z]) {
      rg.z[2] <- res$results_total[,mos.z] + (res$results_total[,mos.z]/1000)
    }

    mean.z <- ifelse(shiny::req(scale) == "lin",
                     res$results_total[,mos.z],
                     log(res$results_total[,mos.z]))
    tr.mean.z <- (mean.z-rg.z[1])/diff(rg.z)

    f_colZ <- grDevices::colorRamp(colrange.z, bias = log(tr.mean.z, base = 0.5))

    graphics::par(
      mar = c(1, 14, 8, 12),
      bg = col.bg,
      oma = c(0, 0, 0, 0)
    )

    plot(
      NULL,
      xlim = c(0, 1),
      ylim = c(0,1),
      xlab = '',
      ylab = '',
      axes = FALSE,
      xaxs = 'i',
      yaxs = 'i'
    )


    if(is.null(mos.y)) {
      for (i in 1:length(mid.x)) {
        for (j in 1:length(mid.y)) {
          val.z.ij <- val.z[j,i]
          col.z.ij <- ifelse(
            is.na(val.z.ij),
            col.bg,
            grDevices::rgb(f_colZ((val.z.ij - rg.z[1])/diff(rg.z)), maxColorValue = 255)
          )
          graphics::rect(
            xleft = prop.x[i],
            xright = prop.x[i + 1],
            ybottom = prop.y[j],
            ytop = prop.y[j + 1],
            col = col.z.ij,
            border = "#000000",
            lwd = 1
          )
        }
      }

    } else {
      if(!is.null(mos.y2)) {
        if (shiny::req(scale) == "log") {
          val.z <- log(val.z)
        }
      }
      for (i in 1:length(mid.x)) {
        for (j in 1:dim(mid.y)[1]) {
          val.z.ij <- val.z[j,i]
          col.z.ij <- ifelse(
            is.na(val.z.ij),
            col.bg,
            grDevices::rgb(f_colZ((val.z.ij - rg.z[1])/diff(rg.z)), maxColorValue = 255)
          )
          graphics::rect(
            xleft = prop.x[i],
            xright = prop.x[i + 1],
            ybottom = prop.y[j,i],
            ytop = prop.y[j + 1,i],
            col = col.z.ij,
            border = "#000000",
            lwd = 1
          )
        }
      }
    }
    label_text_x <- names(mid.x)

    for (i in 1:length(mid.x)) {
      if (names(mid.x)[i] %>% nchar() < 25) {
        label_text_x[i] <- names(mid.x)[i]
      } else {
        label_text_x[i] <- names(mid.x)[i] %>% stringr::str_sub(1,25) %>% stringr::str_c("...")
      }
    }
    graphics::text(
      x = mid.x,
      y = 1.025,
      xpd = NA,
      adj = c(0.25,-3),
      col = col.txt,
      labels = label_text_x,
      cex = ifelse(is.null(mos.y2), 1, 0.75),
      srt = 45
    )

    if(!is.null(rownames(mid.y))) {
    label_text_y <- rownames(mid.y)
    for (i in 1:dim_y) {
      if (!is.null(rownames(mid.y))) {
        if (rownames(mid.y)[i] %>% nchar() < 30) {
          label_text_y[i] <- rownames(mid.y)[i]
        } else {
          label_text_y[i] <- rownames(mid.y)[i] %>% stringr::str_sub(1,30) %>% stringr::str_c("...")
        }
      } else {
        label_text_y <- NULL
      }
    }

    graphics::text(
      y = apply(mid.y,1,mean),
      x = -0.3,
      xpd = NA,
      adj = c(0, 0.5),
      col = col.txt,
      labels = label_text_y,
      srt = 0,
      cex = ifelse(is.null(mos.y2), 1, 0.75)
    )
  }
    leg.x <- graphics::grconvertX(1,'npc','user') + 0.5 * (graphics::grconvertX(1, 'ndc', 'user') - graphics::grconvertX(1, 'npc', 'user'))
    leg.y <- seq(graphics::grconvertY(0.1, 'npc', 'user'), graphics::grconvertY(0.9, 'npc', 'user'), length.out = 1001)
    leg.width <- 0.05
    graphics::rect(
      xleft = leg.x - leg.width / 2,
      xright = leg.x + leg.width / 2,
      ybottom = leg.y[-1],
      ytop = leg.y[-length(leg.y)],
      xpd = NA,
      col = grDevices::rgb(f_colZ(seq(0, 1, length.out = length(leg.y) - 1)), maxColorValue = 255), border = NA)

    ndig <- 2
    if(shiny::req(scale) == "lin") {
      ticks.q <- c(0, 1, 2, 3, 4) / 4
      graphics::text(
        x = leg.x - (leg.width / 2 + 0.01),
        y = quantile(leg.y, prob = ticks.q),
        xpd = NA,
        col = col.txt,
        adj = c(1, 0.5),
        labels = round(quantile(seq(rg.z[1], rg.z[2], length.out = 1001), prob = ticks.q), ndig),
        cex = 0.75
      )
    }
    if (shiny::req(scale) == "log") {
      ticks.q <- c(0, 1, 2, 3, 4) / 4
      graphics::text(
        x = leg.x - (leg.width / 2 + 0.01),
        y = quantile(leg.y, prob = ticks.q),
        xpd = NA,
        col = col.txt,
        adj = c(1, 0.5),
        labels = round(exp(quantile(seq(rg.z[1], rg.z[2], length.out = 1001), prob = ticks.q)), ndig),
        cex = 0.75
      )
    }

    graphics::segments(
      x0 = leg.x + (leg.width / 2),
      x1 = leg.x + (leg.width / 2 + 0.01),
      y0 = quantile(leg.y, prob = tr.mean.z),
      col = col.txt,
      lwd = 2,
      xpd = NA
    )

    graphics::text(
      x = leg.x + (leg.width / 2 + 0.02),
      y = quantile(leg.y, prob = tr.mean.z),
      xpd = NA,
      col = col.txt,
      adj = c(0, 0.5),
      font = 2,
      labels = paste0(ifelse(shiny::req(scale) == "lin", round(mean.z, ndig), round(exp(mean.z), ndig)),' (total)'),
      cex = 0.75
    )

    graphics::text(
      x = leg.x - 0.09,
      y = graphics::grconvertY(0.5, 'npc', 'user'),
      xpd = NA,
      col = col.txt,
      adj = c(0.5, 0),
      srt = 90,
      labels = mos.z,
      cex = 1,
      font = 2
    )
    }
}

