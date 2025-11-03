#' (iii) Determine variable importance
#'
#' Determine variable importance for continuous, categorical or right-censored
#' survival endpoints (overall and per treatment group) using random forests
#'
#' @param data The data frame containing the dependent and independent variables.
#' @param y The name of the column in \code{data} that contains the dependent variable.
#' @param cens The name of the column in \code{data} that contains the censoring variable, if \code{y} is an event time (default=NULL).
#' @param trt The name of the column in \code{data} that contains the treatment variable (default=NULL).
#' @param x Vector that contains the names of the columns in \code{data} with the independent variables (default=NULL, i.e. all remaining variables)
#' @return A list containing ordered data frames with the variable importances
#'   (one for each treatment level, one with the ranking variability between the
#'   treatment levels and one with the total results)
#'
#' @keywords variable importance
#' @export subscreenvi
#' @examples
#' \dontrun{
#'require(survival)
#'data(pbc, package="survival")
#'# generate categorical versions of some of the baseline covariates
#'pbc$ageg[!is.na(pbc$age)]        <-
#'   ifelse(pbc$age[!is.na(pbc$age)]          <= median(pbc$age,     na.rm=TRUE), "Low", "High")
#'pbc$albuming[!is.na(pbc$albumin)]<-
#'   ifelse(pbc$albumin[!is.na(pbc$albumin)]  <= median(pbc$albumin, na.rm=TRUE), "Low", "High")
#'pbc$phosg[!is.na(pbc$alk.phos)]  <-
#'   ifelse(pbc$alk.phos[!is.na(pbc$alk.phos)]<= median(pbc$alk.phos,na.rm=TRUE), "Low", "High")
#'pbc$astg[!is.na(pbc$ast)]        <-
#'   ifelse(pbc$ast[!is.na(pbc$ast)]          <= median(pbc$ast,     na.rm=TRUE), "Low", "High")
#'pbc$bilig[!is.na(pbc$bili)]      <-
#'   ifelse(pbc$bili[!is.na(pbc$bili)]        <= median(pbc$bili,    na.rm=TRUE), "Low", "High")
#'pbc$cholg[!is.na(pbc$chol)]      <-
#'   ifelse(pbc$chol[!is.na(pbc$chol)]        <= median(pbc$chol,    na.rm=TRUE), "Low", "High")
#'pbc$copperg[!is.na(pbc$copper)]  <-
#'   ifelse(pbc$copper[!is.na(pbc$copper)]    <= median(pbc$copper,  na.rm=TRUE), "Low", "High")
#'pbc$ageg[is.na(pbc$age)]         <- "No Data"
#'pbc$albuming[is.na(pbc$albumin)] <- "No Data"
#'pbc$phosg[is.na(pbc$alk.phos)]   <- "No Data"
#'pbc$astg[is.na(pbc$ast)]         <- "No Data"
#'pbc$bilig[is.na(pbc$bili)]       <- "No Data"
#'pbc$cholg[is.na(pbc$chol)]       <- "No Data"
#'pbc$copperg[is.na(pbc$copper)]   <- "No Data"
#'#eliminate treatment NAs
#'pbcdat <- pbc[!is.na(pbc$trt), ]
#' pbcdat$status <- ifelse(pbcdat$status==0,0,1)
#' importance <- subscreenvi(data=pbcdat, y='time', cens='status',
#'  trt='trt', x=c("ageg", "sex", "bilig", "cholg", "copperg"))
#' }

subscreenvi <- function(data, y, cens = NULL, x = NULL, trt = NULL) {
  Importance <- NULL
  # transform character to factor
  if (any(sapply(data, is.character))) {
    data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], factor)
  }

  # build formula
  if (is.null(x)) x <- setdiff(colnames(data), c(trt, y, cens))

  x.form <- paste(x, collapse = '+')

  if (!is.null(cens)){
    mod.form <- paste0('Surv(', y, ', ', cens, ')~', x.form)
  }else{
    mod.form <- paste0(y, '~', x.form)
  }
  # empty list for results

  outcome <- list()
  # fit random forest for each treatment level and save variable importance
  for (j in 1:length(mod.form)){
    result <- list()
    tmp <- list()
    if (!is.null(trt)){

      trt.lev <- levels(factor(data[, trt]))

      for (i in 1:length(trt.lev)){

        fit <- ranger::ranger(
          stats::as.formula(mod.form[j]),
          data = data[data[, trt] == trt.lev[i], ],
          importance = "permutation",
          num.trees = 1000
        )

        vi <- sort(fit$variable.importance, decreasing = TRUE)

        res.df <- data.frame('Variable' = names(vi), 'Importance' = vi)
        rownames(res.df) <- NULL
        result[[paste0('VI.trt.', trt.lev[i])]] <- res.df

        res.df[, 2] <- 1:nrow(res.df)
        colnames(res.df)[2] <- paste0('rank', i)
        tmp[[paste0('VI.trt.', trt.lev[i])]] <- res.df
      }
    }

    # summarize trt level results: rank variability over treatment levels
    tmp <- plyr::join_all(tmp, by = 'Variable', type = 'full')
    tmp$'Importance' <- apply(tmp[,-1], MARGIN = 1, FUN = stats::var)
    tmp <- plyr::arrange(tmp, plyr::desc(Importance))
    result[['VI.RV.trt']] <- tmp[, c('Variable', 'Importance')]

    outcome[[j]] <- result$VI.RV.trt
  }
  names(outcome) <- y
  return(outcome)
}

