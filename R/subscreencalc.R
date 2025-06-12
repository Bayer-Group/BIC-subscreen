#' (i) Calculation of the results for the subgroups
#'
#' This function systematically calculates the defined outcome for every combination of subgroups
#' up to the given level (max_comb), i.e. the number of maximum combinations of subgroup defining factors.
#' If, e.g., in a study sex, age group (<=60, >60), BMI group (<=25, >25) are of interest, subgroups of level 2
#' would be, e.g, male subjects with BMI>25 or young females, while subgroups of level 3 would be any combination
#' of all three variables.
#'
#' The evaluation function (eval_function) has to defined by the user. The result needs to be a vector of numerical values,
#' e.g., outcome variable(s) and number of observations/subjects. The input of eval_function is a data frame with the same
#' structure as the input data frame (data) used in the subsreencalc call. See example below.
#' Potential errors occurring due to small subgroups should be caught and handled within eval_function.
#' As the eval_function will be called with every subgroup it may happen that there is only one observation or
#' only one treatment arm or only observations with missing data going into the eval_function. There should always be valid
#' result vector be returned (NAs allowed) and no error causing program abort.
#' For a better display the results may be cut-off to a reasonable range. For example: If my endpoint is a hazard ratio
#' that is expected to be between 0.5 and 2 I would set all values smaller than 0.01 to 0.01 and values above 100 to 100.
#'
#'
#' @param data dataframe with study data
#' @param eval_function name of the function for data analysis
#' @param subjectid name of variable in data that contains the subject identifier, defaults to subjid
#' @param factors character vector containing the names of variables that define the subgroups (required)
#' @param max_comb maximum number of factor combination levels to define subgruops, defaults to 3
#' @param nkernel number of kernels for parallelization (defaults to 1)
#' @param par_functions vector of names of functions used in eval_function to be exported to cluster (needed only if nkernel > 1)
#' @param verbose logical value to switch on/off output of computational information (defaults to TRUE)
#' @param factorial logical value to switch on/off calculation of factorial contexts (defaults to FALSE)
#' @param use_complement logical value to switch on/off calculation of complement subgroups (defaults to FALSE)
#' @param ... further parameters which where outdated used for notes and errors.
#' @return an object of type SubScreenResult of the form
#' list(sge=H,
#'      max_comb=max_comb,
#'      min_comb=min_comb,
#'      subjectid=subjectid,
#'      treat=treat,
#'      factors=factors,
#'      results_total=eval_function(cbind(F,T)))
#' @keywords subgroup analysis
#' @export subscreencalc
#' @examples
#' # get the pbc data from the survival package
#' require(survival)
#' data(pbc, package="survival")
#' # generate categorical versions of some of the baseline covariates
#' pbc$ageg[!is.na(pbc$age)]        <-
#'    ifelse(pbc$age[!is.na(pbc$age)]          <= median(pbc$age,     na.rm=TRUE), "Low", "High")
#' pbc$albuming[!is.na(pbc$albumin)]<-
#'    ifelse(pbc$albumin[!is.na(pbc$albumin)]  <= median(pbc$albumin, na.rm=TRUE), "Low", "High")
#' pbc$phosg[!is.na(pbc$alk.phos)]  <-
#'    ifelse(pbc$alk.phos[!is.na(pbc$alk.phos)]<= median(pbc$alk.phos,na.rm=TRUE), "Low", "High")
#' pbc$astg[!is.na(pbc$ast)]        <-
#'    ifelse(pbc$ast[!is.na(pbc$ast)]          <= median(pbc$ast,     na.rm=TRUE), "Low", "High")
#' pbc$bilig[!is.na(pbc$bili)]      <-
#'    ifelse(pbc$bili[!is.na(pbc$bili)]        <= median(pbc$bili,    na.rm=TRUE), "Low", "High")
#' pbc$cholg[!is.na(pbc$chol)]      <-
#'    ifelse(pbc$chol[!is.na(pbc$chol)]        <= median(pbc$chol,    na.rm=TRUE), "Low", "High")
#' pbc$copperg[!is.na(pbc$copper)]  <-
#'    ifelse(pbc$copper[!is.na(pbc$copper)]    <= median(pbc$copper,  na.rm=TRUE), "Low", "High")
#' #eliminate treatment NAs
#' pbcdat <- pbc[!is.na(pbc$trt), ]
#'# PFS and OS endpoints
#' set.seed(2006)
#' pbcdat$'event.pfs' <- sample(c(0,1),dim(pbcdat)[1],replace=TRUE)
#' pbcdat$'timepfs' <- sample(1:5000,dim(pbcdat)[1],replace=TRUE)
#' pbcdat$'event.os' <- pbcdat$event
#' pbcdat$'timeos' <- pbcdat$time
#'#variable importance for OS for the created categorical variables
#'#(higher is more important, also works for numeric variables)
#' varnames <- c('ageg', 'sex', 'bilig', 'cholg', 'astg', 'albuming', 'phosg')
#' # define function the eval_function()
#' # Attention: The eval_function ALWAYS needs to return a dataframe with one row.
#' #            Include exception handling, like if(N1>0 && N2>0) hr <- exp(coxph(...) )
#' #            to avoid program abort due to errors
#'hazardratio <- function(D) {
#'
#'  HRpfs <- tryCatch(exp(coxph(Surv(D$timepfs, D$event.pfs) ~ D$trt )$coefficients[[1]]),
#'   warning=function(w) {NA})
#'  HRpfs <- 1/HRpfs
#'  HR.pfs <- round(HRpfs, 2)
#'  HR.pfs[HR.pfs > 10]      <- 10
#'  HR.pfs[HR.pfs < 0.00001] <- 0.00001
#'  HRos <- tryCatch(exp(coxph(Surv(D$timeos, D$event.os) ~ D$trt )$coefficients[[1]]),
#'   warning=function(w) {NA})
#'  HRos <- 1/HRos
#'  HR.os <- round(HRos, 2)
#'  HR.os[HR.os > 10]      <- 10
#'  HR.os[HR.os < 0.00001] <- 0.00001
#'  data.frame( HR.pfs, HR.os#, N.of.subjects,N1 ,N2
#'  )
#'}
#'
#'  # run subscreen
#'
#' \dontrun{
#' results <- subscreencalc(
#'   data=pbcdat,
#'   eval_function=hazardratio,
#'   subjectid = "id",
#'   factors=c("ageg", "sex", "bilig", "cholg", "copperg"),
#'   use_complement = FALSE,
#'   factorial = FALSE
#' )
#'
#' # visualize the results of the subgroup screening with a Shiny app
#' subscreenshow(results)
#' }

subscreencalc <- function(
  data,
  eval_function,
  subjectid = "subjid",
  factors = NULL,
  max_comb = 3,
  nkernel = 1,
  par_functions = "",
  verbose = TRUE,
  factorial = FALSE,
  use_complement = FALSE,
  ...
) {

  #get parameter list from environment
  argg <- c(as.list(environment()), list(...))

  subscreencalc_notes <- c()

  #note for outdated min_comb parameter
  if ("min_comb" %in% names(argg)) {
    subscreencalc_notes <- c(subscreencalc_notes, "Note: parameter min_comb is no longer available (since version 4.0.0) and will be set to 1 by default.")
    min_comb <- 1
  } else {
    min_comb <- 1
  }

    #note for outdated min_comb parameter
  if ("treat" %in% names(argg)) {
    subscreencalc_notes <- c(subscreencalc_notes, "Note: parameter treat is no longer available (since version 4.0.0).")
    min_comb <- 1
  } else {
    min_comb <- 1
  }

  #note for outdated endpoints parameter (?)
  if ("endpoints" %in% names(argg)) {
    subscreencalc_notes <- c(subscreencalc_notes, "Note: parameter endpoints is no longer available (since version 4.0.0).")
  } else {

  }

  #Parameter: data
  if (!is.data.frame(data)) {
    stop("parameter data has to be a dataframe!")
  }
  if (dim(data)[1] == 0 || dim(data)[2] == 0) {
    stop("parameter data contains no rows or columns!")
  }

  #Parameter: factors
  if (is.null(factors)) {
    stop("parameter factor is NULL or an empty vector. Please change parameter factor to a non-empty character vector!")
  }
  if (!is.vector(factors)) {
    stop("parameter factors has to be a character vector!")
  }

  if ((length(factors) != sum(factors %in% names(data)))) {
    stop(
      paste0("The following variables in parameter factors are not included in the dataset: ", paste(factors[which(!factors %in% names(data))],collapse = ", "),". Please remove these factors or add them into your data!")
    )
  }
  #### WARNING & ERROR MESSAGES ####
  #Parameter: verbose, factorial, use_complement
  if (!is.logical(verbose) || is.na(verbose)) {
    stop("parameter verbose needs to be logical (TRUE/FALSE) and non-missing!")
  }

  if (!is.logical(factorial) || is.na(factorial)) {
    stop("parameter factorial needs to be logical (TRUE/FALSE) and non-missing!")
  }

  if (!is.logical(use_complement) || is.na(use_complement)) {
    stop("parameter use_complement needs to be logical (TRUE/FALSE) and non-missing!")
  }

  #Parameter: eval_function
  if (!is.function(eval_function) & !is.function(match.fun(eval_function))) {
    stop("parameter eval_function has to be a function name! ")
  } else if (!is.function(eval_function) & is.function(match.fun(eval_function))) {
    eval_function <- match.fun(eval_function)
  }
  if (is(tryCatch(eval_function(data), error=function(e) e, warning = function(w) w),"error")) {
    print(paste("error in calculation eval_function for overall data! Please check your eval_function!"))
    print(tryCatch(eval_function(data), error=function(e) e, warning = function(w) w))
  }

  if(any(is.na(eval_function(data))) & !all(is.na(eval_function(data)))){
    subscreencalc_notes <- c(subscreencalc_notes,
      paste0(
        "Note: overall calculation of endpoints : ",
        paste(names(eval_function(data)[which(is.na(eval_function(data)))]), collapse = ", "),
        " is/are NA. Therefore this/these endpoint(s) will not be applicable in the app!"
      )
    )
  }

  if(all(is.na(eval_function(data)))) {
    stop(
      "calculation of all endpoints (",
      paste(names(eval_function(data)[which(is.na(eval_function(data)))]), collapse = ", "),
      ") are NA. Please check your eval_function and/or dataset!"
    )
  }

  #Parameter: subjectid
  if (is.null(subjectid) || subjectid == "") {
    stop("parameter subjectid variable is empty!")
  }
  if (!(subjectid %in% names(data))) {
    stop(paste0("variable ",subjectid," for parameter subjectid is not in dataframe!"))
  }
  # if (sum(is.na(data[,subjectid])) != 0 || length(data[,subjectid]) != length(unique(data[,subjectid]))) {
  #   stop("variable '",subjectid,"' does not contain unique values")
  # }

  #Parameter: min_comb, max_comb
  if (length(factors) < max_comb) {

    subscreencalc_notes <- c(subscreencalc_notes,
      paste("Note: Number of factors is smaller than parameter max_comb. max_comb is therefore ", min(length(factors), max_comb),"!")
    )
    max_comb <- min(length(factors), max_comb)
  }
  if (length(factors) < min_comb) {
    subscreencalc_notes <- c(subscreencalc_notes,
      paste("Note: Number of factors is smaller than parameter min_comb. min_comb is therefore ", min(length(factors), min_comb),"!")
    )
    min_comb <- min(length(factors), min_comb)
  }
  if (!is.numeric(min_comb) || min_comb <= 0 || (min_comb %% 1) != 0) {
    stop("parameter min_comb has to be a integer number > 0!")
  }
  if (!is.numeric(max_comb) || max_comb <= 0 || (max_comb %% 1) != 0) {
    stop("parameter max_comb has to be a integer number > 0!")
  }
  if (min_comb > max_comb) {
    stop("parameter min_comb has to be less than or equal to max_comb!")
  }

  #Parameter: nkernel
  if(!is.numeric(nkernel)) {
    stop("parameter nkernel has to be numeric")
  }
  if (nkernel > 1) {
    if (!requireNamespace("parallel", quietly = TRUE)) {
      cat("use of more than one kernel requires package parallel to be installed. \n nkernel set to 1 \n")
    }
  }

  #### FUNCTIONS ####
  createCombinationMatrix <- function(n, k, l) {
    t(do.call(cbind, lapply(k:l, function(x) {
      utils::combn(n,x, tabulate, nbins = n)})))
  }

  sugruCount <- function(M, StAn) {
    return(
      apply(
        M,
        1,
        function(x) {
          x = x * StAn
          prod(x[x != 0])
        }
      )
    )
  }

  sugruCalc <- function(i) {
    m = M[i, ]
    S = character()
    S = append(S, names(FFF)[(1:length(m)) * m])
    # if (is(tryCatch(plyr::ddply(cbind(FFF, TTT), S, eval_function), error=function(e) e, warning = function(w) w),"error")) {
    #   print(paste("Error in calculation eval_function for subgroup factor(s combination): ", paste(S,collapse =",")))
    #   print(tryCatch(plyr::ddply(cbind(FFF, TTT), S, eval_function), error=function(e) e, warning = function(w) w))
    # }

    d = plyr::ddply(cbind(FFF, TTT), S, eval_function)
    if(use_complement == TRUE) {
      d_comp = plyr::ddply(cbind(FFF, TTT), S, function(x){eval_function(dplyr::anti_join(cbind(FFF,TTT), x, by = colnames(FFF)))})
      names(d_comp)[!names(d_comp) %in% S] <- paste0("Complement_",names(d_comp)[!names(d_comp) %in% S])
    }
    d_N <- plyr::ddply(cbind(FFF,TTT),S,function(x){
      N.of.subjects <- sum(!is.na(unique(x[subjectid])))
      data.frame(N.of.subjects)
    })
    d <- merge(d,d_N)
    if(use_complement == TRUE) {
      d <- merge(d,d_comp)
    }
    nfactors = sum(m)
    h = cbind(nfactors, d)
    return(h)
  }

  combineDataFrame <- function(h, version = 2) {
    if (version == 1) {
      hf = c("nfactors", names(FFF))
      ha = names(h[[1]])
      for (i in 2:dim(M)[1]) ha = union(ha, names(h[[i]]))
      ht = setdiff(ha, hf)
      hn = union(ht, hf)
      for (i in 1:dim(M)[1]) {
        for (j in hn) {
          if (sum(j == names(h[[i]])) == 0) {
            h[[i]][j] = rep(NA, dim(h[[i]])[1])
          }
        }
      }
      H = h[[1]]
      for (i in 2:dim(M)[1]) {
        H = rbind(H, h[[i]])
      }
    }
    if (version == 2) {

      #add FCID_all variable
      for( i in seq_along(h)){h[[i]]$FCID_all <- rep(i,nrow(h[[i]]))}

      H = as.data.frame(data.table::rbindlist(h, use.names = TRUE,
                                              fill = TRUE))

      for (fac in factors) {
        H[[fac]] <- addNA(H[[fac]])
        levels(H[[fac]])[is.na(levels(H[[fac]]))] <- "Not used"
      }
    }
    if (version == 3) {
      H = do.call("rbind", h)
    }
    SGID = 1:dim(H)[1]
    return(cbind(SGID, H))
  }

  TTT <- data[, (!colnames(data) %in% c(factors))]
  FFF <- data[, (colnames(data) %in% factors), drop = FALSE]
  if (verbose == TRUE) {
    cat("\n",
        "subscreencalc started at ", format(Sys.time(), format = "%F %R %Z"))
  }


  AnFa <- dim(FFF)[2]
  AaS <- dim(data)[1]
  StAn <- apply(FFF,2,function(x){length(unique(x))})
  pc <- StAn[StAn > 2]
  pt0 <- proc.time()

  M <- createCombinationMatrix(AnFa, min(length(factors),min_comb), min(length(factors),max_comb))
  AnSu <- sugruCount(M, StAn)
  pt1 <- proc.time()
  rowsM <- dim(M)[1]

  if (verbose == TRUE) {
    cat("\n", "Number of Subjects                     ",sum(!is.na(unique(cbind(TTT,FFF)[subjectid]))),
        "\n", "Number of Subgroup Factors             ",AnFa,
        "\n", "Potential Subgroups                    ",sum(AnSu))
    }

  if (nkernel > 1) {

    clus <- parallel::makeCluster(nkernel)
    parallel::clusterExport(clus, c("FFF", "TTT", "M", "AaS",
                                    "eval_function"), environment())
    parallel::clusterExport(clus, c("ddply"), environment(plyr::ddply))
    parallel::clusterExport(clus, c("use_complement","subjectid"), environment())
    if (all(par_functions != ""))
      parallel::clusterExport(clus, par_functions)
    h <- parallel::parLapplyLB(cl = clus, 1:rowsM, sugruCalc)
    parallel::stopCluster(clus)

  } else {
    h <- sapply(1:rowsM, sugruCalc, simplify = FALSE)
  }

  #create factorial context ids
  pc_ids <- lapply(h, function(x){any(colnames(x)%in% names(pc)) & all(x$nfactors > 1)})
  pc_max_levels <- lapply(h, function(x){max(StAn[names(StAn) %in% colnames(x)[which(colnames(x) %in% factors)]])})
  pc_df <- data.frame(max_level = unlist(pc_max_levels))
  pc_df$FCID_all <- 1:nrow(pc_df)
  z = numeric()

  for (i in 1:length(h)) z[i] = dim(h[[i]])[1]

  pt2 <- proc.time()

  if (verbose == TRUE) {
    cat("\n", "Non-existent/empty Subgroups           ",sum(AnSu - z),
        "\n", "Existent Subgroups                     ",sum(z),
        "\n\n", "Time for SG Analyses (s)               ", pt2 - pt1)
  }

  h <- lapply(h, function(x) {
    if (any(is.na(x[,colnames(x)[colnames(x) %in% factors]]))) {
      x[,colnames(x)[colnames(x) %in% factors]][is.na(x[,colnames(x)[colnames(x) %in% factors]])] <- "No data"
      x
    } else {
      x
    }
    if (any(x[,colnames(x)[colnames(x) %in% factors]] == ".")) {
      x[,colnames(x)[colnames(x) %in% factors]][x[,colnames(x)[colnames(x) %in% factors]]=="."] <- "No data"
      x
    } else {
      x
    }
    if (any(x[,colnames(x)[colnames(x) %in% factors]] == "")) {
      x[,colnames(x)[colnames(x) %in% factors]][x[,colnames(x)[colnames(x) %in% factors]]==""] <- "No data"
      x
    } else {
      x
    }
    x
    }
  )

  H <- combineDataFrame(h)
  H <- merge(H,pc_df,by="FCID_all")

  pt3 <- proc.time()

  if (verbose == TRUE) {
    cat("\n", "Time for creating Data Frames (s)      ", pt3 - pt2)}


  evfu <- eval_function(cbind(FFF, TTT))
  N <- data.frame('N.of.subjects' = sum(!is.na(unique(cbind(TTT,FFF)[subjectid]))))
  res <- merge(evfu, N)

  pt3a <- proc.time()

  if (verbose == TRUE) {
    cat("\n", "Time for calculating N.of.subjects (s) ", pt3a - pt3)
  }

  if (factorial == TRUE) {
    for (i in 1:length(colnames(evfu))) {
      H <- pseudo_contexts(data = H, endpoint = colnames(evfu)[i], factors = factors)
    }
    fc <- H[!is.na(H$SGID),]
  } else {
    fc <- H
  }

  pt4 <- proc.time()

  if (verbose == TRUE) {
    cat("\n", "Time for factorial context (s)         ", pt4 - pt3a,
        "\n", "Overall time used (HH:MM:SS)           ",
        paste(((pt4 - pt0)[3]/3600)%/%1, (((pt4 - pt0)[3]/60)%/%1)%%60,
              round((pt4 - pt0)[3]%%60, digits = 4), sep = ":"),
        "\n")
  }

  H <- list(sge = fc,
            min_comb = min_comb,
            max_comb = max_comb,
            subjectid = subjectid,
            factors = factors, results_total = res)
  class(H) <- "SubScreenResult"

  if (verbose == TRUE) {
    cat("\n",
        "subscreencalc stopped at ", format(Sys.time(), format = "%F %R %Z"), "\n")}

  ## note about infinite values
  # if (length(names(which(apply(H$sge[,names(H$results_total)],2,function(x) {(!all(is.finite(x[!is.na(x)])))})))) > 0) {
  #   subscreencalc_notes <- c(subscreencalc_notes,
  #     paste0(
  #       "Note: the following target variable(s) include infinite values (-Inf/Inf) : ",
  #       paste(names(which(apply(H$sge[,names(H$results_total)],2,function(x) {(!all(is.finite(x[!is.na(x)])))}))), collapse = ", "),
  #       ", which causes errors within the app and therefore won't be shown! Please check your eval_function to avoid infinite values or replace all infinite values with finite values! "
  #     )
  #   )
  # }
  if (!is.null(subscreencalc_notes)) {
    print(subscreencalc_notes)
  }
  H
}

