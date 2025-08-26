#' Calculation of funnel shape for the subgroups explorer
#'
#' @param data data frame with study data
#' @param H results file from subscreencalc
#' @param eval_function eval function used in subscreencalc
#' @param min_start integer value for minimal subgroup size value for permutation
#' @param n_support_points integer value for number of supportive points
#' @param nperm integer value for number of permutations
#' @param alpha numerical value 
#' @param stratified logical value (TRUE/FALSE) for stratification
#' @param treat character value of treatment variable name
#' @param endpoints character vector of endpoints
#' @param verbose logical value to switch on/off output of computational information (defaults to TRUE)
#' @param nkernel integer value for number of kernel used
#' @param ... further parameters which where outdated used for notes and errors.
#' @return an object of type SubScreenResult of the form
#' @keywords subgroup analysis
#' @export subscreenfunnel
#' @importFrom rlang .data
#'

subscreenfunnel <- function(
  data,
  H,
  eval_function,
  min_start = 2,
  n_support_points = 50,
  nperm = 200,
  alpha = 0.001,
  stratified = TRUE,
  treat = "treat",
  endpoints = NULL,
  verbose = TRUE,
  nkernel = 1,
  ...
) {

  #### WARNING & ERROR MESSAGES ####
  #Parameter: verbose, factorial, use_complement
  if (!is.logical(verbose) || is.na(verbose)) {
    stop("parameter verbose needs to be logical (TRUE/FALSE) and non-missing!")
  }
  if (verbose == TRUE) {
    cat("\n",
      "subscreenfunnel started at ",
      format(Sys.time(),
        format = "%F %R %Z"
      )
    )
    pt1 <- Sys.time()
  }

  weightedSampler <- function(dat, treat = 'T', size = 10) {
    sizePerTrt <- round(size/length(trts))
    trt1 <- dplyr::slice_sample(dat[dat[,treat]== trts[1],] ,n =sizePerTrt)
    trt2 <- dplyr::slice_sample(dat[dat[,treat]== trts[2],] ,n =sizePerTrt)
    samp <- data.frame(matrix(ncol = ncol(dat), nrow = size))
    colnames(samp) <- colnames(dat)
    samp[2*(1:sizePerTrt),] <- trt1
    samp[(2*(1:sizePerTrt)-1),] <- trt2
    samp
  }

  sampsize <- H$results_total$N.of.subjects

  # Generate vector of support points between min and max sample sizes
  sqrtvec <- seq(
    sqrt(min_start),
    by = (sqrt(sampsize) - sqrt(min_start))/n_support_points,
    length.out = (n_support_points+1)
  )
  nsamp <- matrix(round(sqrtvec^2), nrow = 1)

  # First we only remove covariates, since they are not of interest,
  data_trimmed <- data[,c(treat, endpoints)]

  if (stratified) {
    trts <- unique(as.data.frame(data)[,treat])
    lowest_nr_subject_by_trt <- min(nrow(data[data[treat] == trts[1],]),nrow(data[data[treat] == trts[2],]))
    sampsize <- ifelse(
      sampsize > lowest_nr_subject_by_trt,
      lowest_nr_subject_by_trt,
      sampsize
    ) # ensure that max. sampsize does not exceed min. required sampsize for each trt level
    all_samples <- replicate(nperm, weightedSampler(data_trimmed, treat, sampsize))
  } else if (!stratified) {
    all_samples <- replicate(nperm, dplyr::slice_sample(data_trimmed, n = sampsize))
  }

  future::plan("multisession", workers = nkernel)

  tmp <- furrr::future_map2(rep(1:nperm,length(nsamp)), rep(nsamp, each = nperm), function(x, y) {
    tmp1 <- data.frame(eval_function(data.frame(all_samples[,x])[1:y,]))
    tmp1$n <- y
    tmp1
  }
  #
  , .options=furrr::furrr_options(seed = TRUE)
  )

  tmp2 <- do.call("rbind.fill", tmp)

  tmp3 <- tmp2 %>%
    dplyr::as_tibble() %>%
    dplyr::group_by(.data$n) %>%
    dplyr::summarise_all(list(quantile = ~ quantile(., probs = c(alpha/2, 1-(alpha/2)), na.rm = TRUE))) %>%
    dplyr::mutate(alpha = c(alpha/2, 1-(alpha/2))) %>%
    dplyr::ungroup()

  H$funnel_quantiles <- data.frame(tmp3)
  pt2 <- Sys.time()
  if (verbose == TRUE) {
    cat("\n", "Time for funnel calculation(s): ", pt2 - pt1)
  }
  return(H)
 }
