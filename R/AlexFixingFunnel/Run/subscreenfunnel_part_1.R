subscreenfunnel_fix <- function(
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
  
  # CORRECTED: Permute treatment assignments
  generate_permuted_data <- function(data, treat, nperm) {
    lapply(1:nperm, function(i) {
      permuted_data <- data
      permuted_data[, treat] <- sample(data[, treat])  # Permute treatment labels
      return(permuted_data)
    })
  }
  
  # Generate permuted datasets
  all_samples <- generate_permuted_data(data_trimmed, treat, nperm)
  
  future::plan("multisession", workers = nkernel)
  
  # CORRECTED: Create proper index vectors and access list correctly
  perm_indices <- rep(1:nperm, length(nsamp))
  sample_sizes <- rep(as.vector(nsamp), each = nperm)
  
  tmp <- furrr::future_map2(perm_indices, sample_sizes, function(perm_idx, samp_size) {
    # Access the perm_idx-th permuted dataset and sample samp_size rows
    sampled_data <- dplyr::slice_sample(all_samples[[perm_idx]], n = samp_size)
    tmp1 <- data.frame(eval_function(sampled_data))
    tmp1$n <- samp_size
    tmp1
  }, .options=furrr::furrr_options(seed = TRUE))
  
  # FIXED: Replace rbind.fill with dplyr::bind_rows
  tmp2 <- dplyr::bind_rows(tmp)
  
  tmp3 <- tmp2 %>%
    dplyr::as_tibble() %>%
    dplyr::group_by(n) %>%
    dplyr::summarise_all(~quantile(., probs = c(alpha/2, 1-(alpha/2)), na.rm = TRUE)) %>%
    dplyr::mutate(alpha = c(alpha/2, 1-(alpha/2))) %>%
    dplyr::ungroup()
  
  H$funnel_quantiles <- data.frame(tmp3)
  pt2 <- Sys.time()
  if (verbose == TRUE) {
    cat("\n", "Time for funnel calculation(s): ", pt2 - pt1)
  }
  return(H)
}