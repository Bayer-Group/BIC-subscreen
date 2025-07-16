smooth_funnel_bounds <- function(support_points, lower_bound, upper_bound, span = 0.25) {
  # LOESS smoothing as described in paper (span = 0.25)
  loess_lower <- loess(lower_bound ~ support_points, span = span)
  loess_upper <- loess(upper_bound ~ support_points, span = span)
  
  list(
    support_points = support_points,
    lower_smooth = predict(loess_lower),
    upper_smooth = predict(loess_upper),
    lower_raw = lower_bound,
    upper_raw = upper_bound
  )
}

create_funnel_summary <- function(smoothed_results, alpha) {
  # Create a summary data frame for easy plotting and analysis
  if (length(smoothed_results) == 0) return(NULL)
  
  # Use the first endpoint to get support points (they're the same for all)
  first_endpoint <- names(smoothed_results)[1]
  support_points <- smoothed_results[[first_endpoint]]$support_points
  
  # Initialize summary data frame
  summary_df <- data.frame(
    n = support_points,
    alpha = alpha
  )
  
  # Add bounds for each endpoint
  for (endpoint in names(smoothed_results)) {
    smooth_data <- smoothed_results[[endpoint]]
    
    # Add smoothed bounds
    summary_df[[paste0(endpoint, "_lower_smooth")]] <- smooth_data$lower_smooth
    summary_df[[paste0(endpoint, "_upper_smooth")]] <- smooth_data$upper_smooth
    
    # Optionally add raw bounds for comparison
    summary_df[[paste0(endpoint, "_lower_raw")]] <- smooth_data$lower_raw
    summary_df[[paste0(endpoint, "_upper_raw")]] <- smooth_data$upper_raw
  }
  
  return(summary_df)
}

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
    loess_span = 0.25,  # NEW: LOESS smoothing parameter
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
  
  # NEW: Apply LOESS smoothing to funnel bounds
  if (verbose == TRUE) {
    cat("\n", "Applying LOESS smoothing to funnel bounds...")
  }
  
  # Extract bounds for each endpoint
  endpoint_cols <- setdiff(names(tmp3), c("n", "alpha"))
  
  # Separate lower and upper bounds
  lower_bounds <- tmp3[tmp3$alpha == alpha/2, ]
  upper_bounds <- tmp3[tmp3$alpha == 1-(alpha/2), ]
  
  # Apply LOESS smoothing to each endpoint
  smoothed_results <- list()
  
  for (endpoint in endpoint_cols) {
    if (verbose == TRUE) {
      cat("\n", "  Smoothing endpoint:", endpoint)
    }
    
    # Get the bounds for this endpoint
    lower_vals <- lower_bounds[[endpoint]]
    upper_vals <- upper_bounds[[endpoint]]
    support_pts <- lower_bounds$n
    
    # Apply LOESS smoothing
    smoothed <- smooth_funnel_bounds(
      support_points = support_pts,
      lower_bound = lower_vals,
      upper_bound = upper_vals,
      span = loess_span
    )
    
    smoothed_results[[endpoint]] <- smoothed
  }
  
  # NEW: Store both raw quantiles and smoothed results
  H$funnel_quantiles <- data.frame(tmp3)  # Raw quantiles (original)
  H$funnel_smoothed <- smoothed_results   # Smoothed bounds (new)
  
  # NEW: Create a convenient summary data frame for plotting
  H$funnel_bounds <- create_funnel_summary(smoothed_results, alpha)
  
  pt2 <- Sys.time()
  if (verbose == TRUE) {
    cat("\n", "Time for funnel calculation(s): ", pt2 - pt1)
    cat("\n", "LOESS smoothing completed with span =", loess_span)
  }
  return(H)
}