validate_funnel_parameters <- function(data, treat, endpoints, nperm, alpha, min_start = 2, verbose = TRUE) {
  
  # Check if data is provided and valid
  if (missing(data) || is.null(data) || !is.data.frame(data)) {
    stop("Parameter 'data' must be a non-empty data frame")
  }
  
  if (nrow(data) == 0) {
    stop("Data frame is empty")
  }
  
  # Check treatment column
  if (!treat %in% colnames(data)) {
    stop("Treatment column '", treat, "' not found in data. Available columns: ", 
         paste(colnames(data), collapse = ", "))
  }
  
  # Check treatment variable type and values
  treatment_var <- data[, treat]
  if (!is.factor(treatment_var) && !is.character(treatment_var) && !is.numeric(treatment_var)) {
    stop("Treatment variable must be factor, character, or numeric")
  }
  
  # Check treatment groups
  treatment_counts <- table(treatment_var, useNA = "ifany")
  unique_treatments <- length(treatment_counts)
  
  if (unique_treatments < 2) {
    stop("Treatment variable must have at least 2 different values. Found: ", unique_treatments)
  }
  
  if (any(is.na(names(treatment_counts)))) {
    warning("Treatment variable contains NA values. These will be excluded from analysis.")
  }
  
  # Check for small treatment groups
  min_group_size <- min(treatment_counts, na.rm = TRUE)
  if (min_group_size < 10) {
    warning("Small treatment groups detected (minimum size: ", min_group_size, 
            "). Results may be unstable. Consider minimum group size of 10.")
  }
  
  if (verbose) {
    cat("\n  Treatment groups:", paste(names(treatment_counts), "=", treatment_counts, collapse = ", "))
  }
  
  # Check endpoints
  if (!is.null(endpoints)) {
    missing_endpoints <- setdiff(endpoints, colnames(data))
    if (length(missing_endpoints) > 0) {
      stop("Endpoint columns not found in data: ", paste(missing_endpoints, collapse = ", "))
    }
    
    # Check for missing values in endpoints
    for (endpoint in endpoints) {
      na_count <- sum(is.na(data[, endpoint]))
      if (na_count > 0) {
        warning("Endpoint '", endpoint, "' has ", na_count, " missing values (", 
                round(na_count/nrow(data)*100, 1), "%)")
      }
    }
  }
  
  # Validate permutation parameters
  if (!is.numeric(nperm) || length(nperm) != 1 || nperm < 1) {
    stop("Parameter 'nperm' must be a positive integer")
  }
  
  if (nperm < 100) {
    stop("Parameter 'nperm' must be at least 100 for meaningful results")
  }
  
  if (nperm < 1000) {
    warning("nperm < 1000 may produce unstable confidence intervals. Paper recommends >= 1000. Current: ", nperm)
  }
  
  # Allow vector alpha but validate each element
  if (!is.numeric(alpha) || length(alpha) == 0 || any(alpha <= 0) || any(alpha >= 1)) {
    stop("Parameter 'alpha' must be numeric with all values between 0 and 1. Got: ", 
         paste(alpha, collapse = ", "))
  }
  
  if (any(duplicated(alpha))) {
    warning("Duplicate alpha values detected and will be removed")
    alpha <- unique(alpha)
  }
  
  # Validate minimum start size
  if (!is.numeric(min_start) || length(min_start) != 1 || min_start < 2 || min_start != round(min_start)) {
    stop("Parameter 'min_start' must be an integer >= 2")
  }
  
  if (min_start > nrow(data)/4) {
    warning("min_start (", min_start, ") is large relative to data size (", nrow(data), 
            "). This may limit the funnel plot range.")
  }
  
  # Check data size relative to parameters
  if (nrow(data) < min_start * 2) {
    stop("Data size (", nrow(data), ") is too small for min_start = ", min_start, 
         ". Need at least ", min_start * 2, " observations.")
  }
  
  if (verbose) {
    cat("\n  ✓ All parameters validated successfully")
    cat("\n  Data size:", nrow(data), "observations")
    cat("\n  Permutations:", nperm)
    cat("\n  Alpha level:", alpha)
  }
  
  return(TRUE)
}

generate_support_points <- function(min_size, max_size, n_points = 50, verbose = TRUE) {
  
  # Validate inputs
  if (!is.numeric(min_size) || !is.numeric(max_size) || !is.numeric(n_points)) {
    stop("All parameters must be numeric")
  }
  
  if (length(min_size) != 1 || length(max_size) != 1 || length(n_points) != 1) {
    stop("All parameters must be single values")
  }
  
  if (min_size < 2) {
    stop("min_size must be at least 2")
  }
  
  if (max_size <= min_size) {
    stop("max_size (", max_size, ") must be greater than min_size (", min_size, ")")
  }
  
  if (n_points < 10) {
    stop("n_points must be at least 10 for meaningful funnel plot")
  }
  
  if (n_points > 100) {
    warning("n_points > 100 may be computationally expensive. Consider reducing.")
  }
  
  # Paper specifies exactly 50 points, not 51 (fix off-by-one error)
  sqrt_min <- sqrt(max(min_size, 4))
  sqrt_max <- sqrt(max_size)
  
  # Generate exactly n_points in sqrt space
  sqrt_seq <- seq(sqrt_min, sqrt_max, length.out = n_points)
  support_points <- round(sqrt_seq^2)
  
  # Handle duplicates while maintaining count
  support_points <- unique(pmax(support_points, max(min_size, 4)))
  
  # If we lost points due to duplicates, regenerate with slight adjustment
  if (length(support_points) < n_points * 0.9) {
    # Use linear space as fallback
    support_points <- unique(round(seq(min_size, max_size, length.out = n_points)))
    warning("Switched to linear spacing due to many duplicates in sqrt spacing")
  }
  
  return(sort(support_points))
}

preflight_check <- function(data, H, eval_function, treat, endpoints, nperm, alpha, min_start, verbose = TRUE) {
  
  if (verbose) {
    cat("\n", paste(rep("=", 50), collapse = ""))
    cat("\n", "SUBSCREENFUNNEL PRE-FLIGHT CHECK")
    cat("\n", paste(rep("=", 50), collapse = ""))
  }
  
  # 1. Basic parameter validation
  validate_funnel_parameters(data, treat, endpoints, nperm, alpha, min_start, verbose)
  
  # 2. Check H object
  if (missing(H) || is.null(H)) {
    stop("Parameter 'H' is required")
  }
  
  if (!"results_total" %in% names(H)) {
    stop("H object must contain 'results_total' component")
  }
  
  if (!"N.of.subjects" %in% names(H$results_total) || !is.numeric(H$results_total$N.of.subjects)) {
    stop("H$results_total must contain 'N.of.subjects'")
  }
  
  # 3. Test eval_function
  if (missing(eval_function) || !is.function(eval_function)) {
    stop("eval_function must be a function")
  }
  
  # Test eval_function with small sample
  test_size <- min(10, nrow(data))
  test_data <- data[1:test_size, c(treat, endpoints)]
  
  tryCatch({
    test_result <- eval_function(test_data)
    
    if (!is.list(test_result) && !is.data.frame(test_result) && !is.numeric(test_result)) {
      stop("eval_function must return a list, data.frame, or numeric vector")
    }
    
    if (verbose) {
      cat("\n  ✓ eval_function test successful")
      cat("\n    Returns:", class(test_result)[1])
      if (is.list(test_result) || is.data.frame(test_result)) {
        cat("\n    Elements:", paste(names(test_result), collapse = ", "))
      }
    }
    
  }, error = function(e) {
    stop("eval_function failed on test data: ", e$message)
  })
  
  # Estimate computation time
  estimated_operations <- nrow(data) * nperm * 50  # Rough estimate
  if (estimated_operations > 1e8) {
    warning("Large computation expected. Consider reducing nperm or data size. Estimated operations: ", 
            format(estimated_operations, scientific = TRUE))
  }
  
  if (verbose) {
    cat("\n", paste(rep("=", 50), collapse = ""))
    cat("\n", "✓ PRE-FLIGHT CHECK COMPLETED SUCCESSFULLY")
    cat("\n", paste(rep("=", 50), collapse = ""))
  }
  
  return(TRUE)
}

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
    skip_validation = FALSE,  # NEW: Allow skipping validation for advanced users
    ...
) {
  
  #### PRE-FLIGHT VALIDATION ####
  if (!skip_validation) {
    preflight_check(data, H, eval_function, treat, endpoints, nperm, alpha, min_start, verbose)
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
  
  # Generate vector of support points with validation
  support_points <- generate_support_points(
    min_size = min_start,
    max_size = sampsize,
    n_points = n_support_points,
    verbose = verbose
  )
  
  if (length(support_points) != n_support_points) {
    stop("Generated ", length(support_points), " support points instead of requested ", 
            n_support_points, " due to duplicates or constraints")
  }
  
  nsamp <- matrix(support_points, nrow = 1)
  
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