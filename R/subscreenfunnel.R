#' Calculation of funnel shape for the subgroups explorer
#'
#' Builds reference funnel bounds under a homogeneous treatment-effect
#' assumption by drawing random size-\code{n} subsets (keeping outcome and
#' treatment pairs intact), computing endpoint quantiles at each support
#' point, and smoothing the resulting bounds with LOESS.
#'
#' @param data data frame with study data
#' @param H results file from subscreencalc
#' @param eval_function eval function used in subscreencalc
#' @param min_start integer value for minimal subgroup size value for permutation
#' @param n_support_points integer value for number of supportive points
#' @param nperm integer value for number of permutations
#' @param alpha numerical value
#' @param stratified If \code{FALSE} (default), draw size-\code{n} subsets at
#'   random from the full data. If \code{TRUE}, draw equal counts from each
#'   treatment arm (balanced resampling). Both keep treatment and endpoints
#'   intact.
#' @param treat character value of treatment variable name
#' @param endpoints character vector of endpoints
#' @param verbose logical value to switch on/off output of computational information (defaults to TRUE)
#' @param nkernel integer value for number of kernel used
#' @param loess_span LOESS span for smoothing funnel bounds (default 0.5).
#' @param skip_validation Skip pre-flight parameter checks (default FALSE).
#' @param ... further parameters which where outdated used for notes and errors.
#' @return \code{H} with \code{funnel_quantiles} (raw), \code{funnel_smoothed},
#'   and \code{funnel_bounds}.
#' @keywords subgroup analysis
#' @export subscreenfunnel

subscreenfunnel <- function(
  data,
  H,
  eval_function,
  min_start = 2,
  n_support_points = 50,
  nperm = 200,
  alpha = 0.001,
  stratified = FALSE,
  treat = "treat",
  endpoints = NULL,
  verbose = TRUE,
  nkernel = 1,
  loess_span = 0.5,
  skip_validation = FALSE,
  ...
) {
  #### WARNING & ERROR MESSAGES ####
  #Parameter: verbose, factorial, use_complement
  if (!is.logical(verbose) || is.na(verbose)) {
    stop("parameter verbose needs to be logical (TRUE/FALSE) and non-missing!")
  }
  if (!skip_validation) {
    funnel_preflight_check(
      data,
      H,
      eval_function,
      treat,
      endpoints,
      nperm,
      alpha,
      min_start,
      stratified = stratified,
      verbose = verbose
    )
  }

  if (verbose) {
    cat(
      "\n",
      "subscreenfunnel started at ",
      format(Sys.time(), format = "%F %R %Z")
    )
    pt1 <- Sys.time()
  }

  sampsize <- H$results_total$N.of.subjects
  # First we only remove covariates, since they are not of interest,
  data_trimmed <- data[, c(treat, endpoints)]
  trts <- unique(as.data.frame(data)[, treat])
  trts <- trts[!is.na(trts)]

  if (isTRUE(stratified)) {
    lowest_nr_subject_by_trt <- min(
      nrow(data[data[treat] == trts[1], ]),
      nrow(data[data[treat] == trts[2], ])
    )
    sampsize <- min(sampsize, lowest_nr_subject_by_trt)
  }

  support_points <- funnel_generate_support_points(
    min_size = min_start,
    max_size = sampsize,
    n_points = n_support_points,
    verbose = verbose
  )

  if (length(support_points) != n_support_points) {
    stop(
      "Generated ",
      length(support_points),
      " support points instead of requested ",
      n_support_points,
      " due to duplicates or constraints"
    )
  }

  nsamp <- matrix(support_points, nrow = 1)

  weighted_sampler <- function(dat, treat_var, trt_levels, size) {
    size_per_trt <- round(size / length(trt_levels))
    trt1 <- dplyr::slice_sample(
      dat[dat[, treat_var] == trt_levels[1], ],
      n = size_per_trt
    )
    trt2 <- dplyr::slice_sample(
      dat[dat[, treat_var] == trt_levels[2], ],
      n = size_per_trt
    )
    samp <- data.frame(matrix(ncol = ncol(dat), nrow = size))
    colnames(samp) <- colnames(dat)
    samp[2 * (1:size_per_trt), ] <- trt1
    samp[(2 * (1:size_per_trt) - 1), ] <- trt2
    samp
  }

  future::plan("multisession", workers = nkernel)

  perm_indices <- rep(seq_len(nperm), length(nsamp))
  sample_sizes <- rep(as.vector(nsamp), each = nperm)

  tmp <- furrr::future_map2(
    perm_indices,
    sample_sizes,
    function(perm_idx, samp_size) {
      if (isTRUE(stratified)) {
        sampled_data <- weighted_sampler(
          data_trimmed,
          treat,
          trts,
          samp_size
        )
      } else {
        sampled_data <- dplyr::slice_sample(data_trimmed, n = samp_size)
      }
      tmp1 <- data.frame(eval_function(sampled_data))
      tmp1$n <- samp_size
      tmp1
    },
    .options = furrr::furrr_options(seed = TRUE)
  )

  tmp2 <- dplyr::bind_rows(tmp)

  endpoint_cols <- setdiff(names(tmp2), "n")
  tmp3 <- tmp2 %>%
    dplyr::as_tibble() %>%
    dplyr::group_by(.data$n) %>%
    dplyr::reframe(dplyr::across(
      dplyr::all_of(endpoint_cols),
      function(x) {
        stats::quantile(
          x,
          probs = c(alpha / 2, 1 - (alpha / 2)),
          na.rm = TRUE
        )
      }
    )) %>%
    dplyr::group_by(.data$n) %>%
    dplyr::mutate(alpha = c(alpha / 2, 1 - (alpha / 2))) %>%
    dplyr::ungroup()

  if (verbose) {
    cat("\n", "Applying LOESS smoothing to funnel bounds...")
  }

  smoothed_results <- list()

  for (a in alpha) {
    lower_bounds <- tmp3[tmp3$alpha == a / 2, , drop = FALSE]
    upper_bounds <- tmp3[tmp3$alpha == 1 - (a / 2), , drop = FALSE]
    support_pts <- lower_bounds$n

    for (endpoint in endpoint_cols) {
      if (verbose) {
        cat("\n", "  Smoothing endpoint:", endpoint, "alpha =", a)
      }

      smoothed_key <- if (length(alpha) == 1L) {
        endpoint
      } else {
        paste0(endpoint, "_alpha_", a)
      }

      smoothed_results[[smoothed_key]] <- funnel_smooth_bounds(
        support_points = support_pts,
        lower_bound = lower_bounds[[endpoint]],
        upper_bound = upper_bounds[[endpoint]],
        span = loess_span
      )
    }
  }

  H$funnel_quantiles <- data.frame(tmp3)
  H$funnel_smoothed <- smoothed_results
  H$funnel_bounds <- funnel_create_summary(smoothed_results, alpha)

  if (verbose) {
    pt2 <- Sys.time()
    cat("\n", "Time for funnel calculation(s): ", pt2 - pt1)
    cat("\n", "LOESS smoothing completed with span =", loess_span)
  }

  H
}

funnel_validate_parameters <- function(
  data,
  treat,
  endpoints,
  nperm,
  alpha,
  min_start = 2,
  stratified = FALSE,
  verbose = TRUE
) {
  if (missing(data) || is.null(data) || !is.data.frame(data)) {
    stop("Parameter 'data' must be a non-empty data frame")
  }

  if (nrow(data) == 0) {
    stop("Data frame is empty")
  }

  if (!is.logical(stratified) || length(stratified) != 1 || is.na(stratified)) {
    stop("Parameter 'stratified' must be logical TRUE or FALSE")
  }

  if (!treat %in% colnames(data)) {
    stop(
      "Treatment column '",
      treat,
      "' not found in data. Available columns: ",
      paste(colnames(data), collapse = ", ")
    )
  }

  treatment_var <- data[, treat]
  if (
    !is.factor(treatment_var) &&
      !is.character(treatment_var) &&
      !is.numeric(treatment_var)
  ) {
    stop("Treatment variable must be factor, character, or numeric")
  }

  treatment_counts <- table(treatment_var, useNA = "ifany")
  unique_treatments <- length(treatment_counts)

  if (unique_treatments < 2) {
    stop(
      "Treatment variable must have at least 2 different values. Found: ",
      unique_treatments
    )
  }

  if (isTRUE(stratified) && unique_treatments != 2) {
    stop(
      "stratified = TRUE requires exactly 2 treatment levels. Found: ",
      unique_treatments
    )
  }

  if (anyNA(names(treatment_counts))) {
    warning(
      "Treatment variable contains NA values. These will be excluded from analysis."
    )
  }

  min_group_size <- min(treatment_counts, na.rm = TRUE)
  if (min_group_size < 10) {
    warning(
      "Small treatment groups detected (minimum size: ",
      min_group_size,
      "). Results may be unstable. Consider minimum group size of 10."
    )
  }

  if (verbose) {
    cat(
      "\n  Treatment groups:",
      paste(names(treatment_counts), "=", treatment_counts, collapse = ", ")
    )
  }

  if (!is.null(endpoints)) {
    missing_endpoints <- setdiff(endpoints, colnames(data))
    if (length(missing_endpoints) > 0) {
      stop(
        "Endpoint columns not found in data: ",
        paste(missing_endpoints, collapse = ", ")
      )
    }

    for (endpoint in endpoints) {
      na_count <- sum(is.na(data[, endpoint]))
      if (na_count > 0) {
        warning(
          "Endpoint '",
          endpoint,
          "' has ",
          na_count,
          " missing values (",
          round(na_count / nrow(data) * 100, 1),
          "%)"
        )
      }
    }
  }

  if (!is.numeric(nperm) || length(nperm) != 1 || nperm < 1) {
    stop("Parameter 'nperm' must be a positive integer")
  }

  if (nperm < 100) {
    stop("Parameter 'nperm' must be at least 100 for meaningful results")
  }

  if (nperm < 1000) {
    warning(
      "nperm < 1000 may produce unstable confidence intervals. ",
      "Paper recommends >= 1000. Current: ",
      nperm
    )
  }

  if (
    !is.numeric(alpha) ||
      length(alpha) == 0 ||
      any(alpha <= 0) ||
      any(alpha >= 1)
  ) {
    stop(
      "Parameter 'alpha' must be numeric with all values between 0 and 1. Got: ",
      paste(alpha, collapse = ", ")
    )
  }

  if (anyDuplicated(alpha) > 0) {
    warning("Duplicate alpha values detected and will be removed")
    alpha <- unique(alpha)
  }

  if (
    !is.numeric(min_start) ||
      length(min_start) != 1 ||
      min_start < 2 ||
      min_start != round(min_start)
  ) {
    stop("Parameter 'min_start' must be an integer >= 2")
  }

  if (min_start > nrow(data) / 4) {
    warning(
      "min_start (",
      min_start,
      ") is large relative to data size (",
      nrow(data),
      "). This may limit the funnel plot range."
    )
  }

  if (nrow(data) < min_start * 2) {
    stop(
      "Data size (",
      nrow(data),
      ") is too small for min_start = ",
      min_start,
      ". Need at least ",
      min_start * 2,
      " observations."
    )
  }

  if (verbose) {
    cat("\n  All parameters validated successfully")
    cat("\n  Data size:", nrow(data), "observations")
    cat("\n  Permutations:", nperm)
    cat("\n  Alpha level:", paste(alpha, collapse = ", "))
    cat("\n  Stratified:", stratified)
  }

  TRUE
}

funnel_generate_support_points <- function(
  min_size,
  max_size,
  n_points = 50,
  verbose = TRUE
) {
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
    stop(
      "max_size (",
      max_size,
      ") must be greater than min_size (",
      min_size,
      ")"
    )
  }

  if (n_points < 10) {
    stop("n_points must be at least 10 for meaningful funnel plot")
  }

  if (n_points > 100) {
    warning(
      "n_points > 100 may be computationally expensive. Consider reducing."
    )
  }

  sqrt_min <- sqrt(max(min_size, 4))
  sqrt_max <- sqrt(max_size)
  sqrt_seq <- seq(sqrt_min, sqrt_max, length.out = n_points)
  support_points <- unique(pmax(round(sqrt_seq^2), max(min_size, 4)))

  if (length(support_points) < n_points * 0.9) {
    support_points <- unique(round(seq(
      min_size,
      max_size,
      length.out = n_points
    )))
    if (verbose) {
      warning(
        "Switched to linear spacing due to many duplicates in sqrt spacing"
      )
    }
  }

  support_points <- support_points[
    support_points >= min_size & support_points <= max_size
  ]
  ideal <- unique(round(seq(min_size, max_size, length.out = n_points)))
  pool <- sort(unique(c(support_points, ideal)))

  if (length(pool) < n_points) {
    pool <- sort(unique(c(pool, seq(min_size, max_size))))
  }

  if (length(pool) > n_points) {
    idx <- unique(round(seq(1, length(pool), length.out = n_points)))
    support_points <- sort(pool[idx])
  } else {
    support_points <- pool
  }

  sort(support_points)
}

funnel_preflight_check <- function(
  data,
  H,
  eval_function,
  treat,
  endpoints,
  nperm,
  alpha,
  min_start,
  stratified = FALSE,
  verbose = TRUE
) {
  if (verbose) {
    cat("\n", paste(rep("=", 50), collapse = ""))
    cat("\n", "SUBSCREENFUNNEL PRE-FLIGHT CHECK")
    cat("\n", paste(rep("=", 50), collapse = ""))
  }

  funnel_validate_parameters(
    data,
    treat,
    endpoints,
    nperm,
    alpha,
    min_start,
    stratified = stratified,
    verbose = verbose
  )

  if (missing(H) || is.null(H)) {
    stop("Parameter 'H' is required")
  }

  if (!"results_total" %in% names(H)) {
    stop("H object must contain 'results_total' component")
  }

  if (
    !"N.of.subjects" %in% names(H$results_total) ||
      !is.numeric(H$results_total$N.of.subjects)
  ) {
    stop("H$results_total must contain 'N.of.subjects'")
  }

  if (missing(eval_function) || !is.function(eval_function)) {
    stop("eval_function must be a function")
  }

  test_size <- min(10, nrow(data))
  test_data <- data[seq_len(test_size), c(treat, endpoints)]

  tryCatch(
    {
      test_result <- eval_function(test_data)

      if (
        !is.list(test_result) &&
          !is.data.frame(test_result) &&
          !is.numeric(test_result)
      ) {
        stop("eval_function must return a list, data.frame, or numeric vector")
      }

      if (verbose) {
        cat("\n  eval_function test successful")
        cat("\n    Returns:", class(test_result)[1])
        if (is.list(test_result) || is.data.frame(test_result)) {
          cat("\n    Elements:", paste(names(test_result), collapse = ", "))
        }
      }
    },
    error = function(e) {
      stop("eval_function failed on test data: ", e$message)
    }
  )

  estimated_operations <- nrow(data) * nperm * 50
  if (estimated_operations > 1e8) {
    warning(
      "Large computation expected. Consider reducing nperm or data size. ",
      "Estimated operations: ",
      format(estimated_operations, scientific = TRUE)
    )
  }

  if (verbose) {
    cat("\n", paste(rep("=", 50), collapse = ""))
    cat("\n", "PRE-FLIGHT CHECK COMPLETED SUCCESSFULLY")
    cat("\n", paste(rep("=", 50), collapse = ""))
  }

  TRUE
}

funnel_smooth_bounds <- function(
  support_points,
  lower_bound,
  upper_bound,
  span = 0.25
) {
  loess_lower <- stats::loess(lower_bound ~ support_points, span = span)
  loess_upper <- stats::loess(upper_bound ~ support_points, span = span)

  list(
    support_points = support_points,
    lower_smooth = stats::predict(loess_lower),
    upper_smooth = stats::predict(loess_upper),
    lower_raw = lower_bound,
    upper_raw = upper_bound
  )
}

funnel_create_summary <- function(smoothed_results, alpha) {
  if (length(smoothed_results) == 0) {
    return(NULL)
  }

  summaries <- lapply(alpha, function(a) {
    if (length(alpha) == 1L) {
      keys <- names(smoothed_results)
    } else {
      suffix <- paste0("_alpha_", a)
      keys <- names(smoothed_results)[
        endsWith(names(smoothed_results), suffix)
      ]
    }

    if (length(keys) == 0) {
      return(NULL)
    }

    support_points <- smoothed_results[[keys[1]]]$support_points
    summary_df <- data.frame(n = support_points, alpha = a)

    for (key in keys) {
      endpoint <- if (length(alpha) == 1L) {
        key
      } else {
        strsplit(key, "_alpha_", fixed = TRUE)[[1]][1]
      }
      smooth_data <- smoothed_results[[key]]

      summary_df[[paste0(
        endpoint,
        "_lower_smooth"
      )]] <- smooth_data$lower_smooth
      summary_df[[paste0(
        endpoint,
        "_upper_smooth"
      )]] <- smooth_data$upper_smooth
      summary_df[[paste0(endpoint, "_lower_raw")]] <- smooth_data$lower_raw
      summary_df[[paste0(endpoint, "_upper_raw")]] <- smooth_data$upper_raw
    }

    summary_df
  })

  dplyr::bind_rows(summaries)
}
