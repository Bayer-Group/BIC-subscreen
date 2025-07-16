#'Create example data for subscreen package with pbc data set
#'
#'

library(survival)
library(dplyr)
library(ggplot2)
library("subscreen")
source("C:/Temp8600/BIC-subscreen/Documents/Funnel/Run/subscreenfunnel_part_2.R")

utils::data(pbc, package = "survival")
##generate categorical versions of some of the baseline covariates
pbc <- pbc %>%
  dplyr::mutate(
    ageg = dplyr::case_when(
      age <= quantile(pbc$age, 0.33, na.rm = TRUE) ~ "Low",
      age > quantile(pbc$age, 0.33, na.rm = TRUE) & age <= quantile(pbc$age, 0.66, na.rm = TRUE) ~"Middle",
      age > quantile(pbc$age, 0.66, na.rm = TRUE) ~ "High",
      TRUE ~ "No data"
    ),
    phosg = dplyr::case_when(
      alk.phos<= quantile(pbc$alk.phos, 0.5, na.rm = TRUE) ~ "Low",
      alk.phos > quantile(pbc$alk.phos, 0.5, na.rm = TRUE) ~ "High",
      TRUE ~ "No data"
    ),
    albuming = dplyr::case_when(
      albumin<= quantile(pbc$albumin, 0.33, na.rm = TRUE) ~ "Low",
      albumin > quantile(pbc$albumin, 0.33, na.rm = TRUE) & albumin <= quantile(pbc$albumin, 0.66, na.rm = TRUE) ~"Middle",
      albumin > quantile(pbc$albumin, 0.66, na.rm = TRUE) ~ "High",
      TRUE ~ "No data"
    ),
    astg = dplyr::case_when(
      ast<= quantile(pbc$ast, 0.33, na.rm = TRUE) ~ "Low",
      ast > quantile(pbc$ast, 0.33, na.rm = TRUE) & ast<= quantile(pbc$ast, 0.66, na.rm = TRUE) ~"Middle",
      ast > quantile(pbc$ast, 0.66, na.rm = TRUE) ~ "High",
      TRUE ~ "No data"
    ),
    bilig = dplyr::case_when(
      bili<= quantile(pbc$bili, 0.33, na.rm = TRUE) ~ "Low",
      bili > quantile(pbc$bili, 0.33, na.rm = TRUE) & bili<= quantile(pbc$bili, 0.66, na.rm = TRUE) ~"Middle",
      bili > quantile(pbc$bili, 0.66, na.rm = TRUE) ~ "High",
      TRUE ~ "No data"
    ),
    cholg = dplyr::case_when(
      chol<= quantile(pbc$chol, 0.5, na.rm = TRUE) ~ "Low",
      chol > quantile(pbc$chol, 0.5, na.rm = TRUE) ~ "High",
      TRUE ~ "No data"
    ),
    copperg = dplyr::case_when(
      copper<= quantile(pbc$copper, 0.33, na.rm = TRUE) ~ "Low",
      copper > quantile(pbc$copper, 0.33, na.rm = TRUE) & copper<= quantile(pbc$copper, 0.66, na.rm = TRUE) ~ "Middle",
      copper > quantile(pbc$copper, 0.66, na.rm = TRUE) ~ "High",
      TRUE ~ "No data"
    ),
    ascitesg = dplyr::case_when(
      ascites == 1 ~ "Yes",
      ascites == 0  ~ "No",
      TRUE ~ "No data"
    ),
    plateletg = dplyr::case_when(
      platelet<= quantile(pbc$platelets, 0.5, na.rm = TRUE) ~ "Low",
      platelet > quantile(pbc$platelet, 0.5, na.rm = TRUE) ~ "High",
      TRUE ~ "No data"
    ),
    spidersg = dplyr::case_when(
      spiders == 1 ~ "Yes",
      spiders == 0  ~ "No",
      TRUE ~ "No data"
    )
  )
## # eliminate treatment NAs#

pbcdat <- pbc[!is.na(pbc$trt), ]
# # PFS and OS endpoints
set.seed(2006)##
pbcdat$'event.pfs' <- sample(c(0, 1), dim(pbcdat)[1], replace = TRUE)
pbcdat$'timepfs' <- sample(1:5000, dim(pbcdat)[1], replace = TRUE)#
pbcdat$'event.os' <- pbcdat$event
pbcdat$'timeos' <- pbcdat$time##
#variable importance for OS for the created categorical variables
# (higher is more important, also works for numeric variables)

#define function the eval_function()

#Attention: The eval_function ALWAYS needs to return a dataframe with one row.
#            Include exception handling, like if(N1>0 && N2>0) hr <- exp(coxph(...) )#
#           to avoid program abort due to errors
hazardratio <- function(D) {
  HRpfs <- tryCatch(
    exp(coxph(Surv(D$timepfs, D$event.pfs) ~ D$trt )$coefficients[[1]]),
    warning = function(w) {NA}
  )
  HRpfs <- 1/HRpfs
  HR.pfs <- round(HRpfs, 2)
  HR.pfs[HR.pfs > 10]      <- 10
  HR.pfs[HR.pfs < 0.00001] <- 0.00001
  HRos <- tryCatch(
    exp(coxph(Surv(D$timeos, D$event.os) ~ D$trt )$coefficients[[1]]),
    warning = function(w) {NA}
  )
  HRos <- 1/HRos
  HR.os <- round(HRos, 2)
  HR.os[HR.os > 10]      <- 10
  HR.os[HR.os < 0.00001] <- 0.00001
  data.frame(
    HR.pfs,
    HR.os
  )
}
# # Variable importance
pbcdat$status <- ifelse(pbcdat$status == 0, 0, 1)
importance <- subscreenvi(
  data = pbcdat,
  y = 'time',
  cens = 'status',
  trt = 'trt',
  x = c("sex", "ageg", "phosg",
        "albuming", "astg", "bilig",
        "cholg", "copperg", "ascitesg",
        "plateletg","spidersg"
  )
)

results <- subscreencalc(
  data = pbcdat,
  eval_function = hazardratio,
  subjectid = "id",
  factors = c("sex", "ageg", "phosg",
              "albuming", "astg", "bilig",
              "cholg", "copperg", "ascitesg",
              "plateletg","spidersg"
  ),
  use_complement = FALSE,
  factorial = FALSE)

results_factorial_true <- subscreencalc(
  data = pbcdat,
  eval_function = hazardratio,
  subjectid = "id",
  factors = c("sex", "ageg", "phosg",
              "albuming", "astg", "bilig",
              "cholg", "copperg", "ascitesg",
              "plateletg","spidersg"
  ),
  use_complement = FALSE,
  factorial = TRUE
)

results_factorial_complement_true1 <- subscreencalc(
  data = pbcdat,
  eval_function = hazardratio,
  subjectid = "id",
  factors = c("sex", "ageg", "phosg",
              "albuming", "astg", "bilig",
              "cholg", "copperg", "ascitesg",
              "plateletg","spidersg"
  ),
  use_complement = TRUE,
  factorial = TRUE
)

########################################################

results_factorial_complement_true_fix <- subscreenfunnel_fix(
  data = pbcdat,
  H = results_factorial_complement_true1,
  eval_function = hazardratio,
  min_start = 15,
  n_support_points = 25,
  nperm = 1500,
  alpha = c(0.05,0.1),
  treat = "trt",
  endpoints = c("timepfs" , "event.pfs", "timeos", "event.os")
)

# Access results
raw_quantiles <- results_factorial_complement_true_fix$funnel_quantiles      # Original quantiles
smoothed_bounds <- results_factorial_complement_true_fix$funnel_smoothed     # Detailed smoothed results
summary_bounds <- results_factorial_complement_true_fix$funnel_bounds        # Convenient summary for plotting

########################################################

plot_funnel <- function(H, endpoint = NULL) {
  if (is.null(H$funnel_bounds)) {
    stop("No funnel bounds found. Run subscreenfunnel_fix first.")
  }
  
  bounds_df <- H$funnel_bounds
  
  # Auto-detect endpoint if not specified
  if (is.null(endpoint)) {
    endpoint_cols <- grep("_lower_smooth$", names(bounds_df), value = TRUE)
    if (length(endpoint_cols) == 0) {
      stop("No smoothed bounds found in the data")
    }
    endpoint <- gsub("_lower_smooth$", "", endpoint_cols[1])
    message("Using endpoint: ", endpoint)
  }
  
  # Check if the endpoint columns exist
  lower_col <- paste0(endpoint, "_lower_smooth")
  upper_col <- paste0(endpoint, "_upper_smooth")
  lower_raw_col <- paste0(endpoint, "_lower_raw")
  upper_raw_col <- paste0(endpoint, "_upper_raw")
  
  if (!all(c(lower_col, upper_col) %in% names(bounds_df))) {
    stop("Endpoint '", endpoint, "' not found in smoothed results")
  }
  
  library(ggplot2)
  
  p <- ggplot(bounds_df, aes(x = n)) +
    # Smoothed bounds (ribbon and lines)
    geom_ribbon(
      aes_string(ymin = lower_col, ymax = upper_col),
      alpha = 0.3, fill = "blue"
    ) +
    geom_line(
      aes_string(y = lower_col),
      color = "blue", size = 1, linetype = "solid"
    ) +
    geom_line(
      aes_string(y = upper_col),
      color = "blue", size = 1, linetype = "solid"
    ) +
    labs(
      title = paste("LOESS Smoothed Funnel Plot for", endpoint),
      subtitle = paste("Blue: LOESS smoothed bounds (span = 0.25)"),
      x = "Sample Size (n)",
      y = paste("Effect Size:", endpoint)
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 12),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10)
    )
  
  # Add raw bounds if they exist
  if (all(c(lower_raw_col, upper_raw_col) %in% names(bounds_df))) {
    p <- p +
      geom_line(
        aes_string(y = lower_raw_col),
        color = "red", linetype = "solid", alpha = 0.7, size = 0.8
      ) +
      geom_line(
        aes_string(y = upper_raw_col),
        color = "red", linetype = "solid", alpha = 0.7, size = 0.8
      ) +
      labs(subtitle = "Blue: LOESS smoothed bounds, Red: Raw quantile bounds")
  }
  
  # Actually display the plot
  print(p)
  return(p)
}

plot_funnel(results_factorial_complement_true_fix)

########################################################

results_factorial_complement_true <- subscreenfunnel(
  data = pbcdat,
  H = results_factorial_complement_true1,
  eval_function = hazardratio,
  min_start = 15,
  n_support_points = 25,
  nperm = 1500,
  alpha = c(0.05,0.1),
  treat = "trt",
  endpoints = c("timepfs" , "event.pfs", "timeos", "event.os")
)

########################################################

# Extract funnel quantiles
funnel_old <- results_factorial_complement_true$funnel_quantiles
funnel_new <- results_factorial_complement_true_fix$funnel_quantiles

# Add method labels
funnel_old$method <- "Old (Resampling)"
funnel_new$method <- "New (Permutation)"

# Combine for comparison
funnel_comparison <- rbind(funnel_old, funnel_new)

# View the differences
head(funnel_comparison)

########################################################

# Prepare data for proper funnel plotting
plot_data <- funnel_comparison %>%
  tidyr::pivot_longer(
    cols = c("HR.pfs", "HR.os"),
    names_to = "endpoint",
    values_to = "hazard_ratio"
  ) %>%
  mutate(
    # Identify upper and lower bounds
    bound_type = case_when(
      alpha <= 0.5 ~ "Lower",
      alpha > 0.5 ~ "Upper",
      TRUE ~ "Unknown"
    ),
    # Create significance level grouping (adjust based on your actual alpha values)
    significance_level = case_when(
      alpha %in% c(0.025, 0.975) ~ "95% CI",
      alpha %in% c(0.05, 0.95) ~ "90% CI",
      alpha %in% c(0.1, 0.9) ~ "80% CI",
      TRUE ~ paste0("Alpha = ", alpha)
    ),
    endpoint_label = case_when(
      endpoint == "HR.pfs" ~ "Progression-Free Survival",
      endpoint == "HR.os" ~ "Overall Survival",
      TRUE ~ endpoint
    ),
    # Create unique line identifier
    line_id = paste(method, significance_level, bound_type, sep = "_")
  ) %>%
  # Important: Order by sample size within each line
  arrange(line_id, n)

# Plot with proper funnel structure
p1 <- ggplot(plot_data, aes(x = n, y = hazard_ratio, color = method, linetype = significance_level)) +
  geom_line(aes(group = line_id), linewidth = 1) +  # Group by line_id to prevent zigzag
  facet_wrap(~endpoint_label, scales = "free_y") +
  labs(
    title = "Funnel Plot Comparison: Old vs New Method",
    subtitle = "Old method (resampling) vs New method (permutation)",
    x = "Sample Size (n)",
    y = "Hazard Ratio",
    color = "Method",
    linetype = "Confidence Level"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold"),
    strip.text = element_text(size = 12, face = "bold")
  ) +
  scale_color_manual(values = c("Old (Resampling)" = "blue", "New (Permutation)" = "red"))

print(p1)

# Plot 2: Difference between methods
difference_data <- funnel_old %>%
  select(-method) %>%
  left_join(
    funnel_new %>% select(-method),
    by = c("n", "alpha"),
    suffix = c("_old", "_new")
  ) %>%
  mutate(
    HR.pfs_diff = HR.pfs_new - HR.pfs_old,
    HR.os_diff = HR.os_new - HR.os_old
  ) %>%
  tidyr::pivot_longer(
    cols = c("HR.pfs_diff", "HR.os_diff"),
    names_to = "endpoint",
    values_to = "difference"
  ) %>%
  mutate(
    alpha_label = paste0("Alpha = ", alpha),
    endpoint_label = case_when(
      endpoint == "HR.pfs_diff" ~ "PFS Difference",
      endpoint == "HR.os_diff" ~ "OS Difference",
      TRUE ~ endpoint
    )
  )

p2 <- ggplot(difference_data, aes(x = n, y = difference, color = alpha_label)) +
  geom_line(size = 1) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  facet_wrap(~endpoint_label, scales = "free_y") +
  labs(
    title = "Difference Between Methods (New - Old)",
    subtitle = "Positive values = New method gives wider funnels",
    x = "Sample Size (n)",
    y = "Difference in Hazard Ratio",
    color = "Significance Level"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    plot.title = element_text(size = 14, face = "bold")
  )

print(p2)

########################################################

# Summary statistics
comparison_summary <- funnel_comparison %>%
  group_by(method, alpha) %>%
  summarise(
    mean_HR_pfs = mean(HR.pfs, na.rm = TRUE),
    mean_HR_os = mean(HR.os, na.rm = TRUE),
    range_HR_pfs = max(HR.pfs, na.rm = TRUE) - min(HR.pfs, na.rm = TRUE),
    range_HR_os = max(HR.os, na.rm = TRUE) - min(HR.os, na.rm = TRUE),
    .groups = "drop"
  )

print("Comparison Summary:")
print(comparison_summary)

# Calculate percentage differences
pct_diff <- funnel_old %>%
  select(-method) %>%
  left_join(funnel_new %>% select(-method), by = c("n", "alpha"), suffix = c("_old", "_new")) %>%
  summarise(
    avg_pfs_pct_diff = mean(abs(HR.pfs_new - HR.pfs_old) / HR.pfs_old * 100, na.rm = TRUE),
    avg_os_pct_diff = mean(abs(HR.os_new - HR.os_old) / HR.os_old * 100, na.rm = TRUE)
  )

cat("\nAverage percentage differences:\n")
cat("PFS:", round(pct_diff$avg_pfs_pct_diff, 2), "%\n")
cat("OS:", round(pct_diff$avg_os_pct_diff, 2), "%\n")

########################################################

results_complement_true <- subscreencalc(
  data = pbcdat,
  eval_function = hazardratio,
  subjectid = "id",
  factors = c("sex", "ageg", "phosg",
              "albuming", "astg", "bilig",
              "cholg", "copperg", "ascitesg",
              "plateletg","spidersg"
  ),
  use_complement = TRUE,
  factorial = FALSE
)


#save
save(results, file = "C:/Temp8600/BIC-subscreen/Documents/Funnel/Run/Data/results.rda")
save(results_factorial_true, file = "C:/Temp8600/BIC-subscreen/Documents/Funnel/Run/Data/results_factorial_true.rda")
save(results_factorial_complement_true, file = "C:/Temp8600/BIC-subscreen/Documents/Funnel/Run/Data/results_factorial_complement_true.rda")
save(results_complement_true, file = "C:/Temp8600/BIC-subscreen/Documents/Funnel/Run/Data/results_complement_true.rda")
save(importance, file = "C:/Temp8600/BIC-subscreen/Documents/Funnel/Run/Data/importance.rda")

# studies <- read.csv("data/studies.csv")
# save(studies, file = "data/studies.rda")


# ##save
# saveRDS(results, "data/results.rds", version = 2)
# saveRDS(results_factorial_true, "data/results_factorial_true.rds", version = 2)
# saveRDS(results_factorial_complement_true, "data/results_factorial_complement_true.rds", version = 2)
# saveRDS(results_complement_true, "data/results_complement_true.rds", version = 2)
#
# saveRDS(importance, "data/importance.rds", version = 2)
#
# # studies <- read.csv("data/studies.csv")#
# #saveRDS(studies, "data/studies.rds", version = 2)




