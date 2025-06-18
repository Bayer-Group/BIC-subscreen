#' #'Create example data for subscreen package with pbc data set
#' #'
#' #'
#' 
#' library(survival)
#' library(dplyr)
#' utils::data(pbc, package = "survival")
#' ##generate categorical versions of some of the baseline covariates
#' pbc <- pbc %>%
#'   dplyr::mutate(
#'   ageg = dplyr::case_when(
#'     age <= quantile(pbc$age, 0.33, na.rm = TRUE) ~ "Low",
#'     age > quantile(pbc$age, 0.33, na.rm = TRUE) & age <= quantile(pbc$age, 0.66, na.rm = TRUE) ~"Middle",
#'     age > quantile(pbc$age, 0.66, na.rm = TRUE) ~ "High",
#' TRUE ~ "No data"
#'   ),
#'   phosg = dplyr::case_when(
#'     alk.phos<= quantile(pbc$alk.phos, 0.5, na.rm = TRUE) ~ "Low",
#'     alk.phos > quantile(pbc$alk.phos, 0.5, na.rm = TRUE) ~ "High",
#'      TRUE ~ "No data"
#'   ),
#'   albuming = dplyr::case_when(
#'     albumin<= quantile(pbc$albumin, 0.33, na.rm = TRUE) ~ "Low",
#'     albumin > quantile(pbc$albumin, 0.33, na.rm = TRUE) & albumin <= quantile(pbc$albumin, 0.66, na.rm = TRUE) ~"Middle",
#'     albumin > quantile(pbc$albumin, 0.66, na.rm = TRUE) ~ "High",
#'           TRUE ~ "No data"
#'   ),
#'   astg = dplyr::case_when(
#'     ast<= quantile(pbc$ast, 0.33, na.rm = TRUE) ~ "Low",
#'     ast > quantile(pbc$ast, 0.33, na.rm = TRUE) & ast<= quantile(pbc$ast, 0.66, na.rm = TRUE) ~"Middle",
#'     ast > quantile(pbc$ast, 0.66, na.rm = TRUE) ~ "High",
#'      TRUE ~ "No data"
#'   ),
#'   bilig = dplyr::case_when(
#'     bili<= quantile(pbc$bili, 0.33, na.rm = TRUE) ~ "Low",
#'     bili > quantile(pbc$bili, 0.33, na.rm = TRUE) & bili<= quantile(pbc$bili, 0.66, na.rm = TRUE) ~"Middle",
#'     bili > quantile(pbc$bili, 0.66, na.rm = TRUE) ~ "High",
#'     TRUE ~ "No data"
#'     ),
#'   cholg = dplyr::case_when(
#'     chol<= quantile(pbc$chol, 0.5, na.rm = TRUE) ~ "Low",
#'     chol > quantile(pbc$chol, 0.5, na.rm = TRUE) ~ "High",
#'      TRUE ~ "No data"
#'   ),
#'     copperg = dplyr::case_when(
#'     copper<= quantile(pbc$copper, 0.33, na.rm = TRUE) ~ "Low",
#'     copper > quantile(pbc$copper, 0.33, na.rm = TRUE) & copper<= quantile(pbc$copper, 0.66, na.rm = TRUE) ~ "Middle",
#'     copper > quantile(pbc$copper, 0.66, na.rm = TRUE) ~ "High",
#'       TRUE ~ "No data"
#'       ),
#'     ascitesg = dplyr::case_when(
#'     ascites == 1 ~ "Yes",
#'       ascites == 0  ~ "No",
#'       TRUE ~ "No data"
#'       ),
#'     plateletg = dplyr::case_when(
#'     platelet<= quantile(pbc$platelets, 0.5, na.rm = TRUE) ~ "Low",
#'       platelet > quantile(pbc$platelet, 0.5, na.rm = TRUE) ~ "High",
#'      TRUE ~ "No data"
#'       ),
#'     spidersg = dplyr::case_when(
#'       spiders == 1 ~ "Yes",
#'       spiders == 0  ~ "No",
#'     TRUE ~ "No data"
#'       )
#'     )
#' ## # eliminate treatment NAs#
#' 
#'   pbcdat <- pbc[!is.na(pbc$trt), ]
#' # # PFS and OS endpoints
#'   set.seed(2006)##
#'   pbcdat$'event.pfs' <- sample(c(0, 1), dim(pbcdat)[1], replace = TRUE)
#'   pbcdat$'timepfs' <- sample(1:5000, dim(pbcdat)[1], replace = TRUE)#
#'   pbcdat$'event.os' <- pbcdat$event
#'   pbcdat$'timeos' <- pbcdat$time##
#'   #variable importance for OS for the created categorical variables
#'   # (higher is more important, also works for numeric variables)
#' 
#'   #define function the eval_function()
#' 
#' #Attention: The eval_function ALWAYS needs to return a dataframe with one row.
#' #            Include exception handling, like if(N1>0 && N2>0) hr <- exp(coxph(...) )#
#' #           to avoid program abort due to errors
#'  hazardratio <- function(D) {
#'    HRpfs <- tryCatch(
#'      exp(coxph(Surv(D$timepfs, D$event.pfs) ~ D$trt )$coefficients[[1]]),
#'     warning = function(w) {NA}
#'   )
#'   HRpfs <- 1/HRpfs
#'   HR.pfs <- round(HRpfs, 2)
#'   HR.pfs[HR.pfs > 10]      <- 10
#'   HR.pfs[HR.pfs < 0.00001] <- 0.00001
#'   HRos <- tryCatch(
#'     exp(coxph(Surv(D$timeos, D$event.os) ~ D$trt )$coefficients[[1]]),
#'     warning = function(w) {NA}
#'     )
#'   HRos <- 1/HRos
#'   HR.os <- round(HRos, 2)
#'   HR.os[HR.os > 10]      <- 10
#'   HR.os[HR.os < 0.00001] <- 0.00001
#'   data.frame(
#'     HR.pfs,
#'     HR.os
#'   )
#' }
#' # # Variable importance
#' pbcdat$status <- ifelse(pbcdat$status == 0, 0, 1)
#' importance <- subscreenvi(
#'   data = pbcdat,
#'   y = 'time',
#'   cens = 'status',
#'   trt = 'trt',
#'   x = c("sex", "ageg", "phosg",
#'     "albuming", "astg", "bilig",
#'     "cholg", "copperg", "ascitesg",
#'     "plateletg","spidersg"
#'   )
#' )
#' 
#' results <- subscreencalc(
#'   data = pbcdat,
#'   eval_function = hazardratio,
#'   subjectid = "id",
#'   factors = c("sex", "ageg", "phosg",
#'     "albuming", "astg", "bilig",
#'     "cholg", "copperg", "ascitesg",
#'     "plateletg","spidersg"
#'   ),
#'   use_complement = FALSE,
#'   factorial = FALSE)
#' 
#' results_w_funnel <- subscreenfunnel(
#'   data = pbcdat,
#'   H = results,
#'   hazardratio,
#'   min_start=25,
#'   n_support_points=6,
#'   nperm=2500,
#'   alpha = c(0.1,0.01,0.001),
#'   treat = "trt",
#'   endpoints = c("event.pfs","timepfs","event.os","timeos")
#' )
#' 
#' results_factorial_true <- subscreencalc(
#'   data = pbcdat,
#'   eval_function = hazardratio,
#'   subjectid = "id",
#'   factors = c("sex", "ageg", "phosg",
#'     "albuming", "astg", "bilig",
#'     "cholg", "copperg", "ascitesg",
#'     "plateletg","spidersg"
#'   ),
#'   use_complement = FALSE,
#'   factorial = TRUE
#' )
#' 
#' results_factorial_complement_true <- subscreencalc(
#'   data = pbcdat,
#'   eval_function = hazardratio,
#'   subjectid = "id",
#'   factors = c("sex", "ageg", "phosg",
#'     "albuming", "astg", "bilig",
#'     "cholg", "copperg", "ascitesg",
#'     "plateletg","spidersg"
#'   ),
#'   use_complement = TRUE,
#'   factorial = TRUE
#' )
#' 
#' results_complement_true <- subscreencalc(
#'   data = pbcdat,
#'   eval_function = hazardratio,
#'   subjectid = "id",
#'   factors = c("sex", "ageg", "phosg",
#'     "albuming", "astg", "bilig",
#'     "cholg", "copperg", "ascitesg",
#'     "plateletg","spidersg"
#'   ),
#'   use_complement = TRUE,
#'   factorial = FALSE
#' )
#' 
#' 
#' #save
#' save(results, file = "data/results.rda")
#' save(results_w_funnel, file = "data/results_w_funnel.rda")
#' save(results_factorial_true, file = "data/results_factorial_true.rda")
#' save(results_factorial_complement_true, file = "data/results_factorial_complement_true.rda")
#' save(results_complement_true, file = "data/results_complement_true.rda")
#' save(importance, file = "data/importance.rda")
#' 
#' # studies <- read.csv("data/studies.csv")
#' # save(studies, file = "data/studies.rda")
#' 
#' 
#' # ##save
#' # saveRDS(results, "data/results.rds", version = 2)
#' # saveRDS(results_factorial_true, "data/results_factorial_true.rds", version = 2)
#' # saveRDS(results_factorial_complement_true, "data/results_factorial_complement_true.rds", version = 2)
#' # saveRDS(results_complement_true, "data/results_complement_true.rds", version = 2)
#' #
#' # saveRDS(importance, "data/importance.rds", version = 2)
#' #
#' # # studies <- read.csv("data/studies.csv")#
#' # #saveRDS(studies, "data/studies.rds", version = 2)
