#' Function for adding the status of a factorial context ("complete"/"incomplete" or "pseudo complete") to the
#' SubScreenResult object (used in subscreencalc if parameter 'factorial = TRUE').
#'
#' @param data The list entry 'sge' from the "SubScreenResult" object generated via function 'subscreencalc'.
#' @param endpoint The vector of target variable(s).
#' @param factors The list entry 'factors' from the "SubScreenResult" object generated via function 'subscreencalc'.
#'
#'@keywords internal

pseudo_contexts <- function(data, endpoint, factors) {

  if (!"max_level" %in% colnames(data)) {
    data$max_levels <- max(unlist(apply(data[factors],2,function(x){length(unique(x[all(x!="Not used")]))})))
  }

  data[,paste0("FCID_complete_",endpoint)] <- numeric(dim(data)[1])
  data[,paste0("FCID_incomplete_",endpoint)] <- numeric(dim(data)[1])
  data[,paste0("FCID_pseudo_",endpoint)] <- numeric(dim(data)[1])

  data2 <- plyr::ddply(data,"FCID_all", function(y) {
    dim_actual_context <- dim(y)[1]
    expected_context <- expand.grid(lapply(lapply(y %>% dplyr::select(names(which(apply(y %>% dplyr::select(dplyr::all_of(factors)),2,function(x){all(x != "Not used")})))),levels),function(x){x[x != "Not used"]}))
    dim_expected_context <- dim(expected_context)[1]

    if(dim_actual_context !=  dim_expected_context){
      expected_context2 <- expected_context %>%
        dplyr::mutate(
          FCID_all = unique(y$FCID_all),
          max_level = unique(y$max_level),
          nfactors = unique(y$nfactors)
        )
      y2 <- y %>% dplyr::right_join(expected_context2, by = c(colnames(expected_context),"FCID_all","max_level","nfactors"))

    } else {
      y2 <- y
    }
    y2
  })
  data <- data2

  complete_ids <- plyr::ddply(data,"FCID_all", function(x) {
      if(sum(is.na(x[,endpoint])) == 0){
        unique(x$FCID_all)
      }
  })

  complete <- data[data$FCID_all %in% complete_ids$FCID_all,]
  if(dim(complete)[1] != 0) {
    complete[,paste0("FCID_complete_",endpoint)] <- as.integer(factor(rank(complete$FCID_all,ties.method="min")))
    complete[,paste0("FCID_incomplete_",endpoint)] <- "Complete"
    complete[,paste0("FCID_pseudo_",endpoint)] <-"No Pseudo"
  }


  pseudo_cand_ids <- plyr::ddply(data,"FCID_all", function(x) {
    if(
      unique(x$max_level) > 2 &
      unique(x$nfactors) >= 2 &
      sum(is.na(x[,endpoint])) > 0 &
      sum(is.na(x[,endpoint])) <=  unique(x$max_level)
    ) {
        unique(x$FCID_all)
      }
  })
  pseudo_cand <- data[data$FCID_all %in% pseudo_cand_ids$FCID_all,]

  pseudo_fc <- plyr::ddply(pseudo_cand, "FCID_all", function(x){pseudo_func(results = x, endpoint = endpoint, factors = factors)})
  pseudo_all <- data[data$FCID_all %in% unique(pseudo_fc$FCID_all),]
  if(dim(pseudo_all )[1] != 0) {
    pseudo_all[,paste0("FCID_complete_",endpoint)] <- "Not complete"
    pseudo_all[,paste0("FCID_incomplete_",endpoint)] <- "Pseudo complete"
    pseudo_all[,paste0("FCID_pseudo_",endpoint)] <-"No Pseudo"

    pseudo_all[pseudo_all$SGID %in% pseudo_fc$SGID,paste0("FCID_pseudo_",endpoint)] <- as.integer(factor(rank(pseudo_fc$FCID_all,ties.method="min")))
    incomplete <- data[(!data$SGID %in% pseudo_all$SGID) & (!data$SGID %in% complete$SGID) ,]
  } else {
    incomplete <- data[!data$SGID %in% complete$SGID,]
  }

  if(dim(incomplete)[1] != 0) {
    incomplete[,paste0("FCID_complete_",endpoint)] <- "Not complete"
    incomplete[,paste0("FCID_incomplete_",endpoint)] <- "Incomplete"
    incomplete[,paste0("FCID_pseudo_",endpoint)] <-"No Pseudo"
  }
  res <- rbind(complete, incomplete, pseudo_all)

  res <- res[order(res$SGID),]

  return(res)
}
