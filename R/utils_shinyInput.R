shinyInput <- function(FUN, len, id, click_data,...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, click_data[i]), ...))
  }
  inputs
}
