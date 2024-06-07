#Helper Function for Memorize Subgroup Table
shinyInput_remove <- function (FUN, len, id, remove_data, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, as.numeric(strsplit(remove_data, "_")[[1]][2])), ...))
  }
  inputs
}
