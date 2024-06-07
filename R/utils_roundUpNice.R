  roundUpNice <- function(x, nice) {
    if (length(x) != 1) stop("'x' must be of length 1")
    if (x >= 0) 10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) *nice)[[1]]]]
    else -1 * (roundDownNice(-x, nice = nice))
  }
