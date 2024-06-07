createCombinationMatrix <- function(n, k, l) {
  t(
    do.call(
      cbind,
      lapply(
        k:l,
        function(x) {
          utils::combn(
            n,
            x,
            tabulate,
            nbins = n
          )
        }
      )
    )
  )
}
