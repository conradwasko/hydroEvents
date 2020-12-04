#' Baseflow removal (after Ladson et al)
#'
#' @description This function removes baseflow based on a recursive digital filter and is based on
#' the implementation described in Ladson et al (2013).
#'
#' @references Ladson, A., Brown, R., Neal, B., & Nathan, R. (2013). A standard approach to baseflow separation using the Lyne and Hollick filter.
#' Australian Journal of Water Resources, 17(1).
#'
#' @param q The vector series of streamflow
#' @param alpha Filter parameter
#' @param passes Number of passes
#' @param r number of points refelcted at start and end of data set
#'
#' @details The reflected points act to resolve spin up issues and are removed before the baseflow is removed.
#'
#' @return Function returns a vector represeting the baseflow of into the inflow \code{q}
#'
#' @keywords baseflow
#' @export
#' @examples
#' data("bassRiver")
#' bf = baseFlow(bassRiver, alpha = 0.98)
#' BFI = sum(bf)/sum(bassRiver)
#' print(BFI) # 0.20
#' plot(1:length(bassRiver), bassRiver, type = "l", lwd = 2, col = "steelblue",
#'  ylab = "Flow (ML/d)", xlab = "Time index", mgp = c(2, 0.6, 0))
#' lines(1:length(bassRiver), bf, lwd = 2, lty = 2, col = "darkgreen")
#' legend("topright", legend = c("Flow", "Baseflow"), cex = 0.8,
#'  lwd = 2, col = c("steelblue", "darkgreen"), bty = "n")

baseFlow <- function(q, alpha = 0.925, passes = 3, r = 30) {
  # Pad with reflected data
  n.i = length(q)
  q.c = c(q[(r+1):2], q, q[(n.i-1):(n.i-r)])

  # Initialise
  n = length(q.c)
  srt = rep(c(1,n),  passes-1)[1:passes]
  end = rep(c(n,1),  passes-1)[1:passes]
  add = rep(c(1,-1), passes-1)[1:passes]
  qf = numeric(n)
  qf[1] = q.c[1]
  bf = q.c

  # Run filter
  for (j in 1:passes) {
    for (i in (srt[j] + add[j]):(end[j])) {
      qf[i] = alpha*qf[i-add[j]] + ((1+alpha)/2)*(bf[i]-bf[i-add[j]])
    }
    bf[qf > 0] = bf[qf > 0] - qf[qf > 0]
    qf[end[j]] = bf[end[j]]
  }

  # Return baseflow
  bf = head(tail(bf, -r), -r)
  return(bf)
}
