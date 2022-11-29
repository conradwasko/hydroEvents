#' Baseflow removal (after Ladson et al)
#'
#' @description This function calculates baseflow using a recursive digital filter and is based on
#' the implementation described in Ladson et al (2013).
#'
#' @references Ladson, A., Brown, R., Neal, B., & Nathan, R. (2013). A standard approach to baseflow separation using the Lyne and Hollick filter.
#' Australian Journal of Water Resources, 17(1).
#'
#' @param q The vector series of streamflow
#' @param alpha Filter parameter
#' @param passes Number of passes
#' @param r number of points reflected at start and end of data set
#'
#' @details The reflected points act to resolve spin up issues and are removed before the baseflow is removed.
#'
#' @return A list of the baseflow and baseflow index at each timestep.
#'
#' @keywords baseflow
#' @export
#' @examples
#' library(hydroEvents)
#' data(dataBassRiver)
#' alpha.list = c(0, 0.9, 0.925, 0.95, 0.98, 0.987)
#' BFI = numeric(length(alpha.list))
#' for (i in 1:length(alpha.list)) {
#'   bf = baseflowB(dataBassRiver, alpha = alpha.list[i])
#'   BFI[i] = sum(bf$bf)/sum(dataBassRiver)
#' }
#' print(cbind(alpha.list, BFI))

baseflowB <- function(q, alpha = 0.925, passes = 3, r = 30) {
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
  bfi = bf/q
  res = list(bf = bf, bfi = bfi)
  return(res)
}
