#' Baseflow removal (after Fuka et al. 2018)
#'
#' @description This function caclualates baseflow using a recursive digital filter and is based on
#' the implementation in the EcoHydRology package.
#'
#' The formulation is orginally after Lyne and Hollick (1979) and described in Furey and Gupta (2001).
#' Reccomended parameters are after Nathan and McMahon (1990).
#'
#' @references Fuka D. R., Walter, M.T., Archiblad, J.A., Steenhuis, T.S., & Easton, Z. M. (2018).
#' A Community Modeling Foundation for Eco-Hydrology, R package version 0.4.12.1
#' Flow from Streamflow Time Series. Water Resources Research, 37(11), 2709–2722.
#' @references Furey, P., & Gupta, V. (2001). A Physically Based Filter for Spearating Base
#' Flow from Streamflow Time Series. Water Resources Research, 37(11), 2709–2722.
#' @references Lyne, V., & Hollick, M. (1979). Stochastic time-variable rainfall-runoff modelling.
#' Institute of Engineers Australia National Conference, 89-92.
#' @references Nathan, R. J., & McMahon, T. A. (1990). Evaluation of automated techniques for base flow and recession analyses.
#' Water Resources Research, 26(7), 1465–1473.
#'
#' @param q The vector series of streamflow
#' @param alpha Filter parameter
#' @param passes Number of passes
#'
#' @details
#'
#' @return A list of the baseflow and baseflow index at each timestep.
#'
#' @keywords baseflow
#' @export
#' @examples
#' library(hydroEvents); library(EcoHydRology)
#' alpha.list = c(0, 0.9, 0.925, 0.95, 0.98, 0.987)
#' BFI.1 = BFI.2 = numeric(length(alpha.list))
#' for (i in 1:length(alpha.list)) {
#'   bf.1 = baseflowA(dataBassRiver, alpha = alpha.list[i])
#'   bf.2 = BaseflowSeparation(dataBassRiver, filter_parameter = alpha.list[i])
#'   BFI.1[i] = sum(bf.1$bf)/sum(dataBassRiver)
#'   BFI.2[i] = sum(bf.2$bt)/sum(dataBassRiver)
#' }
#' print(cbind(alpha.list, BFI.1, BFI.2))

baseflowA <- function(q, alpha = 0.925, passes = 3) {
  # Initialise
  n = length(q)
  end = c(1,n)*rep(1,(passes+1))
  add = suppressWarnings(c(1,-1)*rep(1,passes))
  q.new = q # Previous step baseflow
  b = numeric(n)
  b[1] = if (q[1] < quantile(q, 0.25)) {q[1]} else {mean(q)/1.5}

  # Run filter
  for(j in 1:passes){
    # Filter
    for (i in (end[j] + add[j]):(end[j+1])) {
      b.c = alpha*b[i-add[j]]+((1-alpha)/2)*(q.new[i]+q.new[i-add[j]])
      if (b.c > q.new[i]) {
        b[i] = q.new[i]
      } else {
        b[i] = b.c
      }
    }

    # Refine approximation of end values after the pass
    if (j < passes){
      q.new = b
      b[end[j+1]] = if(q[end[j+1]] < mean(q.new)) {q[end[j+1]]/1.2} else {mean(q.new)}
    }
  }

  # Return baseflow
  bfi = b/q
  res = list(bf = b, bfi = bfi)
  return(res)
}

