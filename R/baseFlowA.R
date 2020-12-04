#' Baseflow removal (after Fuka et al)
#'
#' @description This function removes baseflow based on a recursive digital filter and is based on
#' the implementation in the EcoHydRology package with initial values taken from Fuka et al. (2018).
#'
#' The formulation is after Lyne and Hollick (1979) and nicely described in Furey and Gupta (2001).
#' Reccomended parameters are after Nathan and McMahon (1990). Additional discussion on implementations
#' can be found in Ladson et al (2013).
#'
#'
#' @references Furey, P., & Gupta, V. (2001). A Physically Based Filter for Spearating Base
#' Flow from Streamflow Time Series. Water Resources Research, 37(11), 2709–2722.
#' @references Ladson, A., Brown, R., Neal, B., & Nathan, R. (2013). A standard approach to baseflow separation using the Lyne and Hollick filter.
#' Australian Journal of Water Resources, 17(1).
#' @references Nathan, R. J., & McMahon, T. A. (1990). Evaluation of automated techniques for base flow and recession analyses.
#' Water Resources Research, 26(7), 1465–1473.
#' @param Y The vector series of streamflow
#' @param k Filter parameter
#' @param passes Number of passes
#' @keywords baseflow
#' @export
#' @examples
#' data("bassRiver")
#' bf = baseFlow(bassRiver, k = 0.925)
#' BFI = sum(bf)/sum(bassRiver)
#' print(BFI) # 0.22
#' plot(1:length(bassRiver), bassRiver, type = "l", lwd = 2, col = "steelblue",
#'  ylab = "Flow (ML/d)", xlab = "Time index", mgp = c(2, 0.6, 0))
#' lines(1:length(bassRiver), bf, lwd = 2, lty = 2, col = "darkgreen")
#' legend("topright", legend = c("Flow", "Baseflow"), cex = 0.8,
#'  lwd = 2, col = c("steelblue", "darkgreen"), bty = "n")

baseFlowA <- function(Y, k = 0.925, passes = 3) {
  # Initialise
  n = length(Y)
  end = c(1,n)*rep(1,(passes+1))
  add = suppressWarnings(c(1,-1)*rep(1,passes))
  Y.new = Y # Previous step baseflow
  b = numeric(n)
  b[1] = if (Y[1] < quantile(Y, 0.25)) {Y[1]} else {mean(Y)/1.5}

  # Run filter
  for(j in 1:passes){
    # Filter
    for (i in (end[j] + add[j]):(end[j+1])) {
      b.c = k*b[i-add[j]]+((1-k)/2)*(Y.new[i]+Y.new[i-add[j]])
      if (b.c > Y.new[i]) {
        b[i] = Y.new[i]
      } else {
        b[i] = b.c
      }
    }

    # Refine approximation of end values after the pass
    if (j < passes){
      Y.new = b
      b[end[j+1]] = if(Y[end[j+1]] < mean(Y.new)) {Y[end[j+1]]/1.2} else {mean(Y.new)}
    }
  }

  # Return baseflow
  return(b)
}

