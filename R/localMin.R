#' Local minima
#'
#' @description
#' Return index of local minima. If values are repeated it returns the first index of occurrence.
#' If the first value is repeated it is ignored as local minima.
#'
#' @references None
#'
#' @param x The data vector (e.g. a streamflow time series)
#'
#' @details None
#'
#' @return Returns indices of local minima
#'
#' @keywords minima maxima
#' @source \url{https://stackoverflow.com/questions/6836409/finding-local-maxima-and-minima}
#' @export
#' @examples
#' # Find minima (with repeated values)
#' x = c(1, 2, 9, 9, 2, 1, 1, 5, 5, 1)
#' m = localMin(x)
#' plot(x, type = "l", lwd = 2, xlab = "", ylab = "")
#' points(m, x[m], pch = 16, col = "red")
#'
#' # Find maxima (with repeated values)
#' x = c(1, 2, 9, 9, 2, 1, 1, 5, 5, 1)
#' m = localMin(-x)
#' plot(x, type = "l", lwd = 2, xlab = "", ylab = "")
#' points(m, x[m], pch = 16, col = "red")
#'
#' # Minima in streamflow
#' m = localMin(dataBassRiver)
#' plot(dataBassRiver, type = "l", col = "steelblue", lwd = 2, ylab = "Flow (ML/d)", xlab = "Time index", mgp = c(2, 0.6, 0))
#' points(m, dataBassRiver[m], col = "red", pch = 16)
#'
#' # Minima in quickflow
#' bf = baseFlow(dataBassRiver, alpha = 0.925)
#' qf = dataBassRiver - bf
#' m = localMin(qf)
#' plot(qf, type = "l", lwd = 2, ylab = "Quickflow (ML/d)", xlab = "Time index", mgp = c(2, 0.6, 0))
#' points(m, qf[m], col = "red", pch = 16)
#'
#' # Maxima in quickflow
#' bf = baseFlow(dataBassRiver, alpha = 0.925)
#' qf = dataBassRiver - bf
#' m = localMin(-qf)
#' plot(qf, type = "l", lwd = 2, ylab = "Quickflow (ML/d)", xlab = "Time index", mgp = c(2, 0.6, 0))
#' points(m, qf[m], col = "red", pch = 16)

localMin <- function(x) {
  arms = diff(c(-.Machine$integer.max, -x)) > 0 # Increasing arms TRUE
  index.arms = cumsum(rle(arms)$lengths) # This gets the changes in runs
  end.arms.true = index.arms[seq.int(from = 1, to = length(index.arms), by = 2)] # Only want every second value
  if (x[1] == x[2]) {
    end.arms.true = end.arms.true[-1] # The first value will be a turning point if the value is repeated
  }
  return(end.arms.true)
}
