#' Extract statistics from events
#'
#' @description Given the start and end indices of events statistics are calculated for the values in
#' between the start and end points inclusive.
#'
#' @param srt Vector of indices for the event start
#' @param end Vector of indices for the event end
#' @param data Vector of data
#' @param f.vec c("which.max", "max", "min") Functions to be applied to the events
#'
#' @return Returns a dataframe where the row is each event and the column is each statistic. If \code{which.min} or
#' \code{which.max} are called the indices returned are global, that is, relative to the start of \code{data}.
#'
#' @keywords events
#' @seealso \code{\link{eventPOT}} \code{\link{eventBaseflow}} \code{\link{eventMaxima}} \code{\link{eventMinima}}
#' @export
#' @examples
#' # Extract event statistics and plot the maxima
#' event.indices = eventPOT(dataLoch, out.style = "none")
#' event.stats = calcStats(event.indices$srt, event.indices$end, dataLoch)
#' print(event.stats)
#'
#' plot(1:length(dataLoch), dataLoch, type = "h", lwd = 2, col = "steelblue",
#'      ylab = "Rainfall (mm)", xlab = "Time index", mgp = c(2, 0.6, 0))
#' points(event.stats$which.max, event.stats$max, col = "red", pch = 16, cex = 1.2)
#' legend("topright", legend = c("Rainfall", "Max"), cex = 0.8,
#'        lwd = c(2, NA), pch = c(NA, 16), col = c("steelblue", "red"), bty = "n")

calcStats <- function(srt, end, data, f.vec = c("which.max", "max", "min")) {
  n.events = length(srt)
  n.functions = length(f.vec)
  val = as.data.frame(matrix(NA_real_, n.events, n.functions))
  for (i in 1:n.events) {
    if (!is.na(srt[i])) {
      for (j in 1:n.functions) {
        if (f.vec[j] == "which.max" | f.vec[j] == "which.min") {
          val[i, j] = do.call(f.vec[j], list(data[srt[i]:end[i]])) + srt[i] - 1
        } else {
          val[i, j] = do.call(f.vec[j], list(data[srt[i]:end[i]]))
        }
      }
    }
  }
  names(val) <- as.character(f.vec)
  return(val)
}

