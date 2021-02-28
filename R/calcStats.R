#' Extract statistics from events
#'
#' @description
#' Given the start and end indices statistics are calculated for the values in
#' between the start and end points inclusive.
#'
#' @param srt Vector of indices for the event start
#' @param end Vector of indices for the event end
#' @param f.list List of functions to be applied to the events
#'
#' @details No additional details provided.
#'
#' @return Returns a dataframe where the row is each event and the column is each statistic
#' @keywords events
#' @seealso \code{\link{eventPOT}}
#' @export
#' @examples
#' # Extract event statistics from quickflow
#' bf = baseFlow(bassRiver, alpha = 0.925)
#' qf = bassRiver - bf
#' event.indices = eventPOT(qf)
#' event.stats = eventStats(event.indices$srt, event.indices$end, qf)
#'
#' # Plot maxima of events
#' plot(1:length(bassRiver), qf, type = "h", lwd = 2, col = "steelblue",
#'      ylab = "Flow (ML/d)", xlab = "Time index", mgp = c(2, 0.6, 0))
#' points(event.indices$srt + event.stats$which.max - 1, event.stats$max, col = "red", pch = 16, cex = 1.2)
#' legend("topright", legend = c("Quickflow", "Max"), cex = 0.8,
#'        lwd = c(2, NA), pch = c(NA, 16), col = c("steelblue", "red"), bty = "n")

calcStats <- function(srt, end, data, f.vec = c("which.max", "max", "min")) {
  n.events = length(srt)
  n.functions = length(f.vec)
  val = as.data.frame(matrix(NA_real_, n.events, n.functions))
  for (i in 1:n.events) {
    for (j in 1:n.functions) {
      if (f.vec[j] == "which.max" | f.vec[j] == "which.min") {
        val[i, j] = do.call(f.vec[j], list(data[srt[i]:end[i]])) + srt[i] - 1
      } else {
        val[i, j] = do.call(f.vec[j], list(data[srt[i]:end[i]]))
      }
    }
  }
  names(val) <- as.character(f.vec)
  return(val)
}

