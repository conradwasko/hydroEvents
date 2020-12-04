#' Event identification (using a peak over threshold algorithm)
#'
#' @description Description here
#'
#' @references None
#'
#' @param data The data vector (e.g. a streamflow time series)
#' @param threshold Value aove which an event is considered to have occurred
#' @param min.diff Spacing of X considered to be independent (e.g. 1 day of zero flow)
#' @param ... Further arguments passed event.stats (NEED TO FIX)
#'
#' @details The reflected points act to resolve spin up issues and are removed before the baseflow is removed.
#'
#' @return Returns indices of start and end of events as a two column dataframe.
#'
#' @keywords events baseflow
#' @export
#' @examples
#' ## STREAMFLOW EXAMPLES
#' # Extract events
#' bf = baseFlow(dataBassRiver, alpha = 0.925)
#' qf = dataBassRiver - bf
#' events = eventPOT(qf)
#'
#' # Plot original flow
#' plot(1:length(dataBassRiver), dataBassRiver, type = "l", lwd = 2, col = "steelblue",
#'      ylab = "Flow (ML/d)", xlab = "Time index", mgp = c(2, 0.6, 0))
#' lines(1:length(dataBassRiver), bf, lwd = 2, lty = 2, col = "darkgreen")
#' points(events$srt, dataBassRiver[events$srt], col = "red3", pch = 1, cex = 1.2)
#' points(events$end, dataBassRiver[events$end], col = "black", pch = 2, cex = 1.2)
#' legend("topright", legend = c("Flow", "Baseflow", "Start Event", "End Event"), cex = 0.8,
#'        lwd = c(2, 2, NA, NA), pch = c(NA, NA, 1, 2), col = c("steelblue", "darkgreen", "red3", "black"), bty = "n")
#'
#' # Plot quickflow only
#' plot(1:length(dataBassRiver), qf, type = "h", lwd = 2, col = "steelblue",
#'      ylab = "Flow (ML/d)", xlab = "Time index", mgp = c(2, 0.6, 0))
#' points(events$srt, qf[events$srt], col = "red3", pch = 1, cex = 1.2)
#' points(events$end, qf[events$end], col = "black", pch = 2, cex = 1.2)
#' points(events$srt + events$which.max - 1, events$max, col = "red", pch = 16, cex = 0.8)
#' legend("topright", legend = c("Quickflow", "Start Event", "End Event", "Max"), cex = 0.8,
#'        lwd = c(2, NA, NA, NA), pch = c(NA, 1, 2, 16), col = c("steelblue", "red3", "black", "red"), bty = "n")
#'
#' ## RAINFALL EXAMPLES
#' # Extract events and plot
#' rain.min = 2
#' events = eventPOT(dataLoch, threshold = rain.min, min.diff = 2)
#' plot(1:length(dataLoch), dataLoch, type = "h", lwd = 2, col = "steelblue",
#'     ylab = "Rainfall (mm)", xlab = "Time index", mgp = c(2, 0.6, 0))
#' abline(h = rain.min, lty = 3)
#' points(events$srt, rep(rain.min, length(events$srt)), col = "red3", pch = 1, cex = 1.2)
#' points(events$end, rep(rain.min, length(events$end)), col = "black", pch = 2, cex = 1.2)
#' points(events$srt + events$which.max - 1, events$max, col = "red", pch = 16, cex = 0.8)
#' legend("topright", legend = c("Rainfall", "Start Event", "End Event", "Max"), cex = 0.8,
#'      lwd = c(2, NA, NA, NA), pch = c(NA, 1, 2, 16), col = c("steelblue", "red3", "black", "red"), bty = "n")

eventPOT <- function(data, threshold = 0, min.diff = 1, ...) {
  # Select data over threshold
  data.index = which(data > threshold)

  # Split data up into events: If events are too close together they are considered part of the same event
  dif.index = diff(data.index)
  sep.index = which(dif.index > min.diff)
  srt.index = c(data.index[1], data.index[sep.index+1])
  end.index = c(data.index[sep.index], tail(data.index,1))
  n.events  = length(srt.index)

  # Return the event indices
  if (n.events == 0) {
    return(NULL)
  } else {
    event.stats = eventStats(srt.index, end.index, data, ...)
    return(data.frame(srt = srt.index, end = end.index, event.stats))
  }
}
