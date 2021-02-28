#' Plot Events
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

plotPairedEvents <- function(data.1, data.2, events, ymax = max(data.1), main ="", type = "hyet", col.f = rainbow()) {

  n.events = nrow(events)
  color.list = col.f(n.events)

  if (names(matched)[1] == "srt") {
    if (type == "hyet") {
      plot(data.1, type = "h", mgp = c(2, 0.6, 0), col = "black")
      for (i in 1:n.events) {
        points(events$srt[i]:events$end[i], data.1[events$srt[i]:events$end[i]],
               type = "h", col = color.list[i], lwd = 2)
      }
      par(new = TRUE)
      plot(data.2, type = "l", col = "black", lwd = 1, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
      axis(side = 4)
      for (i in 1:n.events) {
        if (!is.na(events$matched.srt[i])) {
          points(events$matched.srt[i]:events$matched.end[i], data.2[events$matched.srt[i]:events$matched.end[i]],
                 type = "l", col = color.list[i], lwd = 3)
        } else {
          points(events$srt[i]:events$end[i], rep(0, length(events$srt[i]:events$end[i])),
                 type = "o", pch = 1, col = "black", lwd = 2)
        }
      }
    }
    legend("topright", bty = "n", legend = "unmatched", col = "black", pch = 1, lwd = 2)
  } else
  if (names(matched)[1] == "matched.srt") {
    if (type == "hyet") {
      plot(data.2, type = "l", mgp = c(2, 0.6, 0), col = "black")
      for (i in 1:n.events) {
        lines(events$srt[i]:events$end[i], data.2[events$srt[i]:events$end[i]],
               col = color.list[i], lwd = 2)
      }
      par(new = TRUE)
      plot(data.1, type = "h", col = "black", lwd = 1, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
      axis(side = 4)
      for (i in 1:n.events) {
        if (!is.na(events$matched.srt[i])) {
          points(events$matched.srt[i]:events$matched.end[i], data.1[events$matched.srt[i]:events$matched.end[i]],
                 type = "h", col = color.list[i], lwd = 3)
        } else {
          points(events$srt[i]:events$end[i], rep(0, length(events$srt[i]:events$end[i])),
                 type = "o", pch = 1, col = "black", lwd = 2)
        }
      }
    }
    legend("topright", bty = "n", legend = "unmatched", col = "black", pch = 1, lwd = 2)
  }
}
