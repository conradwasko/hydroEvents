#' Plot Paired Events
#'
#' @description Wrapper function for plotting paired events
#'
#' @param data.1 The first data vector
#' @param data.2 The second data vector
#' @param events The pairedeEvents data frame from \link{pairEvents}
#' @param dates Optional date vector
#' @param type The type of plot (see details)
#' @param col.list Vector of colours used for plotting
#' @param xlab x-axis label
#' @param ylab.1 primary y-axis label
#' @param ylab.2 secondary y-axis label
#' @param main Plot title
#'
#' @details If the type is \code{"hyet"} then \code{data.1} is plotted as a vertical lines and \code{data.2} as a line.
#' If the type is \code{"lineover"} then all data is plotted as lines.
#'
#' @keywords plot events pairs
#' @seealso \code{\link{pairEvents}}
#' @export
#' @examples
#' library(hydroEvents)
#' BFI_res = eventBaseflow(dataBassRiver)
#' POT_res = eventPOT(dataLoch)
#' pairs.1 = pairEvents(POT_res, BFI_res, type = 1, lag = 5)
#' pairs.3 = pairEvents(POT_res, BFI_res, type = 3, lag = 3)
#' d = as.Date("1974-06-30") + 0:(length(dataBassRiver)-1)
#' par(mar = c(3, 3.5, 2, 3.5), mfrow = c(2, 1))
#' plotPairs(dataLoch, dataBassRiver, pairs.1, dates = d, type = "hyet", xlab = "Date", ylab.1 = "Rain (mm)", ylab.2 = "Flow (ML/day)", main = "Matching Forward")
#' plotPairs(dataLoch, dataBassRiver, pairs.3, dates = d, type = "hyet", xlab = "Date", ylab.1 = "Flow (ML/day)", ylab.2 = "Rain (mm)", main = "Matching Backward")

plotPairs <- function(data.1, data.2, events, dates = NULL, type = "hyet", color.list = rainbow(nrow(events)),
                      xlab = "", ylab.1 = "", ylab.2 = "", main = "") {

  n.events = nrow(events)

  if (type == "hyet") {
    plot.type = "h"
  } else if (type == "lineover") {
    plot.type = "l"
  }

  if (names(events)[1] == "srt") {
    if (is.null(dates)) {
      p = plot(data.1, type = plot.type, mgp = c(1.7, 0.6, 0), col = "black", xlab = xlab, ylab = ylab.1, main = main)
    } else {
      plot(dates, data.1, type = "n", bty = "n", yaxt = "n", mgp = c(1.7, 0.6, 0), xlab = "", ylab = "", main = "")
      par(new = T); plot(data.1, type = plot.type, mgp = c(1.7, 0.6, 0), col = "black", xaxt = "n", xlab = xlab, ylab = ylab.1, main = main)
    }
    for (i in 1:n.events) {
      points(events$srt[i]:events$end[i], data.1[events$srt[i]:events$end[i]],
             type = plot.type, col = color.list[i], lwd = 2)
    }
    par(new = TRUE)
    plot(data.2, type = "l", col = "black", lwd = 1, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    axis(side = 4, mgp = c(1.7, 0.6, 0)); mtext(ylab.2, side = 4, line = 1.7)
    for (i in 1:n.events) {
      if (!is.na(events$matched.srt[i])) {
        points(events$matched.srt[i]:events$matched.end[i], data.2[events$matched.srt[i]:events$matched.end[i]],
               type = "l", col = color.list[i], lwd = 3)
      } else {
        points(events$srt[i]:events$end[i], rep(0, length(events$srt[i]:events$end[i])),
               type = "o", pch = 20, col = "black", lwd = 2)
      }
    }
  } else if (names(events)[1] == "matched.srt") {
    if (is.null(dates)) {
      plot(data.2, type = "l", mgp = c(1.7, 0.6, 0), col = "black", xlab = xlab, ylab = ylab.1, main = main)
    } else {
      plot(dates, data.2, type = "n", bty = "n", yaxt = "n", mgp = c(1.7, 0.6, 0), xlab = "", ylab = "", main = "")
      par(new = T); plot(data.2, type = "l", mgp = c(1.7, 0.6, 0), col = "black", xaxt = "n", xlab = xlab, ylab = ylab.1, main = main)
    }
    for (i in 1:n.events) {
      lines(events$srt[i]:events$end[i], data.2[events$srt[i]:events$end[i]],
             col = color.list[i], lwd = 2)
    }
    par(new = TRUE)
    plot(data.1, type = plot.type, col = "black", lwd = 1, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    axis(side = 4, mgp = c(1.7, 0.6, 0)); mtext(ylab.2, side = 4, line = 1.7)
    for (i in 1:n.events) {
      if (!is.na(events$matched.srt[i])) {
        points(events$matched.srt[i]:events$matched.end[i], data.1[events$matched.srt[i]:events$matched.end[i]],
               type = plot.type, col = color.list[i], lwd = 3)
      } else {
        points(events$srt[i]:events$end[i], rep(0, length(events$srt[i]:events$end[i])),
               type = "o", pch = 20, col = "black", lwd = 2)
      }
    }
  }
}

