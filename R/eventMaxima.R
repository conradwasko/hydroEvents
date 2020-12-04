#' Event identification (using local maxima as a basis)
#'
#' @description Events are identified on the basis of local maxima with an "event" considered to have
#' occurred if the maxima is above a tolerable threshold of the neighbouring troughs/valleys.
#'
#' @references Rory?
#'
#' @param data The data vector (e.g. a streamflow time series)
#' @param delta.y Minimum allowable difference from a peak to a trough
#' @param delta.x Minimum length for an event
#' @param filter.type c("simple", "spline") Optional smoothing of data series
#' @param ... Further arguments passed eventStats
#'
#' @details filter.type can be a "simple" weigthed moving average or smoothing "spline".
#' If \code{delta.y} is negative it is applied a fractional decrease from the peak
#'
#' @return Returns indices of start and end of events as a two column dataframe and event statistics
#'
#' @export
#' @keywords events
#' @seealso \code{\link{eventStats}} \code{\link{eventPOT}}
#' @examples
#' # Example extracting events from quickflow
#' bf = baseFlow(dataBassRiver, alpha = 0.925)
#' qf = dataBassRiver - bf
#' events = eventMaxima(qf, delta.y = 200, delta.x = 1) # 5 events identified
#' # delta.y = 200, delta.x = 1 # 5 events identified
#' # delta.y = 500, delta.x = 1 # 3 events identified
#' # delta.y = 10,  delta.x = 7 # 2 events identified
#' plot(1:length(qf), qf, type = "l", lwd = 1, col = "black", main = "Events with maxima identified",
#'      ylab = "Quickflow (ML/day)", xlab = "Time index", mgp = c(2, 0.6, 0))
#' n.events = nrow(events)
#' for (i in 1:n.events) {
#'   idx = events$srt[i]:events$end[i]
#'   lines(idx, qf[idx], col = rainbow(nrow(events))[i], lwd = 2)
#' }
#' points(events$srt + events$which.max - 1, qf[events$srt + events$which.max - 1], cex = 1.2, lwd = 2)

eventMaxima <- function(data, delta.y = 200, delta.x = 1, filter.type = "none", ...) {
  # Filter
  n.data = length(data)
  if (filter.type == "simple") {
    data[2:(n.data-1)] = filter(data, c(1, 2, 1)/4)[2:(n.data-1)]
  } else if (filter.type == "spline") {
    data = smooth.spline(data)$y
  }

  # Find minima
  minima = localMin(data)
  current.stats = eventStats(head(minima, -1), tail(minima, -1), data, f.vec = c("which.max", "max"))
  maxima.x = c(head(minima, -1) + current.stats$which.max - 1, n.data)
  maxima.y = c(current.stats$max, data[n.data])

  # Identify events
  srt.index = c()
  end.index = c()
  n.minima = length(minima)
  i = 1; j = 2
  while (j <= n.minima) {
    # Current event
    current.x.1 = minima[i]
    current.x.2 = minima[j]
    current.y.1 = data[current.x.1]
    current.y.2 = data[current.x.2]
    current.max = max(maxima.y[i:(j-1)])
    # Test if true end of event
    if (delta.y > 0) {
      test.1 = (current.max - max(current.y.1, current.y.2)) > delta.y
    } else {
      test.1 = (current.max - max(current.y.1, current.y.2)) > -current.max*delta.y
    }
    test.2 = ((maxima.x[j]-maxima.x[j-1]) > delta.x)
    if (test.1 & test.2) {
      srt.index = c(srt.index, current.x.1)
      end.index = c(end.index, current.x.2)
      i = j
      j = i + 1
    } else {
      j = j + 1
    }
  }

  # Return the event indices
  n.events  = length(srt.index)
  if (n.events == 0) {
    return(NULL)
  } else {
    event.stats = eventStats(srt.index, end.index, data, ...)
    return(data.frame(srt = srt.index, end = end.index, event.stats))
  }
}
