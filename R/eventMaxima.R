#' Event identification (using local maxima as a basis)
#'
#' @description Events are identified on the basis of local maxima with an "event" considered to have
#' occurred if the maxima is above a tolerable threshold of the neighbouring troughs/valleys.
#'
#' @references Rory?
#'
#' @param data The data vector (e.g. a streamflow time series)
#' @param delta.y Minimum allowable difference from a peak to a trough
#' @param delta.x Minimum spacing between peaks
#' @param filter.type c("simple", "spline") Optional smoothing of data series
#'
#' @details filter.type can be a "simple" weigthed moving average or smoothing "spline".
#' If \code{delta.y} is negative it is applied a fractional decrease from the peak
#'
#' @return Returns indices of start and end of events as a two column dataframe and event statistics
#'
#' @export
#' @keywords events
#' @seealso \code{\link{calcStats}} \code{\link{eventPOT}}
#' @examples
#' # Example extracting events from quickflow
#' bf = baseFlow(dataBassRiver, alpha = 0.925)
#' qf = dataBassRiver - bf
#' events = eventMaxima(qf, delta.y = 200, delta.x = 1, thresh = 0) # 5 events identified
#' # delta.y = 200; delta.x = 1 # 5 events identified
#' # delta.y = 500; delta.x = 1 # 3 events identified
#' # delta.y = 10;  delta.x = 7 # 2 events identified
#' plot(1:length(qf), qf, type = "l", lwd = 1, col = "black", main = "Events with maxima identified",
#'    ylab = "Quickflow (ML/day)", xlab = "Time index", mgp = c(2, 0.6, 0))
#' n.events = nrow(events)
#' for (i in 1:n.events) {
#'  idx = events$srt[i]:events$end[i]
#'  lines(idx, qf[idx], col = rainbow(nrow(events))[i], lwd = 2)
#' }
#' points(events$which.max, qf[events$which.max], cex = 1.2, lwd = 2)
#' print(events)

eventMaxima <- function(data, delta.y = 200, delta.x = 1, thresh = -1, out.style = "summary") {
  # Find minima
  n.data = length(data)
  minima = localMin(data)
  current.stats = calcStats(head(minima, -1), tail(minima, -1), data, f.vec = c("which.max", "max"))
  maxima.x = c(current.stats$which.max, n.data)
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

  # Remove events that are smaller than the threshold
  if (thresh > 0) {
    check.max = calcStats(srt.index, end.index, data, f.vec = c("max"))
    srt.index = srt.index[check.max >= thresh]
    end.index = end.index[check.max >= thresh]
  }
  #event.stats = calcStats(srt.index, end.index, data, f.vec = c("which.max", "max", "sum"))
  #events = data.frame(srt = srt.index, end = end.index, event.stats)
  #print(events)

  # Return the event indices
  n.events  = length(srt.index)
  if (n.events == 0) {
    return(NULL)

  } else {
    # Remove leading and trailing small values
    if (thresh >= 0) {
      for (i in 1:n.events) {
        event.data = data[srt.index[i]:end.index[i]]
        runs = rle(event.data <= thresh)
        n.runs = length(runs$values)
        if (runs$values[1]) {
          srt.index[i] = srt.index[i] + runs$lengths[1]
        }
        if (runs$values[n.runs]) {
          end.index[i] = end.index[i] - runs$lengths[n.runs]
        }
      }
    }

    if (out.style == "summary") {
      event.stats = calcStats(srt.index, end.index, data, f.vec = c("which.max", "max", "sum"))
      return(data.frame(srt = srt.index, end = end.index, event.stats))
    } else {
      return(data.frame(srt = srt.index, end = end.index))
    }
  }
}
