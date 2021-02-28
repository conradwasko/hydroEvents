#' Event identification (using local minima as a basis)
#'
#' @description Events are identified on the basis of local minima with an "event" considered to have
#' occurred once the data has returned to within a threshold level of the start of the event.
#'
#' @references Tang and Carey (2017) HydRun: A MATLAB toolbox for rainfall-runoff analysis,
#' Hydrological Processes (31) 2670-2682
#'
#' @param data The data vector (e.g. a streamflow time series)
#' @param delta.x Minimum length for an event
#' @param delta.y Maximum difference in data between start and end of an event
#' @param filter.type c("simple", "spline") Optional smoothing of data series
#'
#' @details filter.type can be a "simple" weigthed moving average or smoothing "spline"
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
#' events = eventMinima(qf, delta.x = 5, delta.y = 20)
#' # delta.x = 5, delta.y = 20 # 5 events identified
#' # delta.x = 5, delta.y = 10 # 4 events identified
#' # delta.x = 1, delta.y = 20 # 6 events identified
#' plot(1:length(qf), qf, type = "l", lwd = 1, col = "black", main = "Events with maxima identified",
#'   ylab = "Quickflow (ML/day)", xlab = "Time index", mgp = c(2, 0.6, 0))
#' n.events = nrow(events)
#' for (i in 1:n.events) {
#'   idx = events$srt[i]:events$end[i]
#'   lines(idx, qf[idx], col = rainbow(nrow(events))[i], lwd = 2)
#' }
#' points(events$srt + events$which.max - 1, qf[events$srt + events$which.max - 1], cex = 1.2, lwd = 2)

eventMinima <- function(data, delta.x = 5, delta.y = 20, thresh = -1, out.style = "summary") {
  # Find minima
  minima = localMin(data)

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
    # Test if true end of event
    test.1 = (current.x.2 - current.x.1) > delta.x
    test.2 = (current.y.2 - current.y.1) < delta.y
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

    # Return results
    if (out.style == "summary") {
      event.stats = calcStats(srt.index, end.index, data, f.vec = c("which.max", "max", "sum"))
      return(data.frame(srt = srt.index, end = end.index, event.stats))
    } else {
      return(data.frame(srt = srt.index, end = end.index))
    }
  }
}
