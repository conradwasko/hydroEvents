#' Event identification (using local maxima as a basis)
#'
#' @description Events are identified on the basis of local maxima with an "event" considered to have
#' occurred if the maxima is above a tolerable threshold of the neighbouring troughs/valleys.
#'
#' @param data The data vector
#' @param delta.y Minimum allowable difference from a peak to a trough
#' @param delta.x Minimum spacing between peaks
#' @param threshold Value above which an event is considered to have occurred
#' @param out.style The type of output (currently either "summary" or "none")
#'
#' @details If \code{delta.y} is negative it is applied a fractional decrease from the peak, otherwise it is
#' treated as an absolute value. The \code{threshold} is applied after the event separation meaning that if a trough
#' goes below the threshold but was originally considered one event it will continue to be considered one event.
#' This makes this method distinct from the peaks over threshold algorithm in \code{eventPOT}. The \code{threshold}
#' here should be thought of as a filter to remove trace amounts that are not part of an event rather than event separation
#' metric.
#'
#' @return By default, the \code{out.style} returns the indices of the maximum in each event, as well as the value of
#' the maximum and the sum of the \code{data} in each event, alongside the start and end of the events. Otherwise just
#' the indices of start and end of events as a two column dataframe are returned.
#'
#' @export
#' @keywords events baseflow
#' @seealso \code{\link{calcStats}} \code{\link{eventBaseflow}} \code{\link{eventMaxima}} \code{\link{eventPOT}}
#' @export
#' @examples
#' # Example extracting events from quickflow
#' bf = baseflowB(dataBassRiver, alpha = 0.925)
#' qf = dataBassRiver - bf$bf
#' events = eventMaxima(qf, delta.y = 200, delta.x = 1, threshold = 0)
#' print(events)
#' plotEvents(qf, dates = NULL, events = events, type = "lineover", main = "")
#' # Other examples to try
#' # delta.y = 200; delta.x = 1 # 5 events identified
#' # delta.y = 500; delta.x = 1 # 3 events identified
#' # delta.y = 10;  delta.x = 7 # 2 events identified

eventMaxima <- function(data, delta.y = 200, delta.x = 1, threshold = -1, out.style = "summary") {
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
  if (threshold > 0) {
    check.max = calcStats(srt.index, end.index, data, f.vec = c("max"))
    srt.index = srt.index[check.max >= threshold]
    end.index = end.index[check.max >= threshold]
  }

  # Return the event indices
  n.events  = length(srt.index)
  if (n.events == 0) {
    return(NULL)

  } else {
    # Remove leading and trailing small values
    if (threshold >= 0) {
      for (i in 1:n.events) {
        event.data = data[srt.index[i]:end.index[i]]
        runs = rle(event.data <= threshold)
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
