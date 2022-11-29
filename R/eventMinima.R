#' Event identification (using local minima as a basis)
#'
#' @description Events are identified on the basis of local minima with an "event" considered to have
#' occurred once the data has returned to within a threshold level of the start of the event.
#'
#' @references Tang, W., & Carey, S. K. (2017) HydRun: A MATLAB toolbox for rainfall-runoff analysis,
#' Hydrological Processes (31) 2670-2682
#'
#' @param data The data vector
#' @param delta.y Maximum allowable difference between troughs
#' @param delta.x Minimum length for an event
#' @param threshold Value above which an event is considered to have occurred
#' @param out.style The type of output (currently either "summary" or "none")
#'
#' @details The \code{threshold} is applied after the event separation meaning that if a trough
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
#' events = eventMinima(qf, delta.x = 5, delta.y = 20)
#' print(events)
#' plotEvents(qf, dates = NULL, events = events, type = "lineover", main = "")
#' # delta.x = 5, delta.y = 20 # 5 events identified
#' # delta.x = 5, delta.y = 10 # 4 events identified
#' # delta.x = 1, delta.y = 20 # 6 events identified

eventMinima <- function(data, delta.y = 20, delta.x = 5, threshold = -1, out.style = "summary") {
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

    # Return results
    if (out.style == "summary") {
      event.stats = calcStats(srt.index, end.index, data, f.vec = c("which.max", "max", "sum"))
      return(data.frame(srt = srt.index, end = end.index, event.stats))
    } else {
      return(data.frame(srt = srt.index, end = end.index))
    }
  }
}
