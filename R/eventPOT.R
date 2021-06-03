#' Event identification (using a peak over threshold algorithm)
#'
#' @description Identify events using a specified threshold value over which an event is considered to have occurred.
#'
#' @param data A data vector
#' @param threshold Value above which an event is considered to have occurred
#' @param min.diff Spacing required for two events to be considered seperate
#' @param out.style The type of output (currently either "summary" or "none")
#'
#' @details The \code{threshold} can be thought of a value below which the \code{data} are considered to be "zero".
#' The \code{min.diff} can be viewed as the minimum spacing for event independence.
#'
#' @return By default, the \code{out.style} returns the indices of the maximum in each event, as well as the value of
#' the maximum and the sum of the \code{data} in each event, alongside the start and end of the events. Otherwise just
#' the indices of start and end of events as a two column dataframe are returned.
#'
#' @keywords events baseflow
#' @seealso \code{\link{calcStats}} \code{\link{eventBaseflow}} \code{\link{eventMaxima}} \code{\link{eventMinima}}
#' @export
#' @examples
#' # Example using streamflow data
#' bf = baseflowB(dataBassRiver, alpha = 0.925)
#' qf = dataBassRiver - bf$bf
#' events = eventPOT(qf)
#' plotEvents(qf, dates = NULL, events = events, type = "lineover", main = "Events (plotted on quickflow)")
#' plotEvents(dataBassRiver, dates = NULL, events = events, type = "lineover", main = "Events (plotted on streamflow)")
#'
#' # Examples using rainfall data
#' events = eventPOT(dataLoch, threshold = 0, min.diff = 1)
#' plotEvents(dataLoch, dates = NULL, events = events, type = "hyet", main = "Rainfall Events (threshold = 0, min.diff = 1)")
#'
#' events = eventPOT(dataLoch, threshold = 2, min.diff = 2)
#' plotEvents(dataLoch, dates = NULL, events = events, type = "hyet", main = "Rainfall Events (threshold = 2, min.diff = 2)")

eventPOT <- function(data, threshold = 0, min.diff = 1, out.style = "summary") {
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
    if (out.style == "summary") {
      event.stats = calcStats(srt.index, end.index, data, f.vec = c("which.max", "max", "sum"))
      return(data.frame(srt = srt.index, end = end.index, event.stats))
    } else {
      return(data.frame(srt = srt.index, end = end.index))
    }
  }
}
