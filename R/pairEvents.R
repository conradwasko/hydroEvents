#' Pair Events
#'
#' @description Pairing of events performed either forwards or backwards within specified lag times.
#'
#' @param events.1 Events of first data set
#' @param events.2 Events of second data set
#' @param lag      Maximum lag time (search radius) for pairing
#' @param type     Method used to pair events (see details)
#'
#' @details  Pairing can be performed forwards and backwards and centrally.\code{events.1} and \code{events.2} need to be
#' a dataframe with column names appropriate to the method type.
#' That is, if pairing needs a time of maximum then "which.max" is expected (see examples). Column names are taken from
#' the function event matching functions. The method types are:
#'
#' \itemize{
#'   \item{Type = 1:} Search for the peak in \code{events.2} within the start of \code{event.1} to the end of \code{event.1} + \code{lag}
#'   \item{Type = 2:} Search for an end in \code{events.2} within the start of \code{event.1} to the end of \code{event.1} + \code{lag}
#'   \item{Type = 3:} Search for the peak in \code{events.1} within the start of \code{event.2} - \code{lag} to the peak in \code{event.2}
#'   \item{Type = 4:} Search for a start in \code{events.1} within the start of \code{event.2} - \code{lag} to the start of \code{event.2}
#'   \item{Type = 5:} Search for the peak in \code{events.2} within the peak of \code{event.1} - \code{lag} to the peak of \code{event.1} + \code{lag}
#' }
#'
#' It is appropriate to pick a lag time that is equiavelent to the catchment time of concentratio if matching rainfall to streamflow.
#'
#' @return Returns indices of start and end of events as well as the matched events as a four column dataframe.
#'
#' @keywords events
#' @seealso \code{\link{calcStats}} \code{\link{eventBaseflow}} \code{\link{eventMaxima}} \code{\link{eventMinima}} \code{\link{eventPOT}}
#' @export
#' @examples
#' library(hydroEvents)
#' srt = as.Date("2011-05-01")
#' end = as.Date("2011-08-30")
#' data = data314213[which(data314213$Date >= srt & data314213$Date <= end),]
#' events.P = eventPOT(data$precip_mm, threshold = 1, min.diff = 2)
#' bf = baseflowB(data$Flow_ML)$bf
#' plotEvents(data = data$precip_mm, events = events.P, main = "Precipitation", type = "hyet")
#'
#' events.Q1 = eventMaxima(data$Flow_ML-bf, delta.y = 1,   delta.x = 1, thresh = 0)
#' events.Q2 = eventMaxima(data$Flow_ML-bf, delta.y = 1,   delta.x = 1, thresh = 20)
#' events.Q3 = eventMaxima(data$Flow_ML-bf, delta.y = 500, delta.x = 1, thresh = 100)
#' events.Q4 = eventMaxima(data$Flow_ML-bf, delta.y = 500, delta.x = 7, thresh = 100)
#'
#' par(mfrow = c(4, 1), mar = c(2, 2, 2, 2))
#' plotEvents(data = data$Flow_ML-bf, events = events.Q1, main = "Flow", type = "lineover")
#' plotEvents(data = data$Flow_ML-bf, events = events.Q2, main = "Flow", type = "lineover")
#' plotEvents(data = data$Flow_ML-bf, events = events.Q3, main = "Flow", type = "lineover")
#' plotEvents(data = data$Flow_ML-bf, events = events.Q4, main = "Flow", type = "lineover")
#'
#' matched.1 = pairEvents(events.P, events.Q1, lag = 5,  type = 1)
#' matched.2 = pairEvents(events.P, events.Q2, lag = 10, type = 2)
#' matched.3 = pairEvents(events.P, events.Q3, lag = 5,  type = 3)
#' matched.4 = pairEvents(events.P, events.Q4, lag = 10, type = 4)
#' par(mfrow = c(4, 1), mar = c(2, 2, 2, 2))
#' plotPairedEvents(data.1 = data$precip_mm, data.2 = data$Flow_ML-bf, events = matched.1)
#' plotPairedEvents(data.1 = data$precip_mm, data.2 = data$Flow_ML-bf, events = matched.2)
#' plotPairedEvents(data.1 = data$precip_mm, data.2 = data$Flow_ML-bf, events = matched.3)
#' plotPairedEvents(data.1 = data$precip_mm, data.2 = data$Flow_ML-bf, events = matched.4)

pairEvents <- function(events.1, events.2, lag = 5, type = 1) {

  if (type == 1) {
    if (!any(names(events.2) == "which.max")) stop('Need "which.max" in events.2')

    n.events = nrow(events.1)
    end.plus.lag = events.1$end + lag

    matched.srt = matched.end = rep(NA_integer_, n.events)
    for (i in 1:n.events) {
      index = which(events.2$which.max >= events.1$srt[i] & events.2$which.max <= end.plus.lag[i])
      if (length(index) > 0) {
        matched.srt[i] = events.2$srt[head(index, 1)]
        matched.end[i] = events.2$end[tail(index, 1)]
      }
    }
    return(data.frame(srt = events.1$srt, end = events.1$end, matched.srt, matched.end))
  }

  if (type == 2) {
    n.events = nrow(events.1)
    end.plus.lag = events.1$end + lag

    matched.srt = matched.end = rep(NA_integer_, n.events)
    for (i in 1:n.events) {
      index = which(events.2$end >= events.1$srt[i] & events.2$end <= end.plus.lag[i])
      if (length(index) > 0) {
        matched.srt[i] = events.2$srt[head(index, 1)]
        matched.end[i] = events.2$end[tail(index, 1)]
      }
    }
    return(data.frame(srt = events.1$srt, end = events.1$end, matched.srt, matched.end))
  }

  if (type == 3) {
    if (!any(names(events.2) == "which.max")) stop('Need "which.max" in events.2')
    if (!any(names(events.1) == "which.max")) stop('Need "which.max" in events.1')

    n.events = nrow(events.2)
    start.minus.lag = events.2$srt - lag

    matched.srt = matched.end = rep(NA_integer_, n.events)
    for (i in n.events:1) {
      index = which(events.1$which.max >= start.minus.lag[i] & events.1$which.max <= events.2$which.max[i])
      if (length(index) > 0) {
        matched.srt[i] = events.1$srt[head(index, 1)]
        matched.end[i] = events.1$end[tail(index, 1)]
      }
    }
    return(data.frame(matched.srt, matched.end, srt = events.2$srt, end = events.2$end))
  }

  if (type == 4) {
    n.events = nrow(events.2)
    start.minus.lag = events.2$srt - lag

    matched.srt = matched.end = rep(NA_integer_, n.events)
    for (i in n.events:1) {
      index = which(events.1$srt >= start.minus.lag[i] & events.1$srt <= events.2$srt[i])
      if (length(index) > 0) {
        matched.srt[i] = events.1$srt[head(index, 1)]
        matched.end[i] = events.1$end[tail(index, 1)]
      }
    }
    return(data.frame(matched.srt, matched.end, srt = events.2$srt, end = events.2$end))
  }

  if (type == 5) {
    if (!any(names(events.1) == "which.max")) stop('Need "which.max" in events.1')
    if (!any(names(events.2) == "which.max")) stop('Need "which.max" in events.2')

    n.events = nrow(events.1)
    max.plus.lag = events.1$which.max + lag
    max.minus.lag = events.1$which.max - lag

    matched.srt = matched.end = rep(NA_integer_, n.events)
    for (i in n.events:1) {
      index = which(events.1$which.max >= max.minus.lag[i] & events.1$which.max <= max.plus.lag[i])
      if (length(index) > 0) {
        matched.srt[i] = events.2$srt[head(index, 1)]
        matched.end[i] = events.2$end[tail(index, 1)]
      }
    }
    return(data.frame(srt = events.1$srt, end = events.1$end, matched.srt, matched.end))
  }
}


