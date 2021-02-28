#' Pair rainfall to streamflow events
#'
#' @description Pairing can be performed on the basis of rainfall or streamflow events within a given lag time.
#'
#' @references None
#'
#' @param srt.1 Start time of the dependent events used for pairing
#' @param end.1 End time of the dependent events used for pairing
#' @param srt.2 Start time of the independent events used for pairing
#' @param end.2 End time of the independent events used for pairing
#' @param lag   Maximum lag time (search radius) for pairing
#'
#' @details Pairing can be performed on the basis of rainfall or streamflow events within a given lag time.
#' It is appropriate to pick a lag time that is equiavelent to the catchment time of concentration.
#'
#' @return Returns indices of start and end of events as a two column dataframe.
#'
#' @keywords events baseflow
#' @export
#' @examples
#' ## STREAMFLOW EXAMPLES
#' library(hydroEvents)
#' srt = as.Date("2011-05-01")
#' end = as.Date("2011-08-30")
#' data = data314213[which(data314213$Date >= srt & data314213$Date <= end),]
#' events.P = eventPOT(data$precip_mm, threshold = 1, min.diff = 2)
#' bf = baseFlow(data$Flow_ML, alpha = 0.925)
#' events.Q = eventMaxima(data$Flow_ML-bf, delta.y = 1, delta.x = 1, thresh = 0) # type 1
#' events.Q = eventMaxima(data$Flow_ML-bf, delta.y = 1, delta.x = 1, thresh = 10) # type 2
#' events.Q = eventMaxima(data$Flow_ML-bf, delta.y = 500, delta.x = 1, thresh = 100) # type 3
#' events.Q = eventMaxima(data$Flow_ML-bf, delta.y = 500, delta.x = 7, thresh = 100) # type 3

#' print(events.Q)
#' #events.Q = eventBaseflow(data$Flow_ML-bf)
#' par(mfrow = c(2, 1), mar = c(2, 4, 4, 4))
#' plotEvents(data = pmax(data$precip_mm - 1, 0), events = events.P, type = "hyet")
#' plotEvents(data = data$Flow_ML-bf, events = events.Q, type = "lineover")

#' dev.off()
#' matched = pairEvents(events.P, events.Q, lag = 5, type = 1)   # thres 0
#' matched = pairEvents(events.P, events.Q, lag = 10, type = 2)  # thres 10
#' matched = pairEvents(events.P, events.Q, lag = 5, type = 3)  # thres 0
#' matched = pairEvents(events.P, events.Q, lag = 10, type = 4)  # thres 0


#' print(matched)
#' plotPairedEvents(data.1 = pmax(data$precip_mm), data.2 = data$Flow_ML-bf, events = matched, type = "hyet", col = rainbow)

#' events.1 = events.P
#' events.2 = events.Q
#' lag = 10
#' type = 3

pairEvents <- function(events.1, events.2, lag = 5, type = 1) {

  if (type == 1) {
    n.events = nrow(events.1)
    #end.event    = c(tail(events.1$srt, -1), Inf) - 1
    end.plus.lag = events.1$end + lag
    #end.master   = pmin(end.event, end.plus.lag)

    matched.srt = matched.end = rep(NA_integer_, n.events)
    if (!any(names(events.2) == "which.max")) stop('Need "which.max" in events.2')
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
    #end.event    = c(tail(events.1$srt, -1), Inf) - 1
    end.plus.lag = events.1$end + lag
    #end.master   = pmin(end.event, end.plus.lag)

    matched.srt = matched.end = rep(NA_integer_, n.events)
    for (i in 1:n.events) {
      #index = which(events.2$end >= events.1$end[i] & events.2$end <= end.plus.lag[i])
      index = which(events.2$end >= events.1$srt[i] & events.2$end <= end.plus.lag[i]) # Use this
      if (length(index) > 0) {
        matched.srt[i] = events.2$srt[head(index, 1)]
        matched.end[i] = events.2$end[tail(index, 1)]
      }
    }
    return(data.frame(srt = events.1$srt, end = events.1$end, matched.srt, matched.end))
  }

  if (type == 3) {
    n.events = nrow(events.2)
    start.minus.lag = events.2$srt - lag

    matched.srt = matched.end = rep(NA_integer_, n.events)
    if (!any(names(events.2) == "which.max")) stop('Need "which.max" in events.2')
    if (!any(names(events.1) == "which.max")) stop('Need "which.max" in events.1')
    for (i in n.events:1) {
      index = which(events.1$which.max >= start.minus.lag[i] & events.1$which.max <= events.2$which.max[i])
      if (length(index) > 0) {
        matched.srt[i] = events.1$srt[head(index, 1)]
        matched.end[i] = events.1$end[tail(index, 1)]
      }
    }
  }

  if (type == 4) {
    n.events = nrow(events.2)
    start.minus.lag = events.2$srt - lag

    matched.srt = matched.end = rep(NA_integer_, n.events)
    if (!any(names(events.2) == "which.max")) stop('Need "which.max" in events.2')
    if (!any(names(events.1) == "which.max")) stop('Need "which.max" in events.1')
    for (i in n.events:1) {
      index = which(events.1$srt >= start.minus.lag[i] & events.1$srt <= events.2$srt[i])
      if (length(index) > 0) {
        matched.srt[i] = events.1$srt[head(index, 1)]
        matched.end[i] = events.1$end[tail(index, 1)]
      }
    }
  }
  return(data.frame(matched.srt, matched.end, srt= events.2$srt, end = events.2$end))
}


