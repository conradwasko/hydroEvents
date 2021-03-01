#' Event identification (using baseflow index)
#'
#' @description Events are identified on the basis BFI etc etc
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

eventBaseflow <- function(data, BFI_Th = 0.5, min.diff = 1, threshold = 0, out.style = "summary") {

  # smoothing as of Eqn.3 in https://onlinelibrary.wiley.com/doi/epdf/10.1002/hyp.11185
  # now included in smoothing_DG(...)

  bfi = baseFlow_DG(data)$bfi

  baseind = which(bfi>BFI_Th)
  evind = which(bfi<BFI_Th)

  evS = baseind[which(diff(baseind)>min.diff)]
  evE = baseind[which(diff(baseind)>min.diff)+1]

  #PeakTh = quantile(data,PeakQ,na.rm=T)
  PeakTh = threshold
  ris_ind=fal_ind=rawev=list()
  peakind=sumev=maxev=vector()
  for(i in 1:length(evS)) {
    id = c(evS[i]:evE[i])
    val = data[evS[i]:evE[i]]
    rawev[[i]] = as.data.frame(cbind(id,val))
    peakind[i] = c(evS[i]:evE[i])[which(data[evS[i]:evE[i]]==max(data[evS[i]:evE[i]]))]
    sumev[i] = sum(data[evS[i]:evE[i]])
    maxev[i] = data[evS[i]:evE[i]][which(data[evS[i]:evE[i]]==max(data[evS[i]:evE[i]]))]
    ris_ind[[i]] = c(evS[i]:evE[i])[-1][which(diff(data[evS[i]:evE[i]])>0)]
    fal_ind[[i]] = c(evS[i]:evE[i])[-1][which(diff(data[evS[i]:evE[i]])<0)]
  }
  srt.index = evS
  end.index = evE

  n.events  = length(srt.index)

  risingind = evind[-1][which(diff(data)[evind[-1]]>0)]
  fallingind = evind[-1][which(diff(data)[evind[-1]]<0)]

  if (out.style=="summary") {
    res = data.frame(srt=srt.index,end=end.index, which.max=peakind,max=maxev,sum=sumev)
    # can also output BFI with 'baseind'
  } else {
    res = rawev
  }

  return(res)
}
