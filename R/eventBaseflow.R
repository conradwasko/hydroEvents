#' Event identification (using baseflow index)
#'
#' @description Events are identified on the basis BFI etc etc
#'
#' @references Rory?
#'
#' @param data The data vector (e.g. a streamflow time series)
#' @param BFI_Th Minimum BFI to identify baseflow
#' @param min.diff Minimum length for an event
#' @param threshold Value above which an event is considered to have occurred
#' @param out.style The type of output (currently either "summary" or "none")
#'
#'
#' @return Returns indices of start and end of events as a two column dataframe and event statistics
#'
#' @export
#' @keywords events
#' @seealso \code{\link{calcStats}} \code{\link{eventPOT}}
#' @examples
#' # Example extracting events from quickflow
#' bf = baseflowB(dataBassRiver)
#' BFI_res = eventBaseflow(dataBassRiver, BFI_Th = 0.5, min.diff = 1, threshold = 0)

eventBaseflow <- function(data, BFI_Th = 0.5, min.diff = 1, threshold = 0, out.style = "summary") {
  bfi = baseflowB(data)$bfi

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
