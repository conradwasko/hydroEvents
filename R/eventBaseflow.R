#' Event identification (using baseflow index)
#'
#' @description Events are identified on the basis of the Baseflow Index (BFI).
#'
#' @references Kaur, S., Horne, A., Stewardson, M.J., Nathan, R., Costa, A.M., Szemis, J.M., & Webb, J.A., (2017)
#' Challenges for determining frequency of high flow spells for varying thresholds in environmental flows programmes. J. Ecohydraulics 2, 28–37.
#'
#' @param data The data vector (e.g. a streamflow time series)
#' @param BFI_Th Minimum BFI to identify baseflow
#' @param bfi If no BFI is provided the BFI is calculated automatically using baseflowB
#' @param min.diff Minimum length for an event
#' @param out.style The type of output (currently either "summary" or "none")
#'
#' @details Any flow above the \code{BFI_Th} will be considered an event with a minimum event separation of \code{min.diff}.
#'
#' @return By default, the \code{out.style} returns the indices of the maximum in each event, as well as the value of
#' the maximum and the sum of the \code{data} in each event, alongside the start and end of the events. Otherwise just
#' the indices of start and end of events as a two column dataframe are returned.
#'
#' @export
#' @keywords events
#' @seealso \code{\link{calcStats}} \code{\link{eventBaseflow}} \code{\link{eventMaxima}} \code{\link{eventPOT}}
#' @examples
#' # Example
#' BFI_res = eventBaseflow(dataBassRiver, BFI_Th = 0.5, min.diff = 1)

eventBaseflow <- function(data, BFI_Th = 0.5, alpha_bfi = alpha_bfi  , min.diff = 1, out.style = "summary") {
  
  bfi = baseflowB(data, alpha_bfi =alpha_bfi)$bfi
  
  baseind = which(bfi>BFI_Th)
  evind = which(bfi<BFI_Th)

  evS = baseind[which(diff(baseind)>min.diff)]
  evE = baseind[which(diff(baseind)>min.diff)+1]

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
  
  out = list(res ,ris_ind ,fal_ind)
  names(out) = c('res','ris_ind','fal_ind')
  return(out)

}
