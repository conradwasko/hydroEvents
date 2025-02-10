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

eventBaseflow <- function(data, BFI_Th = 0.5, bfi = baseflowB(data)$bfi, min.diff = 1, out.style = "summary") {
  baseind = which(bfi>BFI_Th)
  evind = which(bfi<BFI_Th)

  evS = baseind[which(diff(baseind)>min.diff)]
  evE = baseind[which(diff(baseind)>min.diff)+1]

  srt.index = evS
  end.index = evE

  if (out.style=="summary") {
    event.stats = calcStats(srt.index, end.index, data, 
                            f.vec = c("which.max", "max", "sum"))
    return(data.frame(srt = srt.index, end = end.index, 
                      event.stats))
  } else {
    res = rawev
  }

  return(res)
}
