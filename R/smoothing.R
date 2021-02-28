#' Perform MA smoothing on data
#'
#' @description
#' To fix
#'
#' @param srt Vector of indices for the event start
#' @param end Vector of indices for the event end
#' @param f.list List of functions to be applied to the events
#'
#' @details No additional details provided.
#'
#' @return Returns a dataframe where the row is each event and the column is each statistic
#' @keywords events
#' @seealso \code{\link{eventPOT}}
#' @references \url{https://onlinelibrary.wiley.com/doi/epdf/10.1002/hyp.11185}
#' @export
#' @examples
#' # Extract event statistics from quickflow
#' bf = baseFlow(bassRiver, alpha = 0.925)
#' qf = bassRiver - bf
#' event.indices = eventPOT(qf)
#' event.stats = eventStats(event.indices$srt, event.indices$end, qf)
#'
#' # Plot maxima of events
#' plot(1:length(bassRiver), qf, type = "h", lwd = 2, col = "steelblue",
#'      ylab = "Flow (ML/d)", xlab = "Time index", mgp = c(2, 0.6, 0))
#' points(event.indices$srt + event.stats$which.max - 1, event.stats$max, col = "red", pch = 16, cex = 1.2)
#' legend("topright", legend = c("Quickflow", "Max"), cex = 0.8,
#'        lwd = c(2, NA), pch = c(NA, 16), col = c("steelblue", "red"), bty = "n")

smoothing <- function(data,nstep=1,med.weight=1) { # default is a 3-d MA
  sdata = vector()
  for(t in (1+nstep):(length(data)-nstep)) {
    sdata[t] = (sum(data[(t-nstep):(t-1)])+med.weight*data[t]+sum(data[(t+1):(t+nstep)]))/(nstep*2+1)
  }
  sdata[1:(1+nstep)] = data[1:(1+nstep)]
  sdata[(t-nstep):t] = data[(t-nstep):t]
  return(sdata)
}
### need to check validity with ref