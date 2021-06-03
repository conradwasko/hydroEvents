#' Plot Events
#'
#' @description Description here
#'
#' @references None
#'
#' @param data The data vector (e.g. a streamflow time series)
#' @param threshold Value aove which an event is considered to have occurred
#' @param min.diff Spacing of X considered to be independent (e.g. 1 day of zero flow)
#' @param ... Further arguments passed event.stats (NEED TO FIX)
#'
#' @details The reflected points act to resolve spin up issues and are removed before the baseflow is removed.
#'
#' @return Returns indices of start and end of events as a two column dataframe.
#'
#' @keywords events baseflow
#' @export
#' @examples
#' ## STREAMFLOW EXAMPLES
#' # Extract events
#' bf = baseFlow(dataBassRiver, alpha = 0.925)
#' qf = dataBassRiver - bf
#' events = eventPOT(qf)
#'
#' # Plot original flow
#' plot(1:length(dataBassRiver), dataBassRiver, type = "l", lwd = 2, col = "steelblue",
#'      ylab = "Flow (ML/d)", xlab = "Time index", mgp = c(2, 0.6, 0))
#' lines(1:length(dataBassRiver), bf, lwd = 2, lty = 2, col = "darkgreen")
#' points(events$srt, dataBassRiver[events$srt], col = "red3", pch = 1, cex = 1.2)
#' points(events$end, dataBassRiver[events$end], col = "black", pch = 2, cex = 1.2)
#' legend("topright", legend = c("Flow", "Baseflow", "Start Event", "End Event"), cex = 0.8,
#'        lwd = c(2, 2, NA, NA), pch = c(NA, NA, 1, 2), col = c("steelblue", "darkgreen", "red3", "black"), bty = "n")
#'
#' # Plot quickflow only
#' plot(1:length(dataBassRiver), qf, type = "h", lwd = 2, col = "steelblue",
#'      ylab = "Flow (ML/d)", xlab = "Time index", mgp = c(2, 0.6, 0))
#' points(events$srt, qf[events$srt], col = "red3", pch = 1, cex = 1.2)
#' points(events$end, qf[events$end], col = "black", pch = 2, cex = 1.2)
#' points(events$srt + events$which.max - 1, events$max, col = "red", pch = 16, cex = 0.8)
#' legend("topright", legend = c("Quickflow", "Start Event", "End Event", "Max"), cex = 0.8,
#'        lwd = c(2, NA, NA, NA), pch = c(NA, 1, 2, 16), col = c("steelblue", "red3", "black", "red"), bty = "n")
#'
#' ## RAINFALL EXAMPLES
#' # Extract events and plot
#' rain.min = 2
#' events = eventPOT(dataLoch, threshold = rain.min, min.diff = 2)
#' plot(1:length(dataLoch), dataLoch, type = "h", lwd = 2, col = "steelblue",
#'     ylab = "Rainfall (mm)", xlab = "Time index", mgp = c(2, 0.6, 0))
#' abline(h = rain.min, lty = 3)
#' points(events$srt, rep(rain.min, length(events$srt)), col = "red3", pch = 1, cex = 1.2)
#' points(events$end, rep(rain.min, length(events$end)), col = "black", pch = 2, cex = 1.2)
#' points(events$srt + events$which.max - 1, events$max, col = "red", pch = 16, cex = 0.8)
#' legend("topright", legend = c("Rainfall", "Start Event", "End Event", "Max"), cex = 0.8,
#'      lwd = c(2, NA, NA, NA), pch = c(NA, 1, 2, 16), col = c("steelblue", "red3", "black", "red"), bty = "n")


plotEvents <- function(data, dates = NULL, events, type = "lineover",
                       colline = "red", colpnt = "blue", ymax = max(data), main = "events") {


  if (!is.null(dates)) {
    plot(data~dates,type="o",pch=20,cex=0.7,ylim=c(0,ymax),main=main, mgp = c(2, 0.6, 0))


  } else {

    if (type=="lineover") {

      plot(data,type="o",pch=20,cex=0.7,ylim=c(0,ymax),main=main, mgp = c(2, 0.6, 0))
      extevents = eventid = list()

      for (k in 1:nrow(events)) {
        extevents[[k]] = data[events$srt[k]:events$end[k]]

        if (!is.null(dates)) {
          eventid[[k]] = dates[events$srt[k]:events$end[k]]
        } else {
          eventid[[k]] = events$srt[k]:events$end[k]
        }

        lines(extevents[[k]]~eventid[[k]],col=colline,type="o",pch=20,cex=0.7)
        points(data[events$srt[k]]~events$srt[k],col=colline,type="o",pch=20,cex=1.5)
        points(data[events$end[k]]~events$end[k],col=colline,type="o",pch=20,cex=1.5)
        points(data[events$which.max[k]]~events$which.max[k],col=colpnt,pch=20,cex=1)

        text(x=median(eventid[[k]]),y=quantile(extevents[[k]],.9),label=paste0("(",k,")"), cex = 0.8)
      }
    } else if (type=="bound") {

      plot(data,type="o",pch=20,cex=0.7,ylim=c(0,ymax),main=main, mgp = c(2, 0.6, 0))

      if (!is.null(dates)) {
        allS = dates[events$srt]
        allE = dates[events$end]
      } else {
        allS = events$srt
        allE = events$end
      }
      abline(v=allS,lty=2,col=colline)
      abline(v=allE,lty=2,col=colline)
      rect(xleft=allS,xright=allE,ybottom=0,ytop=ymax,border=NA, col=adjustcolor("red",alpha.f=0.2))
      text(x=apply(cbind(allS,allE),1,mean),y=ymax*0.9,label=paste("Event",1:nrow(events)))
    } else if (type == "hyet") {

      plot(data,type="h",pch=20,cex=0.7,ylim=c(0,ymax),main=main, mgp = c(2, 0.6, 0))
      extevents = eventid = list()

      for (k in 1:nrow(events)) {

        extevents[[k]] = data[events$srt[k]:events$end[k]]

        if (!is.null(dates)) {
          eventid[[k]] = dates[events$srt[k]:events$end[k]]
        } else {
          eventid[[k]] = events$srt[k]:events$end[k]
        }

        lines(extevents[[k]]~eventid[[k]],col=colline,type="h",pch=20,cex=0.7)
        points(events$srt[k],0,col=colline,type="o",pch=20,cex=1.5)
        points(events$end[k],0,col=colline,type="o",pch=20,cex=1.5)

        text(x=median(eventid[[k]]),y=quantile(extevents[[k]],.9),label=paste("Event",k))
      }
    }

  }
}
