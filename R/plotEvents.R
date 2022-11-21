#' Plot Events
#'
#' @description Wrapper function for plotting identified events.
#'
#' @param data The data vector
#' @param dates Optional date vector
#' @param events Events data frame
#' @param type The type of plot (see details)
#' @param colline Line colour
#' @param colpnt Point colour
#' @param colbound Background colour for plot type \code{"bound"}
#' @param ymin Minimum plot extend in vertical direction
#' @param ymax Maxiumum plot extent in vertical direction
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param main Plot title
#'
#' @details Three plot types are implemented: \code{"lineover"}, \code{"bound"}, \code{"hyet"}. See examples.
#' If events contains a column titled "which.max" the maxima are also plotted.
#'
#' @return No return value.
#'
#' @keywords plot events
#' @seealso \code{\link{eventBaseflow}} \code{\link{eventMaxima}} \code{\link{eventMinima}} \code{\link{eventPOT}}
#' @export
#' @examples
#' # Plot events
#' library(hydroEvents)
#' BFI_res = eventBaseflow(dataBassRiver)
#'
#' oldpar <- par(mfrow = c(3, 1), mar = c(3, 2.7, 2, 1))
#' d = as.Date("1974-06-30") + 0:(length(dataBassRiver)-1)
#' plotEvents(data = dataBassRiver, dates = d, events = BFI_res, type = "lineover", xlab = "Date", ylab = "Flow (ML/day)", main = "lineover")
#' plotEvents(data = dataBassRiver, dates = d, events = BFI_res, type = "bound", xlab = "Date", ylab = "Flow (ML/day)", main = "bound")
#' plotEvents(data = dataBassRiver, dates = d, events = BFI_res, type = "hyet", xlab = "Date", ylab = "Flow (ML/day)", main = "hyet")
#' par(oldpar)

plotEvents <- function(data, dates = NULL, events, type = "lineover",
                       colline = "red", colpnt = "blue", colbound = "red", ymin = min(data), ymax = max(data),
                       xlab = "", ylab = "", main = "events") {

    if (type=="lineover") {

      if (!is.null(dates)) {
        plot(data~dates,type="o",pch=20,cex=0.7,ylim=c(ymin,ymax),main=main, xlab=xlab, ylab=ylab, mgp = c(1.7, 0.6, 0))
      } else {
        plot(data,type="o",pch=20,cex=0.7,ylim=c(ymin,ymax),main=main, xlab=xlab, ylab=ylab, mgp = c(1.7, 0.6, 0))
      }

      extevents = eventid = list()
      for (k in 1:nrow(events)) {
        extevents[[k]] = data[events$srt[k]:events$end[k]]

        if (!is.null(dates)) {
          eventid[[k]] = dates[events$srt[k]:events$end[k]]
        } else {
          eventid[[k]] = events$srt[k]:events$end[k]
        }

        lines(extevents[[k]]~eventid[[k]],col=colline,type="o",pch=20,cex=0.7)
        points(head(extevents[[k]],1)~head(eventid[[k]],1),col=colline,type="o",pch=20,cex=1.5)
        points(tail(extevents[[k]],1)~tail(eventid[[k]],1),col=colline,type="o",pch=20,cex=1.5)
        text(x=median(eventid[[k]]),y=quantile(extevents[[k]],.9),label=paste0("(",k,")"), cex = 1.2)
      }

      if (!is.null(events$which.max)) {
        if (is.null(dates)) {
          points(data[events$which.max]~events$which.max,col=colpnt,pch=20,cex=1)
        } else {
          points(data[events$which.max]~dates[events$which.max],col=colpnt,pch=20,cex=1)
        }
      }


    } else if (type=="bound") {

      if (!is.null(dates)) {
        plot(data~dates,type="o",pch=20,cex=0.7,ylim=c(ymin,ymax),main=main, xlab=xlab, ylab=ylab, mgp = c(1.7, 0.6, 0))
      } else {
        plot(data,type="o",pch=20,cex=0.7,ylim=c(ymin,ymax),main=main, xlab=xlab, ylab=ylab, mgp = c(1.7, 0.6, 0))
      }

      if (!is.null(dates)) {
        allS = dates[events$srt]
        allE = dates[events$end]
      } else {
        allS = events$srt
        allE = events$end
      }
      abline(v=allS,lty=2,col=colline)
      abline(v=allE,lty=2,col=colline)
      rect(xleft=allS,xright=allE,ybottom=0,ytop=ymax,border=NA, col=adjustcolor(colbound,alpha.f=0.2))

      text(x=apply(cbind(allS,allE),1,mean),y=ymax*0.9,label=paste0("(",1:nrow(events),")"),cex = 1.2)

      if (!is.null(events$which.max)) {
        if (is.null(dates)) {
          points(data[events$which.max]~events$which.max,col=colpnt,pch=20,cex=1)
        } else {
          points(data[events$which.max]~dates[events$which.max],col=colpnt,pch=20,cex=1)
        }
      }

    } else if (type == "hyet") {

      if (!is.null(dates)) {
        plot(data~dates,type="h",pch=20,cex=0.7,ylim=c(ymin,ymax),main=main,xlab=xlab, ylab=ylab, mgp = c(1.7, 0.6, 0))
      } else {
        plot(data,type="h",pch=20,cex=0.7,ylim=c(ymin,ymax),main=main,xlab=xlab, ylab=ylab, mgp = c(1.7, 0.6, 0))
      }

      extevents = eventid = list()
      for (k in 1:nrow(events)) {
        extevents[[k]] = data[events$srt[k]:events$end[k]]

        if (!is.null(dates)) {
          eventid[[k]] = dates[events$srt[k]:events$end[k]]
        } else {
          eventid[[k]] = events$srt[k]:events$end[k]
        }

        lines(extevents[[k]]~eventid[[k]],col=colline,type="h",pch=20,cex=0.7)
        points(head(eventid[[k]],1),0,col=colline,type="o",pch=20,cex=1.5)
        points(tail(eventid[[k]],1),0,col=colline,type="o",pch=20,cex=1.5)

        text(x=median(eventid[[k]]),y=quantile(extevents[[k]],.9),label=paste0("(",k,")"),cex = 1.2)

        if (!is.null(events$which.max)) {
          if (is.null(dates)) {
            points(data[events$which.max]~events$which.max,col=colpnt,pch=20,cex=1)
          } else {
            points(data[events$which.max]~dates[events$which.max],col=colpnt,pch=20,cex=1)
          }
        }

      }
    }

}
