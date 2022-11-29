#' Extract rising/falling limbs
#'
#' @description Identify the rising and falling limbs within each event (and optionally plot)
#'
#' @param data The data vector (e.g. a streamflow time series)
#' @param dates Date variable, default to NULL (inputting data as a simple vector)
#' @param events Event extracted
#' @param to.plot c(TRUE,FALSE) whether a plot is produced for the limbs
#' @param ymin Minimum plot extend in vertical direction
#' @param ymax Maximum plot extent in vertical direction
#' @param xlab x-axis label
#' @param ylab y-axis label
#' @param main Plot title
#'
#' @return Returns indices of start and end of events and the rising/falling limbs within each event
#'
#' @export
#' @keywords events
#' @examples
#' # Example 1
#' library(hydroEvents)
#' qdata = WQ_Q$qdata[[1]]
#' BF_res = eventBaseflow(qdata$Q_cumecs)
#' limbs(data = qdata$Q_cumecs, dates = NULL, events = BF_res, main = "with 'eventBaseflow'")
#' BFI_res = eventBaseflow(dataBassRiver)
#'
#' # Example 2
#' library(hydroEvents)
#' BFI_res = eventBaseflow(dataBassRiver)
#' d = as.Date("1974-06-30") + 0:(length(dataBassRiver)-1)
#' limbs(data = dataBassRiver, dates = NULL, events = BFI_res)
#' limbs(data = dataBassRiver, dates = d, events = BFI_res)

limbs <- function(data, dates = NULL, events, to.plot = TRUE,
                  ymin = min(data), ymax = max(data), xlab = "", ylab ="", main = "") {

  if (to.plot == TRUE) {
    if (!is.null(dates)) {
      plot(data~dates,type="o",pch=20,cex=0.7,col="grey",ylim=c(ymin,ymax),main=main, xlab=xlab, ylab=ylab, mgp = c(1.7, 0.6, 0))
    } else {
      plot(data,type="o",pch=20,cex=0.7,col="grey",ylim=c(ymin,ymax),main=main, xlab=xlab, ylab=ylab, mgp = c(1.7, 0.6, 0))
    }
  }

  extevents = eventid = list()
  rising = falling = as.data.frame(matrix(NA_real_, nrow(events), 2))
  colnames(rising)  = c("ris.srt","ris.end")
  colnames(falling) = c("fal.srt","fal.end")

  dropevent = vector()
  for (k in 1:nrow(events)) {
    extevents[[k]] = data[events$srt[k]:events$end[k]]
    eventid[[k]] = events$srt[k]:events$end[k]

    rising[k,]  = eventid[[k]][c(1,(min(which(diff(extevents[[k]])<0))))]
    falling[k,] = eventid[[k]][c((min(which(diff(extevents[[k]])<0))),length(eventid[[k]]))]

    if (to.plot == TRUE) {
      if (!k %in% dropevent) {
        if (!is.null(dates)) {
          points(data[c(rising$ris.srt[k],rising$ris.end[k])]~dates[c(rising$ris.srt[k],rising$ris.end[k])],type="p",pch=20,cex=1.5,col="blue")
          points(data[c(falling$fal.srt[k],falling$fal.end[k])]~dates[c(falling$fal.srt[k],falling$fal.end[k])],type="p",pch=20,cex=1.5,col="red")
          points(data[c(events$which.max[k])]~dates[c(events$which.max[k])],type="p",pch=17,cex=1.8,col="orange")

          lines(data[rising$ris.srt[k]:rising$ris.end[k]]~dates[c(rising$ris.srt[k]:rising$ris.end[k])],type="l",pch=20,cex=1.5,col="blue")
          lines(data[falling$fal.srt[k]:falling$fal.end[k]]~dates[c(falling$fal.srt[k]:falling$fal.end[k])],type="l",pch=20,cex=1.5,col="red")

          text(x=dates[median(eventid[[k]])],y=quantile(extevents[[k]],.9),label=paste("Event",k))

        } else {
          points(data[c(rising$ris.srt[k],rising$ris.end[k])]~c(rising$ris.srt[k],rising$ris.end[k]),type="p",pch=20,cex=1.5,col="blue")
          points(data[c(falling$fal.srt[k],falling$fal.end[k])]~c(falling$fal.srt[k],falling$fal.end[k]),type="p",pch=20,cex=1.5,col="red")
          points(data[c(events$which.max[k])]~c(events$which.max[k]),type="p",pch=17,cex=1.8,col="orange")

          lines(data[rising$ris.srt[k]:rising$ris.end[k]]~c(rising$ris.srt[k]:rising$ris.end[k]),type="l",pch=20,cex=1.5,col="blue")
          lines(data[falling$fal.srt[k]:falling$fal.end[k]]~c(falling$fal.srt[k]:falling$fal.end[k]),type="l",pch=20,cex=1.5,col="red")

          text(x=median(eventid[[k]]),y=quantile(extevents[[k]],.9),label=paste("Event",k))
        }
      }
    }
  }

  if (to.plot == TRUE) {
    legend("topright",legend=c("rising","falling","peak"),col=c("blue","red","orange"),lty=c(1,1,NA),pch=c(20,20,17), bty = "n")
  }

  if (length(dropevent) > 0) {
    events=events[-dropevent,]
    extevents=extevents[-dropevent]
    eventid=eventid[-dropevent]
    rising=rising[-dropevent,]
    falling=falling[-dropevent,]
  }


  res = cbind(events, rising, falling)
  return(res)
}
