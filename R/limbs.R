#' Extract rising/falling limbs
#'
#' @description Identify the rising and falling limbs within each event (and optionally plot)
#'
#' @param data The data vector (e.g. a streamflow time series)
#' @param dates Date variable, default to NULL (inputting data as a simple vector)
#' @param events Event extracted
#' @param plot c(TRUE,FALSE) whether a plot is produced for the limbs
#' @param main Desired title of the plot if plot=T
#'
#' @return Returns indices of start and end of events and the rising/falling limbs within each event
#'
#' @export
#' @keywords events
#' @examples
#' library(hydroEvents)
#' qdata = WQ_Q$qdata[[1]]
#' BF_res = eventBaseflow(qdata$Q_cumecs)
#' limbs(data = qdata$Q_cumecs, dates = NULL, events = BF_res, main = "with 'eventBaseflow'")

limbs <- function(data, dates = NULL, events, plot = TRUE, main = "") {
  if(plot == TRUE) {
    if (!is.null(dates)) {
      plot(data~dates,type="o",pch=20,cex=0.7,main=main,col="grey")
    } else {
      plot(data,type="o",pch=20,cex=0.7,main=main,col="grey")
    }
  }

  extevents = eventid = list()
  rising = falling = as.data.frame(matrix(NA,nrow(events),2))
  colnames(rising)  = c("ris.srt","ris.end")
  colnames(falling) = c("fal.srt","fal.end")

  dropevent = vector()
  for (k in 1:nrow(events)) {
    extevents[[k]] = data[events$srt[k]:events$end[k]]

    if (!is.null(dates)) {
      eventid[[k]] = dates[events$srt[k]:events$end[k]]

    } else {
      eventid[[k]] = events$srt[k]:events$end[k]
    }

    rising[k,] =eventid[[k]][c(1,(min(which(diff(extevents[[k]])<0))))]
    falling[k,] =eventid[[k]][c((min(which(diff(extevents[[k]])<0))),length(eventid[[k]]))]

    if (plot == TRUE) {
      if (!k %in% dropevent) {
        lines(extevents[[k]]~eventid[[k]],type="l",pch=20,cex=0.7)

        points(data[c(rising$ris.srt[k],rising$ris.end[k])]~c(rising$ris.srt[k],rising$ris.end[k]),type="p",pch=20,cex=1.5,col="blue")
        points(data[c(falling$fal.srt[k],falling$fal.end[k])]~c(falling$fal.srt[k],falling$fal.end[k]),type="p",pch=20,cex=1.5,col="red")
        points(data[c(events$which.max[k])]~c(events$which.max[k]),type="p",pch=17,cex=1.8,col="orange")

        lines(data[rising$ris.srt[k]:rising$ris.end[k]]~c(rising$ris.srt[k]:rising$ris.end[k]),type="l",pch=20,cex=1.5,col="blue")
        lines(data[falling$fal.srt[k]:falling$fal.end[k]]~c(falling$fal.srt[k]:falling$fal.end[k]),type="l",pch=20,cex=1.5,col="red")

        text(x=median(eventid[[k]]),y=quantile(extevents[[k]],.9),label=paste("Event",k))

      }
    }
  }

  if (plot == TRUE) {
    legend("topright",legend=c("rising","falling","peak"),col=c("blue","red","orange"),lty=c(1,1,NA),pch=c(20,20,17))
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
