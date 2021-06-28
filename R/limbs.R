#' Extract rising/falling limbs
#'
#' @description ...
#'
#' @references  ...
#'
#' @param data The data vector (e.g. a streamflow time series)
#' @param dates date variable, default to NULL (inputing data as a simple vector)
#' @param events event extracted
#' @param plot c(T,F) whether a plot is produced for the limbs
#' @param main desired title of the plot if plot=T
#' @param filter c("simple", "spline") Optional smoothing of data series
#' @param min.rates vector of length 2, mininum increasing rate during rising limb & minimum decreasing rate during falling limb
#'
#'
#' @return Returns indices of start and end of events and the rising/falling limbs within each event
#'
#' @export
#' @keywords events
#' @examples
#' library(hydroEvents)
#' data("WQ_Q")
#' qdata=WQ_Q$qdata[[1]]
#' Q = as.vector(qdata$Q_cumecs)
#' BF_res = eventBaseflow(Q)
#' limbs(data = Q, dates=NULL, events = BF_res, main="with 'eventBaseflow'")
limbs <- function(data,dates=NULL,events,plot=T,main="Event hydrographs",filter=F,min.rates=c(0,0)) {
  if (!is.null(dates)) {
    if(plot==T) {
      plot(data~dates,type="o",pch=20,cex=0.7,main=main,col="grey")
    }
  } else {
    if(plot==T) {
      plot(data,type="o",pch=20,cex=0.7,main=main,col="grey")
    }

    extevents = eventid = list()
    rising = falling = matrix(NA,nrow(events),2)
    dropevent= vector()
    for (k in 1:nrow(events)) {
      extevents[[k]] = data[events$srt[k]:events$end[k]]

      if (!is.null(dates)) {
        eventid[[k]] = dates[events$srt[k]:events$end[k]]

      } else {
        eventid[[k]] = events$srt[k]:events$end[k]
      }
      if (filter==T) {
        # min.rates = min increasing rate during rising & min decreasing rate during falling
        if (any(diff(extevents[[k]])>=min.rates[1])&any(diff(extevents[[k]])<=min.rates[2])) {
          if (all(c(min(which(diff(extevents[[k]])>=min.rates[1])),
                    max(which(diff(extevents[[k]])>=min.rates[1]))+1,
                    max(which(diff(extevents[[k]])<=min.rates[2]))+1)%in%c(1:length(eventid[[k]])))) {
            rising[k,] =eventid[[k]][c((min(which(diff(extevents[[k]])>=min.rates[1]))),
                                       (max(which(diff(extevents[[k]])>=min.rates[1]))+1))]
            falling[k,] =eventid[[k]][c((max(which(diff(extevents[[k]])>=min.rates[1]))+1),
                                        (max(which(diff(extevents[[k]])<=min.rates[2]))+1))]
          } else {
            dropevent = c(dropevent,k)
            rising[k,] = falling[k,] = c(NA,NA)
          }

        }  else {
          dropevent = c(dropevent,k)
          rising[k,] = falling[k,] = c(NA,NA)

        }


      } else {
        rising[k,] =eventid[[k]][c(1,(min(which(diff(extevents[[k]])<0))))]
        falling[k,] =eventid[[k]][c((min(which(diff(extevents[[k]])<0))),length(eventid[[k]]))]
      }

      rising=as.data.frame(rising)
      falling=as.data.frame(falling)
      colnames(rising)=c("ris.srt","ris.end")
      colnames(falling)=c("fal.srt","fal.end")

      if(plot==T) {
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
    if(plot==T) {
    legend("topright",legend=c("rising","falling","peak"),col=c("blue","red","orange"),lty=c(1,1,NA),pch=c(20,20,17))
}
    if (length(dropevent) > 0) {
      events=events[-dropevent,]
      extevents=extevents[-dropevent]
      eventid=eventid[-dropevent]
      rising=rising[-dropevent,]
      falling=falling[-dropevent,]
    }


  }
  res=cbind(events,rising,falling)
  return(res)
}
