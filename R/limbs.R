#' Extract rising/falling limbs
#'
#' @description ...
#'
#' @references  ...
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

# SASS method used to define threshold rates to separate limb
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
    legend("topright",legend=c("rising","falling","peak"),col=c("blue","red","orange"),lty=c(1,1,NA),pch=c(20,20,17))

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