% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pairEvents.R
\name{pairEvents}
\alias{pairEvents}
\title{Pair Events}
\usage{
pairEvents(events.1, events.2, lag = 5, type = 1)
}
\arguments{
\item{events.1}{Events of first data set}

\item{events.2}{Events of second data set}

\item{lag}{Maximum lag time (search radius) for pairing}

\item{type}{Method used to pair events (see details)}
}
\value{
Returns indices of start and end of events as well as the matched events as a four column dataframe.
}
\description{
Pairing of events performed either forwards or backwards within specified lag times.
}
\details{
Pairing can be performed forwards and backwards and centrally.\code{events.1} and \code{events.2} need to be
a dataframe with column names appropriate to the method type.
That is, if pairing needs a time of maximum then "which.max" is expected (see examples). Column names are taken from
the function event matching functions. The method types are:

\itemize{
\item{Type = 1:} Search for the peak in \code{events.2} within the start of \code{event.1} to the end of \code{event.1} + \code{lag}
\item{Type = 2:} Search for an end in \code{events.2} within the start of \code{event.1} to the end of \code{event.1} + \code{lag}
\item{Type = 3:} Search for the peak in \code{events.1} within the start of \code{event.2} - \code{lag} to the peak in \code{event.2}
\item{Type = 4:} Search for a start in \code{events.1} within the start of \code{event.2} - \code{lag} to the start of \code{event.2}
\item{Type = 5:} Search for the peak in \code{events.2} within the peak of \code{event.1} - \code{lag} to the peak of \code{event.1} + \code{lag}
}

It is appropriate to pick a lag time that is equivalent to the catchment time of concentration if matching rainfall to streamflow.
}
\examples{
# Load package
library(hydroEvents)
# Identify events
srt = as.Date("2015-02-05")
end = as.Date("2015-04-01")
idx = which(dataCatchment$`105105A`$Date >= srt & dataCatchment$`105105A`$Date <= end)
dat = dataCatchment$`105105A`[idx,]
events.P = eventPOT(dat$Precip_mm, threshold = 1, min.diff = 2)
events.Q = eventMaxima(dat$Flow_ML, delta.y = 2, delta.x = 1, thresh = 70)
# Plot events
oldpar <- par(mfrow = c(2, 1), mar = c(3, 2.7, 2, 1))
plotEvents(dat$Precip_mm, events = events.P, type = "hyet", colpnt = "#E41A1C",
   colline = "#E41A1C", ylab = "Precipitation (mm)", xlab = "Index", main = "2015")
plotEvents(dat$Flow_ML, events = events.Q, type = "lineover", colpnt = "#E41A1C",
   colline = "#377EB8", ylab = "Flow (ML/day)", xlab = "Index", main = "")
par(oldpar)
# Pair events
matched.1 = pairEvents(events.P, events.Q, lag = 5,  type = 1)
matched.2 = pairEvents(events.P, events.Q, lag = 5,  type = 2)
matched.3 = pairEvents(events.P, events.Q, lag = 3,  type = 3)
matched.4 = pairEvents(events.P, events.Q, lag = 7, type = 4)
matched.5 = pairEvents(events.P, events.Q, lag = 5, type = 5)
# Plot Pairs
oldpar <- par(mfrow = c(5, 1), mar = c(2, 3, 2, 3))
plotPairs(data.1 = dat$Precip_mm, data.2 = dat$Flow_ML, events = matched.1,
   col = rainbow(nrow(events.P)), ylab.1 = "P (mm)", ylab.2 = "Q (ML/day)", cex.2 = 0.66)
plotPairs(data.1 = dat$Precip_mm, data.2 = dat$Flow_ML, events = matched.2,
   col = rainbow(nrow(events.P)), ylab.1 = "P (mm)", ylab.2 = "Q (ML/day)", cex.2 = 0.66)
plotPairs(data.1 = dat$Precip_mm, data.2 = dat$Flow_ML, events = matched.3,
   col = rainbow(nrow(events.P)), ylab.1 = "Q (ML/day)", ylab.2 = "P (mm)", cex.2 = 0.66)
plotPairs(data.1 = dat$Precip_mm, data.2 = dat$Flow_ML, events = matched.4,
   col = rainbow(nrow(events.P)), ylab.1 = "Q (ML/day)", ylab.2 = "P (mm)", cex.2 = 0.66)
plotPairs(data.1 = dat$Precip_mm, data.2 = dat$Flow_ML, events = matched.5,
   col = rainbow(nrow(events.P)), ylab.1 = "P (mm)", ylab.2 = "Q ML/day)", cex.2 = 0.66)
par(oldpar)
}
\seealso{
\code{\link{calcStats}} \code{\link{eventBaseflow}} \code{\link{eventMaxima}} \code{\link{eventMinima}} \code{\link{eventPOT}}
}
\keyword{events}
