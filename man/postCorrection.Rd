% Post correction of hydroEvents pairing results
% Corrects the rainfall-runoff events identified after pairing in hydroEvents

\name{postCorrection}
\alias{postCorrection}
\title{post-processing of Rainfall-runoff Rvent Pairs}
\description{
Process/Correct the paired rainfall-runoff events identified based on two different methods
}

\usage{
postCorrection(events, method = "remove duplicates")
}

\arguments{
  \item{events}{Data frame. Paired rainfall-runoff events.}
  \item{method}{Character string. Correction method want to be applied. Can be either 
                                  "remove duplicates" or "shift duplicates".}
}

\details{
If method is "remove duplicates" the function searches for duplicated rainfall and 
runoff events and just keeps the first event. If a rainfall or runoff event ends 
after another event strats, the function adjusts the start of the second event. If 
the method is "shift duplicates", the function removes any runoff events which are 
embedded of other runoff events. Moreover, in this case, the function adjusts the start of 
any runoff event which starts before the previous runoff event ends.
}

\value{
A data frame of corrected events.
}

\seealso{
\code{\link{pairEvents}}, \code{\link{plotPairs}}
}

\examples{
\dontrun{
# Example usage
rain_events <- eventPOT(dataLoch, threshold = 1, min.diff = 1)
flow_events <- eventMaxima(dataBassRiver, delta.y = 200, delta.x = 1, threshold = 0)
paired_events <- pairEvents(rain_events, flow_events, lag = 5, type = 1)


corrected_events <- postCorrection(paired_events, method = "remove duplicates")
}
}
