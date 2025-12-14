% Robust Event Identification Criteria (REIC)
% Calculates the REIC value for a set of rainfall-runoff events given rainfall and runoff time series and events

\name{calcREIC}
\alias{calcREIC}
\title{Robust Event Identification Criteria (REIC)}
\description{
Calculates the REIC value for a set of rainfall–runoff events given rainfall, quickflow, 
and event data.
}

\usage{
calcREIC(rainfall, quickflow, events, n_rainfall, area)
}

\arguments{
  \item{rainfall}{Numeric vector. Rainfall data (mm) for the study period.}
  \item{quickflow}{Numeric vector. Quickflow (direct runoff) data (ML) corresponding to the same period as rainfall.}
  \item{events}{Data frame. Identified paired rainfall–runoff events, including rainfall and runoff events' start and end.}
  \item{n_rainfall}{Integer. The number of rainfall events detected.}
  \item{area}{Numeric. Catchment area in square kilometres.}
}

\details{
The Robust Event Identification Criteria (REIC) calculates the REIC value for a set of 
paired rainfall–runoff events based on two criteria outlined in Mohammadpour Khoie et al. (2025). 
It helps identify rainfall–runoff events that are both consistent and reliable (i.e., with fewer 
runoff coefficients greater than one).

The REIC metric is easily transferable and can be applied to various sets of rainfall–runoff 
events identified using different parameter values, helping users select the best-performing configurations.
}

\value{
A numeric vector of REIC values.
}

\references{
Mohammadpour Khoie, M.M., Guo, D., & Wasko, C. (2025). \emph{Improving the consistency of hydrologic event identification.} 
\emph{Environmental Modelling & Software}, 106521.
}

\seealso{
\code{\link{pairEvents}}, \code{\link{plotPairs}}
}

\examples{
\dontrun{
# Example usage
rain_events <- eventPOT(dataLoch, threshold = 1, min.diff = 1)
flow_events <- eventMaxima(dataBassRiver, delta.y = 200, delta.x = 1, 
threshold = 0)
paired_events <- pairEvents(rain_events, flow_events, lag = 5, type = 1)
quickflow <- baseflowA(dataBassRiver, alpha = 0.925, passes = 3)$bf

REIC_values <- calcREIC(dataLoch, quickflow, paired_events, 
n_rainfall = nrow(rain_events), area = 3012)
}
}
