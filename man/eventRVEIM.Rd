% Robust Variance-based Event Identification Method (RVEIM)
% Identifies rainfall-runoff events given a set of rainfall and runoff time series 

\name{eventRVEIM}
\alias{eventRVEIM}
\title{Robust Variance-based Event Identification Method (RVEIM)}
\description{
Identifies rainfall-runoff events given rainfall and runoff time series.
}

\usage{
eventRVEIM(rainfall, streamflow, dvar = 3, alpha = 0.925, out.style = "summary")
}

\arguments{
  \item{rainfall}{Numeric vector. Daily rainfall data (mm) for the study period.}
  \item{streamflow}{Numeric vector. Daily streamflow data (ML) corresponding to the same period as rainfall.}
  \item{dvar}{Integer. Length of moving variance window (days).}
  \item{alpha}{Numeric. Filter parameter of the Lyne and Hollick (1979) baseflow filter.}
  \item{out.style}{The type of output (currently either "summary" or "none")}
}

\details{
The Robust Variance-based Event Identification Method (RVEIM) identifies rainfall–runoff events based on sudden streamflow movements. 
It mimics the real runoff generation process and typically produces more reliable results (runoff coefficients are mostly below 1).
RVEIM uses two parameters and is robust against parameter changes, making it suitable for large-scale applications.
}

\value{
A \code{data.frame} containing start and end dates of rainfall events (first two columns) 
and corresponding runoff events (last two columns).

By default, the \code{out.style} returns the indices of the maximum in each event, as well as the value of
the maximum and the sum of the \code{data} in each event, alongside the start and end of the events. Otherwise just
the indices of start and end of events as a two column dataframe are returned.
}

\seealso{
\code{\link{baseflowA}}, \code{\link{calcREIC}}, 
\code{\link{eventMaxima}}, \code{\link{eventMinima}}, \code{\link{eventBaseflow}}
}

\examples{
# Example usage
events <- eventRVEIM(dataLoch, dataBassRiver, dvar = 3, alpha = 0.925)
}
\references{
  Mohammadpour Khoie, M. M., Guo, D. & Wasko, C. (2025) Improving the consistency of hydrologic event identification<doi:10.1016/j.envsoft.2025.106521>.
}