#' Minimum monthly rainfall
#'
#' @description
#' Find the month of minimum monthly rainfall
#'
#' @param month Vector of months as integers or numeric (can be a time series)
#' @param precip The corresponding precipitation
#'
#' @details No additional details provided.
#'
#' @return Returns the month with the lowest rainfall
#' @keywords events
#' @seealso \code{\link{adjustDay}}
#' @export
#' @examples
#' # Extract event statistics from quickflow
#' Example here
#'
#'
minMonth <- function(month, precip) {
  srt.index = match(1, month)
  end.index = match(12, rev(month))
  n = length(month)

  month  = month[srt.index:(n - end.index + 1)]
  precip = precip[srt.index:(n - end.index + 1)]

  m = 1:12
  s = rep(NA_real_, 12)
  for (i in m) {
    s[i] = sum(precip[month == i])
  }
  return(which.min(s))
}

