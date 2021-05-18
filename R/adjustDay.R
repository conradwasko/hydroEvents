#' Adjust a calendar year day to a water year day
#'
#' @description
#' blah blah
#'
#' @param m Month designated as start of water year
#' @param x The date(s) (must be format Date)
#'
#' @details No additional details provided.
#'
#' @return Returns the month with the lowest rainfall
#' @keywords events
#' @seealso \code{\link{minMonth}}
#' @export
#' @examples
#' # Extract event statistics from quickflow
#' Example here
#'

adjustDay <- function(m, x) {
  if (class(x) != "Date") stop("x must be of class Date")
  day.zero.1 = as.Date(paste0(substr(x, 1, 4), "-", m, "-", 1))
  day.zero.2 = as.Date(paste0(as.numeric(substr(x, 1, 4))-1, "-", m, "-", 1))
  day.of.event = as.Date(substr(x, 1, 10))
  idx = as.numeric(day.of.event - day.zero.1) < 0

  day.zero = day.zero.1
  day.zero[idx] = day.zero.2[idx]

  adjusted.day   = as.numeric(day.of.event - day.zero + 1)
  adjusted.month = as.numeric(substr(x, 6, 7)) + 1 - m
  adjusted.month[adjusted.month < 0] = adjusted.month[adjusted.month < 0] + 12
  adjusted.year  = as.numeric(format(day.zero, "%Y"))

  result = list(day = adjusted.day, month = adjusted.month, year = adjusted.year)
  return(result)
}

