#' @title Streamflow data
#' @description Example data for Bass River at Loch
#' @details This data is obtained from Grayson et al (1996)
#' @format A vector of 67 daily streamflow values in (ML/day)
#' @seealso \code{\link{dataLoch}}
#' @references Grayson, R., Argent, R. M., Nathan, R. J., McMahon, T. A. & Mein, R. G. (1996) Hydrological
#' Recipes, Cooperative Reserach Centre for Catchment Hydrology, Melbourne.
"dataBassRiver"

#' @title Rainfall data
#' @description Example rainfall data for Loch (Station ID 086067) for 30/06/1974-04/09/1974
#' @format A vector of 67 daily rainfall values in (mm)
#' @seealso \code{\link{dataBassRiver}}
#' @source \url{http://www.bom.gov.au/climate/data/stations/}
"dataLoch"

#' @title Catchment data
#' @description Example data for Bass River at Loch
#' @format
#' A data frame with 1559 rows and 5 variables:
#' Each element contains data for a catchment in the order of:
#' A5030502, 314213, G8150018, G0050115, 410061; with corresponding names:
#' Scott Creek, Black River, Elizabeth River, Hugh River and Adelong Creek
#' Each of the 5 elements contains 4 data variables:
#' $P, $E, $Q: the time-series of daily P, PET and Q, all in mm/d.
#' Q was converted from gauge values (ML/d) to mm/d by dividing catchment area, P and PET are averaged over catchment area.
#' $site: the HRS gauge ID
#' @source \url{http://somewhere.important.com/}
#' @examples
#' plot(NULL, xlim = c(110, 160), ylim = c(-50, 0), type = "n")
#' for (i in 1:length(mapAust50m)) {
#' lines(mapAust50m[[i]])
#' }
#' for (j in 1:5) {
#'  dataHRS_sample[[i]]
#' }
"dataHRS_sample"

#' @title Water quality data
#' @description Example water quality data for MOORABOOL RIVER WEST BRANCH at LAL LAL (Station ID 232210)
#' @format blah blah
#' @seealso \code{\link{dataBassRiver}}
#' @source \url{https://data.water.vic.gov.au/}
"data232210"

#' @title Flow-Precipitation data
#' @description Example data for water quality data for Black River at South Forest, Tasmania (Station ID 314213)
#' @format blah blah
#' @seealso \code{\link{dataBassRiver}}
#' @source \url{http://www.bom.gov.au/water/hrs/#id=314213/}
"data232210"
