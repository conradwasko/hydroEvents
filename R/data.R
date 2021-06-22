#' @title Streamflow data
#' @description Streamflow data for Bass River at Loch (227219A) for 30/06/1974-04/09/1974
#' @details This data is obtained from Grayson et al (1996)
#' @format A vector of 67 daily streamflow values in (ML/day)
#' @seealso \code{\link{dataLoch}}
#' @references Grayson, R., Argent, R. M., Nathan, R. J., McMahon, T. A. & Mein, R. G. (1996) Hydrological
#' Recipes, Cooperative Reserach Centre for Catchment Hydrology, Melbourne.
"dataBassRiver"

#' @title Rainfall data
#' @description Rainfall data for Loch (Station ID 086067) for 30/06/1974-04/09/1974
#' @format A vector of 67 daily rainfall values in (mm)
#' @seealso \code{\link{dataBassRiver}}
#' @source \url{http://www.bom.gov.au/climate/data/stations/}
"dataLoch"

#' @title Catchment data
#' @description Example data for five sites across Australia
#' @format
#' A list with streamflow and catchment average precipitation and temperature for the following sites:
#' 120301B, 602004, 235203, 410044, 105105A, corresponding to Arid, Mediterranean, Temperate, Subtropical, and Tropical climates.
#' Catchment areas are 35326, 2433, 721, 1072, 297 km2 respectively.
#' Streamflow is from the Australian Bureau of Meteorology Hydrologic Reference Station network and catchment average climate variables
#' were extracted using AWAPer.
#' @source \url{http://www.bom.gov.au/water/hrs/}
#' @references Peterson, T.J., Wasko, C., Saft., & Peel, M.C. (2020) AWAPer: An R package for area weighted catchment
#' daily meteorological data anywhere within Australia, Hydrological Processes, 34, 1301-1306.
#' @references Jones, D., Wang, W., & Fawcett, R., 2009. High-quality spatial climate data-sets for Australia. Aust. Meteorol. Oceanogr. J. 58, 233–248.
"dataCatchment"
