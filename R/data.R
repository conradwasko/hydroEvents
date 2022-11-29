#' @title Streamflow data
#' @description Streamflow data for Bass River at Loch (227219A) for 30/06/1974-04/09/1974
#' @details This data is obtained from Grayson et al (1996)
#' @format A vector of 67 daily streamflow values in (ML/day)
#' @seealso \code{\link{dataLoch}}
#' @references Grayson, R., Argent, R. M., Nathan, R. J., McMahon, T. A. & Mein, R. G. (1996) Hydrological
#' Recipes, Cooperative Research Centre for Catchment Hydrology, Melbourne.
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

#' @title Example water quality and streamflow data
#' @description Data from 4 HRS (Hydrologic Reference Stations, Australian Bureau of Meteorology) catchments are included:
#' catchment IDs: 410073, 424002, G8150018, A5020502.
#' @format
#' Water quality (WQ) and streamflow (Q) data at matching time steps from 4 HRS catchments.
#' Each dataset (qdata and wqdata) is a list of length 4, corresponding to the 4 catchments.
#' @source
#' HRS streamflow data:
#' \url{http://www.bom.gov.au/water/hrs/}
#' water quality data:
#' WaterNSW: \url{https://www.waternsw.com.au/waterinsights/real-time-data}
#' Northern Territory Department of Environment, Parks and Water Security \url{https://water.nt.gov.au/}
#' South Australia Department for Environment and Water \url{https://www.waterconnect.sa.gov.au/}
"WQ_Q"

#' @title Example sub-daily rainfall and tidal water level data
#' @description Hourly rainfall (P) and water level (WL) at Burnie, Tasmania for 1997-01-14 to 1997-02-14 (Pluvio ID: 091009; Tide gauge: IDO71005)
#' @format
#' Each of P and WL data is a simple vector with no timestamp. The original data is in hourly time step.
#' @source
#' Sub-daily rainfall data are from Australian Bureau of Meteorolgy:
#' \url{http://www.bom.gov.au/climate/data/stations/}
#' Sub-daily tidal water level data are from Australian Bureau of Meteorolgy Australian Baseline Sea Level Monitoring Project:
#' \url{http://www.bom.gov.au/oceanography/projects/abslmp/data/index.shtml}
"data_P_WL"
