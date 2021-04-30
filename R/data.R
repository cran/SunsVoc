# documentation of the 1-year dataset shipped with the package

#' 1 Year of raw outdoor IV curve data.
#'
#' This dataframe contains 1 year of anonymous raw outdoor time-series IV curve data.
#'
#' @format Dataframe with 4140 objects in 10 variables:
#' \describe{
#'     \item{tmst}{A local PosixCT Timestamp}
#'     \item{ivdf}{IV Dataframe}
#'     \item{modt}{Module Temperature}
#'     \item{poa}{Plane of array (POA) irradiance}
#'     \item{isc}{Extracted Short Circuit Current by ddiv}
#'     \item{voc}{Extracted Open Current Voltage by ddiv}
#'     \item{rs}{Extracted Rs (series resistance) by ddiv}
#'     \item{pmp}{Extracted Maximum Power by ddiv}
#'     \item{imp}{Extracted Current at Maximum Power Point}
#'     \item{vmp}{Extracted Voltage at Maximum Power Point}
#' }
#'
"df_wbw"
