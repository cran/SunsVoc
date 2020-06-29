# documentation of the 1year dataset shipped with the package

#' 1 Year of raw outdoor IV curve data.
#'
#' This dataframe contains 1 year of anonymous raw outdoor IV curve data.
#'
#' @format Dataframe with 28649 objects in 20 variables:
#' \describe{
#'     \item{tmst}{A local PosixCT Timestamp}
#'     \item{vocc}{Open Current Voltage}
#'     \item{ghir}{Global Horizontal Irradiance}
#'     \item{ivdf}{IV Dataframe}
#'     \item{imxp}{Current Max Power}
#'     \item{pmpp}{Maximum Power Point}
#'     \item{ishc}{Short Circuit Current}
#'     \item{modt}{Module Temperature}
#'     \item{vmpp}{Voltage Max Power Point}
#'     \item{ffff}{Fill Factor}
#'     \item{tmst_1}{}
#'     \item{poa_ratio}{Ratio between POA and GHI}
#'     \item{poay}{POA irradiance}
#'     \item{eisc}{Extracted Isc (short circuit current)}
#'     \item{ersh}{Extracted Rsh (shunt resistance)}
#'     \item{evoc}{Extracted Voc (open voltage voltage)}
#'     \item{exrs}{Extracted Rs (series resistance)}
#'     \item{epmp}{Extracted Pmp (maximum power point)}
#'     \item{eimp}{Extracted Imp (max power current)}
#'     \item{evmp}{Extracted Vmp (max power voltage)}
#'     \item{exff}{Extracted Fill Factor}
#'     \item{day}{the day, with 1 being the first day}
#'     \item{n_period}{The period this belongs to. Created by grouping specified in method}
#' }
#'
"df_wbw"
