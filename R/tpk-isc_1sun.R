# handle function to obtain the 1sun Isc value
#' Obtain 1-sun Isc Value
#'
#' This function is used internally by IVwbyw and others for the
#' calculation of 1-sun Isc values based on Isc and Irradiance measurements.
#'
#' @param isc Isc values
#' @param Irrad Ir radiance values
#'
#' @return Returns a calculated Isc value at 1-sun of Irradiance.
#'
#' @examples
#' isc_1sun <- isc.1sun(df_wbw$ishc, df_wbw$poay)
#'
#' @importFrom stats predict
#' @export
isc.1sun <- function(isc, Irrad){
  # fit linear model of isc and irradiance
  fit.isc <- stats::lm(isc ~ Irrad)
  # evaluate goodness of fit
  r2 <- summary(fit.isc)$adj.r.squared
  if (r2 < 0) {
    return(NA)
  } else {
    # calculated fitted 1 sun isc (at irradiance of 1000 W/m^2)
    isc_1sun <- predict(fit.isc, new = data.frame(Irrad = 1000))
    names(isc_1sun) <- "isc_1sun"
    return(isc_1sun)
  }
}
