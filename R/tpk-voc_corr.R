
# handle function to correct Voc with module temperature with physical model

#' Correct Voc for Module Temperature
#'
#' This method uses a physical model to correct Voc for the module
#' temperature. Since indoor Suns-Voc is conducted at a steady 25C,
#' a correction on the outdoor readings is necessary for meaningful
#' comparison.
#'
#' @param df A dataframe of IV curve data, containing an lnSun column generated
#' by the \code{\link{p_iv.week}} function.
#'
#' @return returns a list object of modeled Voc.
#'
#' @examples
#' # subset data to first period
#' df_slice <- dplyr::filter(df_wbw, df_wbw$n_period == 1)
#' N_c <- 4 # true of the example data. N_c is the number of cells in series
#'
#' df_slice <- dplyr::mutate(df_slice, T_K = .data$modt + 273.15,
#'                       lnSun = 1.38e-23/1.6e-19 * N_c * .data$T_K * log(.data$ishc),
#'                       T_lnIsc2 = .data$lnSun * log(.data$ishc),
#'                       exrs = as.numeric(.data$exrs),
#'                       expVoc = exp(-.data$vocc/.data$T_K) / .data$ishc,
#'                       I0 = (N_c * 1.38e-23 * .data$T_K) / (1.6e-19 * .data$ishc))
#'
#' voc_mod <- voc.corr(df_slice)
#'
#' @export
voc.corr <- function(df){

  # fit physical model
  fit.voc <- stats::lm(vocc ~ lnSun + modt, data = df)

  res <- fit.voc

  return(res)
}
