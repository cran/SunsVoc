
# handle function to correct Voc with module temperature by physical model

#' Correct Voc to certain reference conditions, used internally in piv_iv_week function.
#'
#' This method uses a physical model to correct Voc to certain reference conditions.
#' Since indoor Suns-Voc is conducted at a steady 25C,
#' a correction on the outdoor readings is necessary for meaningful
#' comparison.
#'
#' @param df A dataframe time series data with columns of voc, lnSun and modt, the
#' dataframe is converted from the request input dataframe like df_wbw by the piv_iv_week
#' function before using voc_corr function.
#
#' @return returns a list object of Voc model.
#'
#' @examples
#' df <- read_df_raw(df_wbw,0.02,7)
#' # subset data to first period
#' df_slice <- dplyr::filter(df, df$n_period == 1)
#' N_c <- 60 # true of the example data. N_c is the number of cells in series
#'
#' df_slice <- dplyr::mutate(df_slice, T_K = .data$modt + 273.15,
#' lnSun = 1.38e-23/1.6e-19 * N_c * .data$T_K * log(.data$isc),
#' isc2 = .data$isc^2,
#' T_lnIsc2 = .data$T_K * .data$lnSun * log(.data$isc),
#' rs = as.numeric(.data$rs),
#' expVoc = exp(-.data$voc/.data$T_K) / .data$isc,
#' I0 = (N_c * 1.38e-23 * .data$T_K) / (1.6e-19 * .data$isc))
#' voc_mod <- voc.corr(df_slice)
#'
#' @export
voc.corr <- function(df){

  # fit physical model
  fit.voc <- stats::lm(voc ~ lnSun + modt, data = df)

  res <- fit.voc

  return(res)
}
