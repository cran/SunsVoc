
# correct iV curve by 1sun voc and 1sun isc (which is already included in df_piv)
# produce an dataframe to be passed to power_loss_bat
# to obtain power loss calculation

#' Correct individual IV curve by 1-sun Voc and Isc
#'
#' This method produces a dataframe with corrected
#' values that can be passed to power_loss_bat in order to calculate
#' power loss from IV curve data. This function is a component of \code{\link{IVcorr_full}},
#' which runs this function on an entire dataframe in a loop
#'
#' @param iv_row A single row of psuedo-IV curve data.
#'
#' @return Returns a single row of corrected psuedo-IV curve data.
#'
#' @export
IVcorr <- function(iv_row) {

  df_iv <- char_to_df(iv_row$ivdf)
  isc_raw <- iv_row$ishc

  isc_1sun <- iv_row$pisc
  voc_raw <- iv_row$vocc

  # use the extracted value of piv voc as the 1sun voc
  voc_1sun <- iv_row$pvoc

  df_iv_corr <- data.frame(v = df_iv$V * voc_1sun/voc_raw, # equations in paper
                           i = df_iv$I * isc_1sun/isc_raw,
                           stringsAsFactors = F)
  new_row <- cbind(iv_row, ivcr = df2chr(df_iv_corr),
                   stringsAsFactors = F)
  return(new_row)
}
