
#' Convert From Dataframe to Hbase Char String
#'
#' Hbase saves some data automatically as a long string in a column
#' of a dataframe. The companion function to this one, \code{\link{char_to_df}},
#' parses this string and creates an additional dataframe from it.
#' This function works the other way, converting that dataframe back to a character
#' string.
#'
#' @param df The dataframe, typically named "ivdf", to be converted.
#'
#' @examples
#' # generate subset of first period
#' df_slice <- dplyr::filter(df_wbw, df_wbw$n_period ==1)
#'
#' temp <- median_temp(df_wbw)
#' res <- p_iv.week(df_slice, temp = temp, N_c = 4)
#'
#' isc_1sun <- res[[3]]
#' df_piv <- data.frame(voc_corr = res[[1]], isc_corr = isc_1sun - res[[2]])
#'
#' chr_piv <- df2chr(df_piv)
#'
#' @return Returns a character string representing an IV curve.
#'
#' @export
df2chr <- function(df){
  chr <- "V*I#"
  for (i in 1:(nrow(df))) {

    chr <- stringr::str_c(chr, df[i, 1], "*")

    chr <- stringr::str_c(chr, df[i, 2], "#")
  }
  return(chr)
}
