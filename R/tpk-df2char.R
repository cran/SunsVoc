
#' Convert From Dataframe to Hbase Char String
#'
#' The companion function to this one, \code{\link{char_to_df}},
#' parses this string and creates an additional dataframe from it.
#' This function works the other way, converting that dataframe back to a character
#' string.
#'
#' @param df The dataframe, typically named "ivdf", to be converted.
#'
#' @return Returns a character string representing an IV curve.
#'
#' @examples
#' df2chr(char_to_df(df_wbw$ivdf[1]))
#'
#' @export
df2chr <- function(df){
  chr <- "V*I#"
  for (i in 1:(nrow(df))) {

    #chr <- stringr::str_c(chr, df[i, 1], "*")

    #chr <- stringr::str_c(chr, df[i, 2], "#")
    chr <- paste0(chr,df[i,1],"*")
    chr <- paste0(chr,df[i,2],"#")
  }
  return(chr)
}
