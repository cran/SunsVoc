
#' Convert From Hbase Char String to Dataframe
#'
#' Hbase saves some data automatically as a long string in a column
#' of a dataframe. This function parses this string and creates
#' an additional dataframe from it.
#'
#' @param str The character string to be converted to a Dataframe.
#'
#' @return Dataframe containing IV curve data.
#'
#' @examples
#' \donttest{
#' # generate Psuedo-IV Curves
#' df_full <- IVXbyX(df_wbw, corr_temp = "median", 4)
#'
#' # subset a single row
#' degr_row <- df_full[1,]
#'
#' degr_piv <- char_to_df(degr_row$piv)
#' }
#'
#' @export
char_to_df <- function(str){

  # Handle warnings. Save old ones, set them to come back on exit, and change locally
  # old <- getOption("warn")
  # on.exit(options(warn = old))
  # options(warn = -1)

  str1 <- str %>% stringr::str_split("\\#", simplify = TRUE)
  str2 <- str1 %>% stringr::str_split("\\*", simplify = TRUE)
  df <- as.data.frame(str2[2:length(str1),], stringsAsFactors = FALSE)
  colnames(df) <- c("V", "I")
  df$V <-  as.numeric(df$V)
  df$I <-  as.numeric(df$I)

  return(df)
}

