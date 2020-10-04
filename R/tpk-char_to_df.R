
#' Convert From Hbase Char String to Dataframe
#'
#' This function parses the I-V curve string and creates
#' an additional dataframe with current and voltage columns from it.
#'
#' @param str The character string to be converted to a Dataframe.
#'
#' @return Dataframe containing IV curve data.
#'
#' @examples
#' char_to_df(df_wbw$ivdf[1])
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

