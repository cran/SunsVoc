
#' Calculate Median Temperature
#'
#' This function calculates the median temperature throughout the data
#' to be used in corrections.
#'
#' @param df Dataframe containing IV Curve data (make sure it includes temperature).
#'
#' @return Returns an integer value of median reported temperature in the data.
#'
#' @examples
#' T_corr <- median_temp(df_wbw)
#'
#' @importFrom rlang .data
#' @export
median_temp <- function(df) {
  corr_T <- df %>%
    filter(.data$poay > 995 & .data$poay < 1005) %>%
    summarise(stats::median(.data$modt, na.rm = T)) %>%
    as.numeric() %>%
    round
  return(corr_T)
}
