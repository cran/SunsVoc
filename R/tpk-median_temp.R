
#' Calculate Median Temperature
#'
#' This function calculates the median module temperature throughout the data
#' to be used in corrections.
#'
#' @param df Dataframe containing timeseries irradiance (column name must be poa)
#' and module temperature (column name must be modt) in unit of Celsius.
#'
#' @return Returns an integer value of median reported module temperature of the data.
#'
#' @examples
#' T_corr <- median_temp(df_wbw)
#'
#' @importFrom rlang .data
#' @export
median_temp <- function(df) {
  corr_T <- df %>%
    dplyr::filter(.data$poa > 995 & .data$poa < 1005) %>%
    summarise(stats::median(.data$modt, na.rm = T)) %>%
    as.numeric() %>%
    round
  return(corr_T)
}
