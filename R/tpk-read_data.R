

#' Read in Raw Data from Dataframe
#'
#' Given an imported datafram of Hbase-Formatted IV curve data,
#' this function reads in the data, filters missing temperature data,
#' and checks for a nonzero maximum power point, in case of power loss.
#' It resets the timestamps based on the minimum timestamp, and filters
#' Isc values for the tracer's accuracy.
#' Finally, a n_period counter is added to the dataframe.
#'
#' @param df dataframe; the IV curve data to be filtered
#' @param tracer_accuracy The accuracy of the IV tracer used. See the device's manual
#' to find the exact value at which Isc readings are no longer accurate.
#' @param t_period Data period for the Psuedo-IV curves. Addded as a column to the dataframe
#' based on the timestamp. Use units of days, i.e. daily periods should have t_period 1,
#' weekly periods should have t_period 7, etc.
#'
#' @return df_raw, a dataframe containing the raw IV curve data
#'
#' @examples
#' df_test <- read_df_raw(df_wbw, 1, 7) # Weekly periods
#' df_test <- read_df_raw(df_wbw, 1, 1) # Daily periods
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
read_df_raw <- function(df, tracer_accuracy, t_period) {

  # In order to correctly generate n_period, we need to starting date
  min_tmst <- min(df$tmst)

  df_raw <- df %>%
    dplyr::filter(!is.na(.data$modt), .data$pmp > 0) %>%
    dplyr::mutate(date = as.Date(.data$tmst),
                  day = date - min(date) + 1,
                  n_period = ceiling(.data$day/t_period)) %>%
    dplyr::filter(.data$isc > tracer_accuracy)

  return(df_raw)
}


#' Generate Initial Dataframe for power_loss_bat
#'
#' The power_loss_bat function makes use of a dataframe containing
#' the first several days of psuedo-IV curves. This function creates
#' that dataframe for the user from the raw dataframe containing IV Curve
#' measurement data.
#'
#' @param df_raw The raw dataframe containing request input columns the same as
#' the example df_wbw dataset and the column of day, which starts from 1 and column
#' of n_period, which is the index of the period, decided by how many days to be
#' grouped as one period, the column of day and n_period can be generated from the
#' function read_df_raw
#' @param days The number of initial days to subsample. Default: 21
#'
#' @return Subset of input dataframe within the first several days decided by the
#' input parameter "days".
#'
#' @examples
#' df <- read_df_raw(df_wbw, 1, 7)
#' df_init <- select_init_df(df, days = 21)
#'
#' @importFrom rlang .data
#' @export
select_init_df <- function(df_raw, days = 21) {

  init_df <- dplyr::filter(df_raw, .data$day < days)
  return(init_df)
}


#' Calculate Correlation Temperature
#'
#'   if(corr_temp == "mean") {
#corr_temp <- df %>%
#  filter(poay > 995 & poay < 1005) %>%
#  summarise(mean(modt, na.rm = T)) %>%
#  as.numeric() %>%
#  round
#}
