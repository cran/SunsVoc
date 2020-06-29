
#' Read in Raw Data
#'
#' Given the file location of a .csv of Hbase-Formatted IV curve data,
#' this function reads in the data, filters missing temperature data,
#' and checks for a nonzero maximum power point, in case of power loss.
#' It resets the timestamps based on the minimum timestamp, and filters
#' Isc values for the tracer's accuracy.
#' Finally, a n_period counter is added to the dataframe.
#'
#' @param file_loc String; The location of the file, including the file name.
#' @param tracer_accuracy The accuracy of the IV tracer used. See the device's manual
#' to find the exact value at which Isc readings are no longer accurate.
#' @param t_period Data period for the Psuedo-IV curves. Addded as a column to the dataframe
#' based on the timestamp. Use units of days, i.e. daily periods should have t_period 1,
#' weekly periods should have t_period 7, etc.
#'
#' @return df_raw, a dataframe containing the raw IV curve data
#'
#' @examples
#' df_subset <- head(df_wbw, 3)
#'
#' tf <- tempfile()
#' write.csv(df_subset, tf)
#'
#' # uses 0 as tracer accuracy to avoid filtering
#' df_test <- read_df_raw_from_csv(tf, 0, 7)
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
read_df_raw_from_csv <- function(file_loc, tracer_accuracy, t_period) {

  df_raw <- data.table::fread(file_loc) %>%
    dplyr::filter(!is.na(.data$modt), .data$pmpp > 0) %>%
    dplyr::mutate(date = as.Date(.data$tmst)) %>%
    dplyr::mutate(day = (.data$date - min(.data$date)),
                  n_period = ceiling(.data$day/t_period)) %>% # day calculated so we can obtain n_period
    dplyr::filter(.data$ishc > tracer_accuracy)

  return(df_raw)
}

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
    dplyr::filter(!is.na(.data$modt), .data$pmpp > 0) %>%
    dplyr::mutate(date = as.Date(.data$tmst),
                  day = date - min(date),
                  n_period = ceiling(.data$day/t_period)) %>%
    dplyr::filter(.data$ishc > tracer_accuracy)

  return(df_raw)
}

#' Perform Temperature Filters on Raw Data
#'
#' Filters a raw dataframe of IV curve data according to specified
#' irradiance levels and temperature range. The function removes data below
#' the low irradiance and above the high irradiance thresholds, calculates
#' mean temperature of the remaining data, and then removes data outside of the
#' temp range around mean temperature (Celsius).
#'
#' @param df_raw A dataframe containing IV curve data.
#' @param low_irrad_thresh A lower bound for irradiance. Default = 995
#' @param high_irrad_thresh An upper bound for irradiance. Default = 1005
#' @param temp_range An acceptable range of temperature from the mean. The
#' filter will allow this value above and below the mean temperature, in Celcius.
#'
#' @return df_reduced, a necessary dataframe for IV Curve Translation (see \code{\link{IVcorr_full}}) This
#' dataframe has been filtered as described above.
#'
#' @examples
#' df_reduced <- filter_df_raw(df_wbw)
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
filter_df_raw <- function(df_raw, low_irrad_thresh = 950, high_irrad_thresh = 1050, temp_range = 3) {

  # keep only data with certain irradiance range and temperature range
  # select T range
  MED_T <- df_raw %>% dplyr::filter(.data$poay > 995 & .data$poay < 1005) %>%
    dplyr::summarise(mean(.data$modt, na.rm = T)) %>%
    as.numeric() %>%
    round

  # bounds for temperature
  LOW_T <- MED_T - temp_range
  UP_T <- MED_T + temp_range

  # filter df to high range of irradiance, within temp bounds defined above
  df_reduced <- df_raw %>% dplyr::filter(.data$poay > low_irrad_thresh & .data$poay < high_irrad_thresh,
                                         .data$modt > LOW_T & .data$modt < UP_T)

  return(df_reduced)
}

#' Generate Initial Dataframe for power_loss_bat
#'
#' The power_loss_bat function makes use of a dataframe containing
#' the first several days of psuedo-IV curves. This function creates
#' that dataframe for the user from the raw dataframe containing IV Curve
#' measurement data.
#'
#' @param df_raw The raw dataframe containing IV Curve measurement data.
#' @param days The number of initial days to subsample. Default: 21
#'
#' @return Subset of input dataframe.
#'
#' @examples
#' df_init <- select_init_df(df_wbw, days = 21)
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
