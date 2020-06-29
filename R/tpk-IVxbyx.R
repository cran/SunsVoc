
#' Create x-by-x Psuedo-IV Curves
#'
#' This function moves through IV curve data x-by-x,
#' generating psuedo IV curves and binding them together into the output.
#'
#' @param df Dataframe containing IV data. Typically, a raw dataframe prior
#' to filtering for irradiance/temperature. Within this package environment, use
#' \code{\link{read_df_raw}} to generate this from a .csv
#' @param corr_temp The temperature from which to create the correction
#' factor. Pass the string "median" in order for the function to automatically
#' calculate the median module temperature at 1 sun irradiance and use it.
#' @param N_c Number of Cells in series; the total number of cells in the system.
#'
#' @return Psuedo-IV Curve data (dataframe) grouped from time periods of set length.
#'
#' @examples
#' \donttest{
#' df_full <- IVXbyX(df_wbw, corr_temp = "median", 4)
#'}
#'
#' @importFrom rlang .data
#' @importFrom magrittr %>%
#' @import dplyr
#' @import purrr
#' @export
IVXbyX <- function(df, corr_temp = "median", N_c){

  # text progress bar
  #pb <- utils::txtProgressBar(min = 1, max = max(df$n_period, na.rm = T),
  #                     title = "Calculating pseudo IV curve...",
  #                     initial = 1, style = 3)


  if(corr_temp == "median") {
    corr_temp <- median_temp(df)
  }

  # helper function that works for a single time period
  IV_slice <- function(df, n_incre, N_c, pb){
    # number of week1
    df_slice <- dplyr::filter(df, df$n_period == n_incre)
    # Not enough data, does not fit model
    if (nrow(df_slice) < 10) {
      res <- data.frame(t_incre = NA, corr_temp = NA,
                        isc_1sun = NA, piv = NA)
    } else {
      # calculate pseudo IV curve of each day
      res <- p_iv.week(df_slice,
                       temp = corr_temp,
                       N_c = N_c)

      # pmp <- max(res[[1]] * res[[2]])
      # obtain 1sun isc
      isc_1sun <- res[[3]]
      # obtain df of pseudo IV curve, not extrapolated or smoothed
      df_piv <- data.frame(voc_corr = res[[1]],
                           isc_corr = isc_1sun - res[[2]])
      chr_piv <- df2chr(df_piv)
      pmp_piv <- max(df_piv$voc_corr * df_piv$isc_corr, na.rm = T)
      # extract IV features of pseudo IV curve
      res_ivf <- ddiv::IVfeature(df_piv$isc_corr, df_piv$voc_corr)

      fitting_param <- as.data.frame(res[3:length(res)])
      df_res <- data.frame(date = min(df_slice$date), day = min(df_slice$day),
                           n_period = n_incre, piv = chr_piv,
                           pisc = res_ivf$Isc, pvoc = res_ivf$Voc,
                           prsh = res_ivf$Rsh, prss = res_ivf$Rs,
                           ppmp = res_ivf$Pmp, pimp = res_ivf$Imp,
                           pvmp = res_ivf$Vmp, pfff = res_ivf$FF,
                           stringsAsFactors = FALSE)
      df_res <- cbind(df_res, fitting_param)
      # update progress bar
      #utils::setTxtProgressBar(pb,n_incre)
    }
    return(df_res)
  }


  # obtain vectors of time increments to loop over
  incres <- unique(df$n_period)
  pb <- progress_estimated(length(incres))
  # return empty dataframe for error, to avoid map_df error
  try_IV <- function(df, n_incre, N_c, pb) {
    res <- tryCatch(
      IV_slice(df, n_incre, N_c),
      error = function(e) data.frame()
      )
    pb$tick()$print()
    return(res)
  }

  df_full <- incres %>% map_dfr(~try_IV(df, ., N_c, pb))
#
#   df_full <- data.frame()
#   for ( i in incres) {
#     df_full <- rbind(df_full, try_IV(df, i, N_c))
#   }

  # char_to_df_smooth <- function(str){
  #   str1 <- str %>% stringr::str_split("\\#", simplify = TRUE)
  #   str2 <- str1 %>% stringr::str_split("\\*", simplify = TRUE)
  #   df <- as.data.frame(str2[2:length(str1),], stringsAsFactors = FALSE)
  #   colnames(df) <- c("voc_corr", "isc_corr")
  #   df$voc_corr <-  as.numeric(df$voc_corr)
  #   df$isc_corr <-  as.numeric(df$isc_corr)
  #   df <- df[stats::complete.cases(df),]
  #
  #   return(df)
  # }

  return(df_full)
}
