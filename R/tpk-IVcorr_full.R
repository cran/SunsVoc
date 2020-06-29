# correct iV curve by 1sun voc and 1sun isc (which is already included in df_piv)
# produce an dataframe to be passed to power_loss_bat
# to obtain power loss calculation

#source("./scripts/IVcorr.R")

#' Correct psuedo-IV curves by 1-sun Voc and Isc
#'
#' This method produces a dataframe with corrected IV curve
#' values that can be passed to power_loss_bat in order to calculate
#' power loss from IV curve data.
#'
#' @param df_red A filtered dataframe containing IV curve data. Obtain with \code{\link{filter_df_raw}}
#' @param df_piv A dataframe of psuedo-IV curve data.
#' Obtain with \code{\link{read_df_raw}} or \code{\link{read_df_raw_from_csv}}.
#'
#' @return Returns A dataframe with corrected IV curves.
#'
#' @export
IVcorr_full <- function(df_red, df_piv) {

  df <- dplyr::left_join(df_red, df_piv, by = "day")

  pb <- utils::txtProgressBar(min = 1, max = nrow(df),
                       initial = 1, style = 3)

  try_IVcorr <- function(count) {
    # obtain the row by row number
    iv_row <- df[count, ]
    # returns an empty dataframe when there is error in IV correction function
    new_row <- tryCatch(IVcorr(iv_row),
                        error = function(e) data.frame())
    utils::setTxtProgressBar(pb, value = count)
    return(new_row)
  }

  df_corr <- 1:nrow(df) %>% purrr::map_dfr(~try_IVcorr(.))

  return(df_corr)
}
