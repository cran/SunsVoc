
#' Physical Model Power Loss Modes of Single period
#'
#' This function decouples power loss to different loss modes, specifically current mismatch,
#' recombination, uniform current loss, and Rs loss, from the changes in IV features
#' for a given period, used internally for power_loss_phys_bat function.
#'
#' @param init_piv Dataframe containing current andvolatge psuedo-IV curves of initial
#' period, calculated from the the result of p_iv.week function applied to timeseries
#' dataframe of initial period.
#' @param init_isc1sun 1 sun isc generated from the p_iv.week output from the initial
#' period dataframe.
#' @param init_prs extracted rs from the psuedo-IV curve of the initial period.
#' @param degr_row one row of output from IVXbyX function.
#'
#' @return dataframe containing information about power loss due to various power
#' loss modes for one given period
#' @examples
#' \donttest{
#' df <- read_df_raw(df_wbw,0.02,7)
#' df_init <- select_init_df(df, days = 21)
#' init <- p_iv.week(df_init, temp = 30, N_c = 60)
#' init_piv <- data.frame(V = init$voc_corr, I = init$isc_1sun - init$isc)
#' init_piv <- dplyr::arrange(init_piv, .data$V)
#' init_pivf <- ddiv::IVfeature(I = init_piv$I, V = init_piv$V, crtvalb = 0.06)
#' init_prs <- init_pivf$Rs
#' init_isc1sun <- init$isc_1sun
#' df_full <- IVXbyX(df, corr_temp = 30, 60)
#' power_loss_phys(init_piv,init_isc1sun, init_prs,df_full[1,])
#'}
#'
#' @importFrom rlang .data
#' @import dplyr
#' @export
power_loss_phys <- function(init_piv, init_isc1sun, init_prs,
                            degr_row) {

  degr_piv <- char_to_df(degr_row$piv)
#
#   # Rsh of initial and degraded pseudo IV curve
#   prsh_init <- init_prsh
#   prsh_degr <- degr_row$prsh

  # uniform current loss
  D_isc <- init_isc1sun - degr_row$isc_1sun

  mod_piv <- init_piv %>% dplyr::mutate(p_init = .data$V * I,
                                 i_c1 = I - D_isc,
                                 p_c1 = .data$i_c1 * .data$V)

  # uniform shunting loss
  # skipping uniform shunting due to insufficient low Ir data
  # mod_piv <- mod_piv %>% mutate(i_c2 = .data$i_c1, p_c2 = .data$i_c2 * .data$V)
  # mod_piv <- mod_piv %>% mutate(i_c2 = i_c1 - V/rsh_degr + V/rsh_init,
  #                               p_c2 = i_c2 * V)

  # recombination loss
  # J01 loss - decrease in Voc (degr suns-voc vs init suns-voc)
  # need to find 1sun voc for piv
  D_voc <- max(init_piv$V, na.rm = T) - max(degr_piv$V, na.rm = T)

  mod_piv <- mod_piv %>% mutate(v_c3 = .data$V - D_voc, p_c3 = .data$i_c1 * .data$v_c3)


  # Rs loss
  # comparing degr piv to real IV curve

  iv_rs <- degr_row$rs_fit
  degr_rs <- degr_row$prss # Rs of degraded pseudo IV curve

  # need to account for the difference in piv and iv
  mod_piv2 <- data.frame(v_c5 = degr_piv$V - degr_piv$I*iv_rs + degr_piv$I*degr_rs) %>%
    mutate(p_c5 = .data$v_c5 * degr_piv$I)

  pmp_init <- max(mod_piv$p_init, na.rm = T)
  pmp_c1 <- max(mod_piv$p_c1, na.rm = T)
  # pmp_c2 <- max(mod_piv$p_c2, na.rm = T)
  pmp_c3 <- max(mod_piv$p_c3, na.rm = T)
  pmp_degr <- max(degr_piv$I * degr_piv$V, na.rm = T)
  # pmp_c4 <- max(mod_piv$p_c1, na.rm = T)
  pmp_c5 <- max(mod_piv2$p_c5, na.rm = T)
  # pmp_IV <- max(degr_row$pmpp)
  pmp_IV <- degr_row$pmp_fit

  res_power <- data.frame(date = degr_row$date,
                          uni_I = -pmp_init + pmp_c1,
                          rec = -pmp_c1 + pmp_c3,
                          rs = -pmp_degr + pmp_c5,
                          I_mis = -pmp_c5 + pmp_IV)

  res_power <- res_power %>% mutate(date = degr_row$date)

  return(res_power)
}

#' Physical Model Power Loss Modes
#'
#' This function decouples power loss to different loss modes, specifically current mismatch,
#' recombination, uniform current loss, and Rs loss, from the changes in IV features.
#'
#' @param df_iv Dataframe containing psuedo-IV curves. Generate with \code{\link{IVXbyX}}.
#' @param init_df Dataframe containing first several weeks of real-world IV data.
#' Generate with \code{\link{select_init_df}}
#' @param corr_T The temperature from which to create the correction
#' factor. Pass only numeric values.
#' @param N_c Number of cells in series; The total number of cells in the system.
#' @param ddiv_param Parameter passed to ddiv::IVfeature for 'crtvalb'
#'
#' @return dataframe containing information about power loss due to various power loss modes
#'
#' @examples
#' \donttest{
#' df <- read_df_raw(df_wbw,0.02,7)
#' df_init <- select_init_df(df, days = 21)
#' df_full <- IVXbyX(df, corr_temp = 30, 60)
#' res <- power_loss_phys_bat(df_full, df_init, corr_T = 30, N_c = 60)
#' }
#' @importFrom rlang .data
#' @import dplyr
#' @export
power_loss_phys_bat <- function(df_iv, init_df, corr_T = 40, N_c, ddiv_param = 0.06){
  # create progress bar
  pb <- progress_estimated(nrow(df_iv))
  # initial pseudo IV constructed from the first several weeks
  init <- p_iv.week(init_df, temp = corr_T, N_c = N_c)
  init_piv <- data.frame(V = init$voc_corr,
                         I = init$isc_1sun - init$isc) %>%
    arrange(.data$V)

  # 1sun isc, Rs and Rsh are extracted with IV feature extraction
  init_pivf <- ddiv::IVfeature(I = init_piv$I, V = init_piv$V, crtvalb = ddiv_param)
  init_prs <- init_pivf$Rs
  # init_prsh <- init_pivf$Rsh
  init_isc1sun <- init$isc_1sun

  power_loss_mod <- function(df_iv, count, pb){
    df <- tryCatch(
      power_loss_phys(init_piv = init_piv, init_isc1sun = init_isc1sun,
                      init_prs = init_prs,
                      degr_row = df_iv[count,]),
      error = function(e) data.frame()
    )
    pb$tick()$print()
    return(df)
  }

  res_all <- 1:nrow(df_iv) %>%
    purrr::map_dfr(~power_loss_mod(df_iv, ., pb))

  return(res_all)
}
