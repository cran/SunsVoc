
# handle function to obtain pseudo I-V curve given a week (or more) of data

#' Obtain Psuedo IV Curve for given week
#'
#' @param df A dataframe containing IV curve data.
#' @param temp The temperature at which the data was obtained
#' @param alpha Parameter for "emp" and "val" methods.
#' @param N_c Number of cells in series. Equal to the total number of cells in the system.
#' @param isc_1sun (optional) Input a Isc 1-Sun value manually. Leave NULL to have one generated
#' from the dataframe.
#'
#' @return Psuedo-IV Curve data (dataframe) for a single, given week.
#'
#' @examples
#' df_slice <- dplyr::filter(df_wbw, df_wbw$n_period == 1)
#'
#' # Check that this has enough data! needs more than 10 rows to be meaningful
#' nrow(df_slice)
#'
#' # needs median temperature
#' temp <- median_temp(df_wbw)
#'
#' res <- p_iv.week(df_slice, temp = temp, N_c = 4)
#'
#'
#' @importFrom rlang .data
#' @importFrom magrittr %<>%
#' @import dplyr
#' @export
p_iv.week <- function(df, temp, alpha = 0.5, N_c, isc_1sun = NULL){

  # create new cols for fitting
  df <- df %>%
    dplyr::filter(.data$ishc > 1e-3)

  # The physical model
  df <- df %>% mutate(T_K = .data$modt + 273.15,
                      lnSun = 1.38e-23/1.6e-19 * N_c * .data$T_K * log(.data$ishc),
                      T_lnIsc2 = .data$lnSun * log(.data$ishc),
                      exrs = as.numeric(.data$exrs),
                      expVoc = exp(-.data$vocc/.data$T_K) / .data$ishc,
                      I0 = (N_c * 1.38e-23 * .data$T_K) / (1.6e-19 * .data$ishc)) # scaling for lm fit

  # select data that has non nan or inf Tln(Ir) values
  df %<>% filter(!(is.na(.data$lnSun) | is.infinite(.data$lnSun)))

  if (is.null(isc_1sun)) {
    # obtain 1sun isc
    isc_1sun <- isc.1sun(df$ishc, df$poay)
  }

  # correct voc by temperature and construct pseudo I-V curve
  voc_mod <- voc.corr(df)
  # replace all temperature to desired temperature as new predictor
  df_STC <- df %>%
    mutate(modt = temp)

  # correct voc to given at various irradiance level according to weekly model
    voc_corr <- predict(voc_mod, newdata = df_STC)
    mod_res <- summary(voc_mod)
    rmse <- sqrt(mean(voc_mod$residuals ^ 2))

    # obtain predicted voc value at 1000 W/m2 (with 1 sun isc)
    # and given temperature
    df_piv <- data.frame(voc_corr = voc_corr,
                         ishc = df$ishc)

  # modeling Imp and Vmp
    mod_imp <- stats::lm(eimp ~ eisc*modt, data = df)
    mod_vmp <- stats::lm(evmp ~ lnSun + T_lnIsc2 + modt, data = df)
    # modeling Rs
    mod_rs <- stats::lm(exrs ~ I0, data = filter(df, .data$ishc > 2 & .data$ishc < 10))

    ln_1Sun = 1.38e-23/1.6e-19*N_c*(temp+273.15)*log(isc_1sun)
    #predict imp and vmp for 1sun and given temp
    imp_fit <- stats::predict(mod_imp,
                       newdata = data.frame(eisc = isc_1sun, modt = temp))
    vmp_fit <- stats::predict(mod_vmp,
                       newdata = data.frame(lnSun = ln_1Sun,
                                            T_lnIsc2 = ln_1Sun * log(isc_1sun),
                                            modt = temp))
    res_vmp <- summary(mod_vmp)
    vmp_rmse <- sqrt(mean(mod_vmp$residuals ^ 2))
    pmp_fit <- imp_fit * vmp_fit
    res_rs <- summary(mod_rs)
    rs_rmse <- sqrt(mean(mod_rs$residuals ^ 2))
    names(imp_fit) <- "imp_fit"
    names(vmp_fit) <- "vmp_fit"
    names(pmp_fit) <- "pmp_fit"

  # Setting names attribute
    voc_1sun <- stats::predict(voc_mod,
                        newdata = data.frame(lnSun = ln_1Sun,
                                             modt = temp))
    names(voc_1sun) <- "voc_1sun"
    rs_fit <- stats::predict(mod_rs,
                      newdata = data.frame(I0 = N_c * 1.38e-23 * (temp + 273.15)/(1.6e-19 * isc_1sun)))
    names(rs_fit) <- "rs_fit"

  # Calculate ideality factor, create res
    n <- voc_mod$coefficients[2]
    names(n) <- "n"
    names(isc_1sun) <- "isc_1sun"

    res <- c(df_piv, isc_1sun, voc_1sun, n,
             imp_fit, vmp_fit, pmp_fit, rs_fit,
             "voc_adjR2" = mod_res$adj.r.squared,
             "voc_rmse" = rmse,
             "vmp_adjR2" = res_vmp$adj.r.squared,
             "vmp_rmse" = vmp_rmse,
             "rs_adjR2" = res_rs$adj.r.squared,
             "rs_rmse" = rs_rmse)

  return(res)
}

