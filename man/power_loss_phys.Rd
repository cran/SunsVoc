% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tpk-power_loss-phys.R
\name{power_loss_phys}
\alias{power_loss_phys}
\title{Physical Model Power Loss Modes of Single period}
\usage{
power_loss_phys(init_piv, init_isc1sun, init_prs, degr_row)
}
\arguments{
\item{init_piv}{Dataframe containing current andvolatge psuedo-IV curves of initial
period, calculated from the the result of p_iv.week function applied to timeseries
dataframe of initial period.}

\item{init_isc1sun}{1 sun isc generated from the p_iv.week output from the initial
period dataframe.}

\item{init_prs}{extracted rs from the psuedo-IV curve of the initial period.}

\item{degr_row}{one row of output from IVXbyX function.}
}
\value{
dataframe containing information about power loss due to various power
loss modes for one given period
}
\description{
This function decouples power loss to different loss modes, specifically current mismatch,
recombination, uniform current loss, and Rs loss, from the changes in IV features
for a given period, used internally for power_loss_phys_bat function.
}
\examples{
\donttest{
df <- read_df_raw(df_wbw,0.02,7)
df_init <- select_init_df(df, days = 21)
init <- p_iv.week(df_init, temp = 30, N_c = 60)
init_piv <- data.frame(V = init$voc_corr, I = init$isc_1sun - init$isc)
init_piv <- dplyr::arrange(init_piv, .data$V)
init_pivf <- ddiv::IVfeature(I = init_piv$I, V = init_piv$V, crtvalb = 0.06)
init_prs <- init_pivf$Rs
init_isc1sun <- init$isc_1sun
df_full <- IVXbyX(df, corr_temp = 30, 60)
power_loss_phys(init_piv,init_isc1sun, init_prs,df_full[1,])
}

}
