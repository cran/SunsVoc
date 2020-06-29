## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
library(SunsVoc)

## -----------------------------------------------------------------------------
# below is the function used to read in our csv
# df_wbw <- read_df_raw_from_csv('../../data/sa42589-albsf_full_ddiv.csv', 0, 7)

colnames(df_wbw)

## ---- warning=FALSE-----------------------------------------------------------
library(dplyr)
library(purrr)
library(magrittr)
library(ddiv)
str_ddiv <- function(iv_str, pb) {
  iv <- char_to_df(iv_str)
  # initialize result for when IVfeature encounters an error
  err_df <- data.frame("Isc" = NA, "Rsh" = NA,
                       "Voc" = NA, "Rs" = NA,
                       "Pmp" = NA,"Imp" = NA,
                       "Vmp" = NA, "FF" = NA)

  # calculate ddiv with error trapping
  res <- df <- tryCatch(
    as.data.frame(IVfeature(I = iv$I, V = iv$V, num = 200, crt = 0.1, crtvalb = 0.2)),
    error = function(e) err_df
  )

  pb$tick()$print()
  return(res)
}

batch_ddiv <- function(df) {
  iv_list <- df$ivdf 
  pb <- dplyr::progress_estimated(length(iv_list))
  ddiv_df <- iv_list %>% map_dfr(~str_ddiv(iv_str = ., pb))
  res <- cbind(df$tmst, ddiv_df)
  return(res)
}

# it is recommended to test on a small number of iv curves
# to tweak the input parameters of ddiv::IVfeature function
test <- sample_n(df_wbw, 5)
ddiv_test <- batch_ddiv(test)
colnames(ddiv_test) <- c("tmst", "eisc", "ersh", "evoc", "exrs", "epmp", "eimp", "evmp", "exff" )

# bind ddiv result with original dataframe

test_res <- full_join(test, ddiv_test)

colnames(test_res)


## -----------------------------------------------------------------------------

T_corr <- median_temp(df_wbw)

T_corr


## -----------------------------------------------------------------------------

df_init <- select_init_df(df_wbw, days = 21)

colnames(df_init)

## -----------------------------------------------------------------------------

df_full <- IVXbyX(df_wbw, corr_temp = "median", 4)

colnames(df_full)

## -----------------------------------------------------------------------------
res <- power_loss_phys_bat(df_full, df_init, corr_T = T_corr, N_c = 4);

# Generate Table
knitr::kable(res[1:5, ], caption = "Power Loss Modes")

## -----------------------------------------------------------------------------
library(ggplot2)
ggplot(data = res, aes(x = date)) +
  geom_point(aes(y = uni_I, color = "Uniform current", shape = "Uniform current"), size = 2) +
  geom_point(aes(y = rec, color = "Recombination", shape = "Recombination"), size = 2) +
  geom_point(aes(y = rs, color = "Rs loss", shape = "Rs loss"),  size = 2) +
  geom_point(aes(y = I_mis, color = "I mismatch", shape = "I mismatch"), size = 2) +
  ylab(expression(paste(Delta, "Power (W)"))) +
  xlab("Date") +
  theme_bw() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 10),
        legend.position = "top") +
  scale_shape_manual(values = c(4, 8, 17, 16),
                     name = "Power loss \n mode") +
  scale_colour_discrete(name = "Power loss \n mode") 


