## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width = 9,
  fig.height = 5
)

## ----setup--------------------------------------------------------------------
library(SunsVoc)

## -----------------------------------------------------------------------------
# below is the function used to read in our csv
# df_wbw <- read_df_raw_from_csv('../../data/sa42589-albsf_full_ddiv.csv', 0, 7)

colnames(df_wbw)


## ----eval=FALSE, warning=FALSE------------------------------------------------
#  # Load required packages
#  library(dplyr)
#  library(purrr)
#  library(magrittr)
#  library(ddiv)
#  
#  # I-V curve feature extraction using the IVfeature function
#  str_ddiv <- function(iv_str, pb) {
#    iv <- char_to_df(iv_str)
#    # initialize result for when IVfeature encounters an error
#    err_df <- data.frame("Isc" = NA, "Rsh" = NA,
#                         "Voc" = NA, "Rs" = NA,
#                         "Pmp" = NA,"Imp" = NA,
#                         "Vmp" = NA, "FF" = NA)
#  
#    # calculate ddiv with error trapping
#    res <- tryCatch(
#      as.data.frame(IVfeature(I = iv$I, V = iv$V, num = 200, crt = 0.1, crtvalb = 0.2)),
#      error = function(e) err_df
#    )
#    pb$tick()$print()
#    return(res)
#  }
#  
#  # Batch processing of I-V curves for feature extraction
#  batch_ddiv <- function(df) {
#    iv_list <- df$ivdf
#    pb <- dplyr::progress_estimated(length(iv_list))
#    ddiv_df <- iv_list %>% map_dfr(~str_ddiv(iv_str = ., pb))
#    res <- cbind(df$tmst, ddiv_df)
#    return(res)
#  }
#  
#  # It is recommended to test on a small number of I-V curves
#  # to tweak the input parameters of the IVfeature function
#  test <- sample_n(df_wbw, 5)
#  ddiv_test <- batch_ddiv(test)
#  colnames(ddiv_test) <- c("tmst", "isc", "rsh", "voc", "rs", "pmp", "imp", "vmp", "ff" )
#  
#  # Bind the ddiv result with the original dataframe
#  test_res <- full_join(test[,c(1,3:4)], ddiv_test[,c(1:2,4:8)])
#  
#  # See the column names of the extracted features
#  colnames(test_res)
#  

## -----------------------------------------------------------------------------
T_corr <- median_temp(df_wbw)

T_corr


## -----------------------------------------------------------------------------
# Read the raw data with tracer accuracy for filtering and time period for pseudo I-V curves
df <- read_df_raw(df_wbw, tracer_accuracy = 0.02, t_period = 7)

# Subset the data for the first 21 days
df_init <- select_init_df(df, days = 21) 

# See the column names of the initial dataframe
colnames(df_init)

## -----------------------------------------------------------------------------
# Psuedo-IV curve generation 
df_full <- IVXbyX(df, corr_temp = T_corr, N_c= 60)

# See the column names of the generated data
colnames(df_full)

## -----------------------------------------------------------------------------
# Power loss mode calculation
res <- power_loss_phys_bat(df_full, df_init, corr_T = T_corr, N_c = 60) 

# Generate an example table for power loss modes
knitr::kable(res[1:5, ], caption = "Power Loss Modes")

## -----------------------------------------------------------------------------
library(ggplot2)
ggplot(data = res, aes(x = date)) +
  geom_point(aes(y = uni_I, color = "Uniform current", shape = "Uniform current"), size = 2) +
  geom_point(aes(y = rec, color = "Recombination",  shape = "Recombination"), size = 2) +
  geom_point(aes(y = rs, color = "Rs loss", shape = "Rs loss"),  size = 2) +
  geom_point(aes(y = I_mis, color = "I mismatch", shape = "I mismatch"), size = 2) +
  ylab(expression(paste(Delta, "Power (W)"))) + xlab("Date") +
  theme_bw() +
  theme(axis.text = element_text(size = 12), axis.title = element_text(size = 12),
        legend.text = element_text(size = 10), legend.position = "top") +
  scale_shape_manual(values = c(4, 8, 17, 16), name = "Power loss \n mode") +
  scale_colour_discrete(name = "Power loss \n mode") 

