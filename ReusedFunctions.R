



#================================================================================================
# normal_outlier_range(): self-defined function for outlier detection under normal distribution
#==============================================================================================
normal_outlier_range <- function(df, sinfo){
  
  if(!is.na(sinfo$NORMAL_P)){p = sinfo$NORMAL_P}; k = qnorm(1-p/2)
  
  df = df %>%
    dplyr::summarise(lower_bound = mean(.data[[sinfo$VALUE_COL]]) - k*sd(.data[[sinfo$VALUE_COL]]),
                     upper_bound = mean(.data[[sinfo$VALUE_COL]]) + k*sd(.data[[sinfo$VALUE_COL]]))
  return(df)
}





