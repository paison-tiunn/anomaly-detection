



#================================================================================================
# normal_outlier_range(): self-defined function for outlier detection under normal distribution
#==============================================================================================
normal_outlier_range <- function(df, sinfo){
  
  if(!is.na(sinfo$NORMAL_P)){p = sinfo$NORMAL_P}; k = qnorm(1-p/2)
  x = df[[sinfo$VALUE_COL]]; Mean = mean(x); Sd = st(x)
  y = list(lower_bound = Mean - k*Sd, upper_bound = Mean + k*Sd)
  return(y)
}

#=============================================================================
# param_estim_gamma(): self-defined function for gamma parameter estimation
#=============================================================================
param_estim_gamma = function(x){
  
  s = log(mean(x)) - mean(log(x))
  k = (3-s+sqrt((s-3)^2+24*s))/(12*s)
  theta = mean(x)/k
  return(c(k, 1/theta))
}

#=================================================================================
# gamma_outlier_range(): self-defined function for outlier detection under gamma
#==================================================================================
gamma_outlier_range <- function(df, sinfo, mode){
  
  if (!is.na(sinfo$GAMMA_P)){p = sinfo$GAMMA_P}else{print("")}
  
  #============================
  # filter out 0s and NAs
  #==========================
  x = df[[sinfo$VALUE_COL]]; x = x[x!=0]; x = x[!is.na(x)]
  
  #==================================
  # check if data is all negative
  #====================================
  negative_data = FALSE; if(max(x) < 0){x = -x; negative_data = TRUE}
  
  param = param_estim_gamma(x)
  
  if(mode==1){
    if(!negative_data){
      df1 = list(lower_bound = 0, upper_bound = qgamma(1-p, param[1], param[2]))
    }else{
      df1 = list(lower_bound = -qgamma(1-p, param[1], param[2]), upper_bound = 0)
    }
  }else if(mode==0){
    if(!negative_data){
      df1 = list(lower_bound = qgamma(p/2, param[1], param[2]), upper_bound = qgamma(1-p/2, param[1], param[2]))
    }else{
      df1 = list(lower_bound = -qgamma(1-p/2, param[1], param[2]), upper_bound = -qgamma(p/2, param[1], param[2]))
    }
  }else if(mode==-1){
    if(!negative_data){
      df1 = list(lower_bound = qgamma(p, param[1], param[2]), upper_bound = 999)
    }else{
      df1 = list(lower_bound = -999, upper_bound = -qgamma(p, param[1], param[2]))
    }
  }
  return(df1)
}

