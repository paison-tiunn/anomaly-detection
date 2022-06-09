



#================================================================================================
# normal_outlier_range(): self-defined function for outlier detection under normal distribution
#==============================================================================================
normal_outlier_range <- function(df, sinfo){
  
  if(!is.na(sinfo$NORMAL_P)){p = sinfo$NORMAL_P}; k = qnorm(1-p/2)
  x = df[[sinfo$VALUE_COL]]
  feat = sinfo$VALUE_FEATURE
  if(feat=="4"){
    x = x[x!=0]; x = x[x!=1]; x = x[x!=100]
    x = (x/100) %>% map(~ log(.x/(1-.x)))
  }
  Mean = mean(x); Sd = sd(x)
  if(feat=="4"){
    y = list(lower_bound = (Mean - k*Sd) %>% map_dbl(~ 1/(1+exp(-.x))),
             upper_bound = (Mean + k*Sd) %>% map_dbl(~ 1/(1+exp(-.x))))
  }else{
    y = list(lower_bound = Mean - k*Sd, upper_bound = Mean + k*Sd)
  }
  return(y)
}

#=============================================================================
# param_estim_gamma(): self-defined function for gamma parameter estimation
#=============================================================================
param_estim_gamma = function(x){
  
  x = x[x!=0]; x = x[!is.na(x)]
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

gamma_jump_upper = function(df, sinfo, diff = 5){
  
  
  p = sinfo$JUMP_P_GAMMA
  Df = df[c(sinfo$TIME_COL, sinfo$VALUE_COL)]
  names(Df) = c("Time", "Value")
  Df = Df %>% mutate(Time = as.POSIXct(as.character(Time)),
                     TimeLag = lag(Time),
                     TimeDiff = Time - TimeLag,
                     ValueLag = lag(Value),
                     ValueDiff = abs(Value - ValueLag))
  
  if(units(Df$TimeDiff)=="mins"){
    Df = Df %>% filter(TimeDiff <= diff)
  }else if(units(Df$TimeDiff)=="secs"){
    Df = Df %>% filter(TimeDiff <= diff*60)
  }else if(units(Df$TimeDiff)=="hours"){
    Df = Df %>% filter(TimeDiff <= diff/60)
  }
  
  x = Df$ValueDiff
  param = param_estim_gamma(x)
  y = qgamma(1-p, param[1], param[2])
  return(y)
}

#=======================================================================
# get_mainDir(): self-defined function for writeLog() and errorCatch
#======================================================================
get_mainDir = function(type){
  
  #檢查路徑
  mainDir <- "C:/Project/log"
  today <- Sys.time()
  year <- format(today, format = "%Y")
  month <- format(today, format = "%m")
  if(type=="txt"){day <- format(today, format = paste0("%F.", type))}
  if(type=="Rout"){day <- format(today, format = paste0("%F_%H-%M-%S.", type))}
  
  # year
  mainDir <- paste(mainDir, year, sep = "/", collapse = "/")
  if(!file.exists(mainDir)){
    dir.create(file.path(mainDir), showWarnings = FALSE)
  }
  
  # month
  mainDir <- paste(mainDir, month, sep = "/", collapse = "/")
  if(!file.exists(mainDir)){
    dir.create(file.path(mainDir), showWarnings = FALSE)
  }
  
  mainDir <- paste(mainDir, day, sep = "/", collapse = "/")
  return(mainDir)
}
