



#================================================================================================
# 合理值域(常態)
#==============================================================================================
normal_outlier_range <- function(df, sinfo){
  
  if(!is.na(sinfo$NORMAL_P)){p = sinfo$NORMAL_P}; k = qnorm(1-p/2)
  x = df[[sinfo$VALUE_COL]]
  feat = sinfo$VALUE_FEATURE
  Max = sinfo$SENSOR_UP
  Min = sinfo$SENSOR_DOWN
  if(feat==4){
    x = x[x!=0]
    if(Max==100 && Min==0){
      CASE_TYPE = "[0, 100]"; x = x[x!=100]; x = (x/100)
    }else if(Max==1 && Min==0){
      CASE_TYPE = "[0, 1]"; x = x[x!=1]
    }else{
      stop("[SENSOR_UP, SENSOR_DOWN] needs to be [0, 100] or [0, 1] when VALUE_FEATURE = 4.")
    }
    x = x %>% map_dbl(~ log(.x/(1-.x)))
  }
  Mean = mean(x); Sd = sd(x)
  if(feat==4){
    y = list(lower_bound = 1/(1+exp(-(Mean - k*Sd))),
             upper_bound = 1/(1+exp(-(Mean + k*Sd))))
    if(CASE_TYPE=="[0, 100]"){
      y = y %>% map(~ .x*100)
    }
  }else{
    y = list(lower_bound = Mean - k*Sd,
             upper_bound = Mean + k*Sd)
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
# 合理值域(Gamma)
#==================================================================================
gamma_outlier_range <- function(df, sinfo, mode){
  
  if (!is.na(sinfo$GAMMA_P)){p = sinfo$GAMMA_P}else{print("")}
  MODE = sinfo$MODE
  
  #============================
  # filter out 0s and NAs
  #==========================
  x = df[[sinfo$VALUE_COL]]; x = x[x!=0]; x = x[!is.na(x)]
  
  #==================================
  # check if data is all negative
  #====================================
  negative_data = FALSE; if(max(x) < 0){x = -x; negative_data = TRUE}
  
  if(MODE=="a"){
    mode = 0
  }else if(MODE=="u"){
    if(negative_data){
      mode = -1
    }else{
      mode = 1
    }
  }else if(MODE=="d"){
    if(negative_data){
      mode = 1
    }else{
      mode = -1
    }
  }
  
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

#========================================================
# 前後跳動(Gamma)
#====================================================
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
  
  if(nrow(Df)==0){
    y = "NULL"
  }else{
    x = Df$ValueDiff
    param = param_estim_gamma(x)
    y = qgamma(1-p, param[1], param[2])
  }
  
  return(y)
}

#=======================================================================
# get_mainDir(): self-defined function for writeLog() and errorCatch
#======================================================================
get_mainDir = function(type){
  
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

#=========================================================================================
# 改變點偵測
#========================================================================================
changeDetect = function(data, sensorInfo){
  DATA = data
  data = DATA[[sensorInfo$VALUE_COL]]
  time = DATA[[sensorInfo$TIME_COL]]
  CHANGE_TIME = time[1]
  source = "[not defined yet]"
  stop = 0; cut_seq = NULL; CPD = 0
  while(stop==0){
    min = 1; max = n = length(data)
    message(paste0("--> stop==0, length(data)==", n))
    message(paste0("\n(max-min)==", max-min))
    if(max > min){
      while(TRUE){
        if((min+1)!=(max-1)){cut = sample((min+1):(max-1), 1)}else{cut = min+1}
        
        c1 = data[1:cut]; c2 = data[(cut+1):n]
        r1 = max(c1, na.rm = TRUE)-min(c1, na.rm = TRUE)
        r2 = max(c2, na.rm = TRUE)-min(c2, na.rm = TRUE)
        if(r1 > r2){max = cut+1}else{min = cut}
        message(paste0("(max-min)==", max-min))
        
        # check whether to break the while(TRUE) loop
        if((max-min)==2){
          mid = min+1
          c1 = data[1:min]; c2 = data[(min+1):n]
          r1 = max(c1, na.rm = TRUE)-min(c1, na.rm = TRUE)
          r2 = max(c2, na.rm = TRUE)-min(c2, na.rm = TRUE)
          r_min = abs(r1-r2)
          c1 = data[1:mid]; c2 = data[(mid+1):n]
          r1 = max(c1, na.rm = TRUE)-min(c1, na.rm = TRUE)
          r2 = max(c2, na.rm = TRUE)-min(c2, na.rm = TRUE)
          r_mid = abs(r1-r2)
          c1 = data[1:max]; c2 = data[(max+1):n]
          r1 = max(c1, na.rm = TRUE)-min(c1, na.rm = TRUE)
          r2 = max(c2, na.rm = TRUE)-min(c2, na.rm = TRUE)
          r_max = abs(r1-r2)
          
          if(r_min < r_max){
            if(r_min < r_mid){cut = min}else{cut = mid}
          }else{
            if(r_max < r_mid){cut = max}else{cut = mid}
          }
          message("\nBreak the while(TRUE) loop!"); break
        }
        
        # check whether to break the while(TRUE) loop
        if((max-min)==1){
          c1 = data[1:min]; c2 = data[(min+1):n]
          r1 = max(c1, na.rm = TRUE)-min(c1, na.rm = TRUE)
          r2 = max(c2, na.rm = TRUE)-min(c2, na.rm = TRUE)
          r_min = abs(r1-r2)
          c1 = data[1:max]; c2 = data[(max+1):n]
          r1 = max(c1, na.rm = TRUE)-min(c1, na.rm = TRUE)
          r2 = max(c2, na.rm = TRUE)-min(c2, na.rm = TRUE)
          r_max = abs(r1-r2)
          if(r_min < r_max){cut = min}else{cut = max}
          message("\nBreak the while(TRUE) loop!"); break
        }
      }
    }else{message(paste0("length(data)==", length(data)))}
    
    message("==> CPD results:")
    
    data1 = data[1:cut]; data2 = data[(cut+1):n]
    m1 = median(data1, na.rm = TRUE); m2 = median(data2, na.rm = TRUE)
    
    if(m1 < m2){
      message(paste0("[LEFT] 1st largest: ", round(sort(data1, TRUE)[1],2), ",\n",
                     " 2nd largest: ", round(sort(data1, TRUE)[2],2), ",\n",
                     " 3rd largest: ", round(sort(data1, TRUE)[3],2), ";\n",
                     "[RIGHT] 1st smallest: ", round(sort(data2)[1],2), ",\n",
                     " 2nd smallest: ", round(sort(data2)[2],2), ",\n",
                     " 3rd smallest: ", round(sort(data2)[3],2), ".\n"))
      if(max(data1, na.rm = TRUE) < min(data2, na.rm = TRUE)){
        stop = 0; cut_seq = c(cut_seq, cut); CPD = 1
      }else{
        stop = 1
        if(CPD==0){
          message("No change points are found.")
        }else{
          message("All change points are found.")
        }
      }
    }else{
      message(paste0("[LEFT] 1st smallest: ", round(sort(data1)[1],2), ",\n",
                     " 2nd smallest: ", round(sort(data1)[2],2), ",\n",
                     " 3rd smallest: ", round(sort(data1)[3],2), ";\n",
                     "[RIGHT] 1st largest: ", round(sort(data2, TRUE)[1],2), ",\n",
                     " 2nd largest: ", round(sort(data2, TRUE)[2],2), ",\n",
                     " 3rd largest: ", round(sort(data2, TRUE)[3],2), ".\n"))
      if(min(data1, na.rm = TRUE) > max(data2, na.rm = TRUE)){
        stop = 0; cut_seq = c(cut_seq, cut); CPD = 1
      }else{
        stop = 1
        if(CPD==0){
          message("No change points are found.")
        }else{
          message("All change points are found.")
        }
      }
    }
    if(stop==0){
      message(paste(length(data)))
      data = data[(cut+1):n]
      message(paste(length(data)))
      time = time[(cut+1):n]; CHANGE_TIME = time[1]
      message(paste0("Detected change point: ", CHANGE_TIME, "\n"))
    }else{
      message("Break the while(stop==0) loop!")
    }
  } # end of the while(stop==0) loop
  
  time = time[(cut+1):n]; CANDIDATE_TIME = time[1]
  message(paste0("Possible/suspicious/candidate change point: ", CANDIDATE_TIME))
  DATA = DATA %>% filter(.data[[sensorInfo$TIME_COL]] >= CHANGE_TIME)
  return(list("DATA" = DATA, "CPD" = CPD, "CHANGE_TIME" = CHANGE_TIME))
}

#======================
# 組合查詢字串
#====================================
getSensorSQL <- function(tbName, time_Col, value_col, sensorID, sid_Col, sdate, edate){
  
  sqlStr <- paste0("SELECT ", sid_Col, ",", value_col, ",", time_Col, " FROM ")
  sqlStr <- paste0(sqlStr, tbName, " where ", sid_Col, " = '", sensorID,"' and ")
  sqlStr <- paste0(sqlStr, time_Col, " between '", sdate, "' and '", edate, "'")
  
  return(sqlStr)
}

#=======================
# 設定資料庫連線
#================================================
setDBConnect <- function(ip,db,user,pwd){
  conn <- dbConnect(odbc(),
                    Driver = "{SQL Server Native Client 11.0}",
                    Server = ip,
                    Database = db,
                    UID = user,
                    PWD = pwd,
                    Port = 1433)
  conn
}

#===================================================
# writeLog(): self-defined function for log writing
#====================================================
writeLog <- function(msg, mainDir){
  
  fileConn <- file(mainDir)
  logMsg <- paste(format(Sys.time(), "%F %R :"), msg, sep = " ")
  write(logMsg, file = mainDir, append = TRUE)
  close(fileConn)
}

#================================================
# 最主要之運算函數
#============================================
calResult <- function(data, sensorInfo, AutoTriggered = FALSE){  
  
  #=====================================================
  # 首先呼叫 changeDetect() 進行改變點偵測
  #==================================================
  message("===Start CPD===")
  CPD_result = changeDetect(data, sensorInfo)
  message("===Finish CPD===")
  data = CPD_result$DATA
  changeFound = CPD_result$CPD
  if(changeFound==0){CHANGE_TIME = NULL}else{CHANGE_TIME = CPD_result$CHANGE_TIME}
  if(is.null(CHANGE_TIME)){CHANGE_TIME = "NULL"}else{CHANGE_TIME = paste0("'",CHANGE_TIME,"'")}
  
  #================================================
  # 將超過 [儀器合理範圍值] 之資料剔除
  #=============================================
  SENSOR_UP = sensorInfo$SENSOR_UP
  SENSOR_DOWN = sensorInfo$SENSOR_DOWN
  if (!is.na(SENSOR_DOWN)) {min = SENSOR_DOWN}
  if (!is.na(SENSOR_UP)) {max = SENSOR_UP}
  data = data %>% filter(between(.data[[sensorInfo$VALUE_COL]], min, max))
  
  #===================================================
  # 呼叫 gamma_outlier_range() 進行合理值域(Gamma)之估計
  #==============================================
  if(SENSOR_UP <= 0 || SENSOR_DOWN >= 0){
    message("-->Start Gamma")
    gammaResult = gamma_outlier_range(data, sensorInfo)
    GAMMA_UP = gammaResult$upper_bound
    GAMMA_DOWN = gammaResult$lower_bound
    if(is.na(GAMMA_UP) || abs(GAMMA_UP) %in% c(0,999)){GAMMA_UP = "NULL"}else{GAMMA_UP = round(GAMMA_UP , 3)}
    if(is.na(GAMMA_DOWN) || abs(GAMMA_DOWN) %in% c(0,999)){GAMMA_DOWN = "NULL"}else{GAMMA_DOWN = round(GAMMA_DOWN , 3)}
    message("-->Finish Gamma")
  }else{
    GAMMA_UP = GAMMA_DOWN = "NULL"; message("-->Gamma not run")
  }
  
  #============================================================
  # 呼叫 normal_outlier_range() 進行合理值域(常態)之估計
  #=============================================================
  normalResult <- normal_outlier_range(data, sensorInfo)
  NORMAL_UP <- normalResult$upper_bound
  NORMAL_DOWN <- normalResult$lower_bound
  if(is.numeric(NORMAL_UP)){NORMAL_UP <- round(NORMAL_UP , 3)}
  if(is.numeric(NORMAL_DOWN)){NORMAL_DOWN <- round(NORMAL_DOWN , 3)}
  
  #============================================================
  # 呼叫 IQRoutlier_range() 進行合理值域(IQR)之估計
  #=============================================================
  IQRResult <-  IQRoutlier_range(data, sensorInfo)
  BOX_UP <- IQRResult$upper_bound
  BOX_DOWN <- IQRResult$lower_bound
  if(is.numeric(BOX_UP)){BOX_UP <- round(BOX_UP , 3)}
  if(is.numeric(BOX_DOWN)){BOX_DOWN <- round(BOX_DOWN , 3)}
  
  #============================================================
  # 呼叫 chebyshev_range() 進行合理值域(Chebyshev)之估計
  #=============================================================
  chebyshevResult <- chebyshev_range(data, sensorInfo)
  CHE_UP <- chebyshevResult$ODV_U
  CHE_DOWN <- chebyshevResult$ODV_L
  if(is.numeric(CHE_UP)){CHE_UP <- round(CHE_UP , 3)}
  if(is.numeric(CHE_DOWN)){CHE_DOWN <- round(CHE_DOWN , 3)}
  
  #======================================================================
  # 呼叫 chebyshev_jumpdata_na() 進行前後跳動(chebyshev)門檻值之估計
  #===============================================================================
  temp_outlier = chebyshev_jumpdata_na(data,sensorInfo,sensorInfo$VALUE_COL)
  JUMP_VALUE <- temp_outlier$ODV_U
  if(is.numeric(JUMP_VALUE)){JUMP_VALUE <- round(JUMP_VALUE , 3)}
  if(is.na(JUMP_VALUE)){JUMP_VALUE <- 0}
  
  #=================================================================
  # 呼叫 gamma_jump_upper() 進行前後跳動(Gamma)門檻值之估計
  #=============================================================
  JUMP_VALUE_GAMMA = gamma_jump_upper(data, sensorInfo)
  if(is.na(JUMP_VALUE_GAMMA)){
    JUMP_VALUE_GAMMA = "NULL"
  }else if(is.numeric(JUMP_VALUE_GAMMA)){
    JUMP_VALUE_GAMMA = JUMP_VALUE_GAMMA %>% round(3)
  }
  
  #==================================================
  # 更新資料表 Sensor_Info
  #=================================================
  sqlr_Update <- "UPDATE [dbo].[Sensor_Info] SET "
  sqlr_Update <- paste(sqlr_Update, "[CHE_UP] = " , CHE_UP, ", [CHE_DOWN] = ", CHE_DOWN, ", ")
  sqlr_Update <- paste(sqlr_Update, "[BOX_UP] = " , BOX_UP, ", [BOX_DOWN] = ", BOX_DOWN, ", ")
  sqlr_Update <- paste(sqlr_Update, "[NORMAL_UP] = " , NORMAL_UP,", [NORMAL_DOWN] = ", NORMAL_DOWN, ", ")
  sqlr_Update <- paste(sqlr_Update, "[GAMMA_UP] = " , GAMMA_UP,", [GAMMA_DOWN] = ", GAMMA_DOWN, ", ")
  sqlr_Update <- paste(sqlr_Update, "[JUMP_VALUE] = " , JUMP_VALUE,", [JUMP_VALUE_GAMMA] = ", JUMP_VALUE_GAMMA, ", ")
  sqlr_Update <- paste(sqlr_Update, "[CHANGE_TIME] = ", CHANGE_TIME)
  sqlr_Update <- paste(sqlr_Update, " WHERE [SN] = ", sensorInfo$SN)
  
  if(AutoTriggered){
    sqlr_Update <- "UPDATE [dbo].[Sensor_Info] SET "
    sqlr_Update <- paste(sqlr_Update, "[CALCUlATE_TIME] = '" , as.Date(sensorInfo$CALCUlATE_TIME) + sensorInfo$UPDATE_FQ, "'")
    sqlr_Update <- paste(sqlr_Update, " WHERE [SN] = ", sensorInfo$SN)
  }
  
  dbGetQuery(basicConn, sqlr_Update)
  
  #==================================================
  # 新增一筆資料到 STAT_HISTORY 資料表作為歷程記錄
  #==========================================================
  if(!AutoTriggered){
    sqlr_Insert <- "INSERT INTO [dbo].[STAT_HISTORY]([CALCUlATE_TIME],[DATA_RANGE],[SENSOR_UP],[SENSOR_DOWN],"
    sqlr_Insert <- paste0(sqlr_Insert, "[CHE_UP],[CHE_DOWN],[CHE_P1],[CHE_P2],")
    sqlr_Insert <- paste0(sqlr_Insert, "[BOX_UP],[BOX_DOWN],[NORMAL_UP],[NORMAL_DOWN],[NORMAL_P],")
    sqlr_Insert <- paste0(sqlr_Insert, "[GAMMA_UP],[GAMMA_DOWN],[GAMMA_P],[JUMP_VALUE],[JUMP_P1],[JUMP_P2],")
    sqlr_Insert <- paste0(sqlr_Insert, "[JUMP_VALUE_GAMMA],[JUMP_P_GAMMA],[CHANGE_TIME],[SN],[TIMESTAMP]) Values ('")
    sqlr_Insert <- paste0(sqlr_Insert, sensorInfo$EDATE, "', ", sensorInfo$DATA_RANGE, ", ", sensorInfo$SENSOR_UP, ", ", sensorInfo$SENSOR_DOWN, ", ")
    sqlr_Insert <- paste0(sqlr_Insert, CHE_UP, ", ", CHE_DOWN, ", ", sensorInfo$CHE_P1, ", ", sensorInfo$CHE_P2, ", ")
    sqlr_Insert <- paste0(sqlr_Insert, BOX_UP, ", ", BOX_DOWN, ", ", NORMAL_UP, ", ", NORMAL_DOWN, ", ", sensorInfo$NORMAL_P, ", ")
    sqlr_Insert <- paste0(sqlr_Insert, GAMMA_UP, ", ", GAMMA_DOWN, ", ", sensorInfo$GAMMA_P, ", ")
    sqlr_Insert <- paste0(sqlr_Insert, JUMP_VALUE, ", ", sensorInfo$JUMP_P1, ", ", sensorInfo$JUMP_P2, ", ")
    sqlr_Insert <- paste0(sqlr_Insert, JUMP_VALUE_GAMMA, ", ", sensorInfo$JUMP_P_GAMMA, ", ", CHANGE_TIME, ", ", sensorInfo$SN, ", getdate())")
  }else{
    sqlr_Insert <- "INSERT INTO [dbo].[STAT_HISTORY]([CALCUlATE_TIME],[DATA_RANGE],[SENSOR_UP],[SENSOR_DOWN],"
    sqlr_Insert <- paste0(sqlr_Insert, "[CHE_UP],[CHE_DOWN],[CHE_P1],[CHE_P2],")
    sqlr_Insert <- paste0(sqlr_Insert, "[BOX_UP],[BOX_DOWN],[NORMAL_UP],[NORMAL_DOWN],[NORMAL_P],")
    sqlr_Insert <- paste0(sqlr_Insert, "[GAMMA_UP],[GAMMA_DOWN],[GAMMA_P],[JUMP_VALUE],[JUMP_P1],[JUMP_P2],")
    sqlr_Insert <- paste0(sqlr_Insert, "[JUMP_VALUE_GAMMA],[JUMP_P_GAMMA],[CHANGE_TIME],[SN],[TIMESTAMP]) Values ('")
    sqlr_Insert <- paste0(sqlr_Insert, as.Date(sensorInfo$EDATE) + sensorInfo$UPDATE_FQ, "', ")
    sqlr_Insert <- paste0(sqlr_Insert, sensorInfo$DATA_RANGE, ", ", sensorInfo$SENSOR_UP, ", ", sensorInfo$SENSOR_DOWN, ", ")
    sqlr_Insert <- paste0(sqlr_Insert, CHE_UP, ", ", CHE_DOWN, ", ", sensorInfo$CHE_P1, ", ", sensorInfo$CHE_P2, ", ")
    sqlr_Insert <- paste0(sqlr_Insert, BOX_UP, ", ", BOX_DOWN, ", ", NORMAL_UP, ", ", NORMAL_DOWN, ", ", sensorInfo$NORMAL_P, ", ")
    sqlr_Insert <- paste0(sqlr_Insert, GAMMA_UP, ", ", GAMMA_DOWN, ", ", sensorInfo$GAMMA_P, ", ")
    sqlr_Insert <- paste0(sqlr_Insert, JUMP_VALUE, ", ", sensorInfo$JUMP_P1, ", ", sensorInfo$JUMP_P2, ", ")
    sqlr_Insert <- paste0(sqlr_Insert, JUMP_VALUE_GAMMA, ", ", sensorInfo$JUMP_P_GAMMA, ", ", CHANGE_TIME, ", ", sensorInfo$SN, ", getdate())")
  }
  
  
  dbGetQuery(basicConn, sqlr_Insert)
}


#=====================================================
# 合理值域(IQR)
#==============================================
IQRoutlier_range <- function(df, sinfo){
  
  df1 = df %>%
    dplyr::summarise(lower_bound=quantile(.data[[sinfo$VALUE_COL]],probs = 0.25)-1.5*IQR(.data[[sinfo$VALUE_COL]]),
                     upper_bound=quantile(.data[[sinfo$VALUE_COL]],probs = 0.75)+1.5*IQR(.data[[sinfo$VALUE_COL]]))
  return(df1)
}

#=========================================================
# 前後跳動(Chebyshev)之主要函數 by Tina
#=====================================================
chebyshev_jumpdata_na <- function(df, sinfo, var){
  
  # 預設值設定
  p1 = 0.1; p2 = 0.05
  
  if (!is.na(sinfo$JUMP_P1)) {p1 = sinfo$JUMP_P1}
  if (!is.na(sinfo$JUMP_P2)) {p2 = sinfo$JUMP_P2}
  
  ODV_LU = chebyshev_jumprange_na(df, sinfo, p1, p2)
  
  return(ODV_LU)
}

#=============================================
# 前後跳動(Chebyshev)之相關函數 by Tina
#==============================================
chebyshev_jumprange_na <- function(df, sinfo, p1 = 0.1, p2){
  
  var = as.character(sinfo$VALUE_COL)
  
  data0 = na_median(df, var, sinfo)
  
  data0[[var]] <- as.numeric(data0[[var]])
  
  data = data0 %>%
    mutate("前時間" = lag(ltime, default = ltime[1]),
           "前面一個" = lag(data0[[var]], default = data0[[var]][1]),
           "前後差" = abs(前面一個 - data0[[var]]))
  
  k1 = 1/sqrt(p1)
  ODV_1LU = data  %>%
    dplyr::summarise(前後差平均_all=mean(前後差),前後差標準差_all=sd(前後差)) %>%
    mutate(ODV_1L = 前後差平均_all-k1*前後差標準差_all,
           ODV_1U = 前後差平均_all+k1*前後差標準差_all)
  
  k2 = 1/sqrt(p2)
  ODV_LU = data %>%
    filter(between(前後差,ODV_1LU$ODV_1L,ODV_1LU$ODV_1U)) %>%
    dplyr::summarise(前後差平均_trun=mean(前後差),前後差標準差_trun=sd(前後差)) %>%
    mutate(ODV_L = 前後差平均_trun-k2*前後差標準差_trun,
           ODV_U = 前後差平均_trun+k2*前後差標準差_trun)
  
  return(ODV_LU)
}

#=============================================
# 前後跳動(Chebyshev)之相關函數 by Tina
#==============================================
chebyshev_range <- function(df, sinfo){
  
  p1 = 0.2
  p2 = 0.5
  
  if (!is.na(sinfo$CHE_P1)) {p1 = sinfo$CHE_P1}
  if (!is.na(sinfo$CHE_P2)) {p2 = sinfo$CHE_P2}
  
  k1 = 1/sqrt(p1)
  k2 = 1/sqrt(p2)
  
  ODV_1LU = df %>% 
    dplyr::summarise(Mean_all = mean(.data[[sinfo$VALUE_COL]]),
                     SD_all = sd(.data[[sinfo$VALUE_COL]])) %>%
    mutate(ODV_1L = Mean_all-k1*SD_all,
           ODV_1U = Mean_all+k1*SD_all)
  
  
  
  ODV_LU = df %>% 
    filter(between(.data[[sinfo$VALUE_COL]], ODV_1LU$ODV_1L, ODV_1LU$ODV_1U)) %>%
    dplyr::summarise(Mean_trun = mean(.data[[sinfo$VALUE_COL]]),
                     SD_trun = sd(.data[[sinfo$VALUE_COL]])) %>%
    mutate(ODV_L = Mean_trun-k2*SD_trun,
           ODV_U = Mean_trun+k2*SD_trun)
  
  return(ODV_LU)
}

#=============================================
# 前後跳動(Chebyshev)之相關函數 by Tina
#==============================================
na_median <- function(data, var, sinfo){
  
  #print("na_median da")
  #print(var)
  #print(year(data[[sinfo$TIME_COL]]))
  #需先將factor轉成character字串形式才能使用year()、month()等等
  #data[[as.character(sinfo$TIME_COL)]]
  #year(data[[as.character(sinfo$TIME_COL)]])
  
  da = data %>%
    select(.data[[var]]) %>%
    #mutate("ltime" = .data[[sinfo$TIME_COL]]) %>%
    #dplyr::rename(sinfo$TIME_COL="ltime") #%>%
    mutate("ltime" = data[[as.character(sinfo$TIME_COL)]],
           "氣象變數" = var,
           "year" = year(data[[as.character(sinfo$TIME_COL)]]),
           "month" = month(data[[as.character(sinfo$TIME_COL)]]),
           "是否遺漏" = ifelse(is.na(data[[var]]),1,0))
  #print(da)
  
  m = da %>%
    group_by(year = year(ltime), month = month(ltime)) %>%
    dplyr::summarise("遺漏總和" = sum(是否遺漏))
  
  #print("after na_median da")
  #每個月的中位數，整月若為NA，則不會顯現
  Median =  da %>%
    group_by(year = year(ltime), month = month(ltime)) %>%
    dplyr::summarise("median" = format(round(median(.data[[var]], na.rm = TRUE), 3),
                                       nsmall = 3))
  
  
  
  m1 = left_join(m,Median, by = c("year","month")) %>%
    select(year, month, median)
  
  
  
  #上面月份為na的使用相同月分的最後一筆資料補起來，這樣每個月都有值
  #相同月分的最後一筆資料，可用slice() n():最後一筆
  last = Median %>%
    filter(median!="NA") %>%
    group_by(month) %>%
    slice(n()) %>%
    select(month,median) %>%
    reshape::rename(c(median = "median.month"))
  
  
  #將NA補中位數
  lm1 = left_join(m1,last,by=c("month"))%>%
    mutate(median = ifelse(is.na(median), median.month, median)) %>%
    select(year, month, median)
  
  
  #原始資料na補中位數
  h = data[[var]]
  
  DATA = left_join(da, lm1, by = c("year","month")) %>%
    mutate( var = ifelse(is.na(h), median, h),
            "中位數補值" = ifelse(is.na(h), 1, 0),
            "前一筆補中位數" = lag(中位數補值, default = 中位數補值[1])) %>%
    select(氣象變數, ltime, var, 中位數補值, 前一筆補中位數) %>%
    reshape::rename(c(var=var))
  
  return(DATA)
}
