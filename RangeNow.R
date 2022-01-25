

#統整合理範圍上下界

# 程式碼最後更新時間:
#   2022/01/13 please see "show log"
#   2022/01/13 取消寫入[CALCUlATE_TIME] = getdate()
#   2022/01/13 新增normal_outlier_range()取代sigma3outlier_range()
#   2020/12/22

#版本: V2_Paison, V1_Keny


#自訂函數使用:
#station_name():選監測站站名中文  
#description():選監測站中文與變數最大最小值

#sigma3outlier_range():3倍標準差合理範圍上下界
#sigma3outlier_data():3倍標準差外outlier的資料與時間點 (資料為常態分配適用)
#sigma3outlier_count():依年月份算出3倍標準差外的outlier個數

#IQRoutlier_range():IQR合理範圍上下界
#IQRoutlier_data():盒狀圖1.5倍IQR外的outlier的資料與時間點 
#IQRoutlier_count():依年月份算出1.5倍IQR外的outlier個數

#chebyshev_range():chebyshev正常範圍上下界 (p1、p2決定)
#chebyshevoutlier_data():chebyshev正常範圍外outlier的資料時間點
#chebyshevoutlier_count():依年月份算出在chebyshev正常範圍外outlier個數

if (!require('plyr', warn.conflicts = FALSE)) 
{
  install.packages('plyr',repos='https://cran.rstudio.com');
  library(plyr, warn.conflicts = FALSE);
}
if (!require('tidyverse', warn.conflicts = FALSE)) 
{
  install.packages('tidyverse',repos='https://cran.rstudio.com');
  library(tidyverse, warn.conflicts = FALSE);
}

if (!require('lubridate', warn.conflicts = FALSE)) 
{
  install.packages('lubridate',repos='https://cran.rstudio.com');
  library(lubridate, warn.conflicts = FALSE);
}
if (!require('magrittr', warn.conflicts = FALSE)) 
{
  install.packages('magrittr',repos='https://cran.rstudio.com');
  library(magrittr, warn.conflicts = FALSE);
}
if (!require('scales', warn.conflicts = FALSE)) 
{
  install.packages('scales',repos='https://cran.rstudio.com');
  library(scales, warn.conflicts = FALSE);
}

#if (!require('dplyr', warn.conflicts = FALSE)) 
#{
#  install.packages('dplyr',repos='https://cran.rstudio.com');
#  library(dplyr, warn.conflicts = FALSE);
#}

if (!require('reshape', warn.conflicts = FALSE)) 
{
  install.packages('reshape',repos='https://cran.rstudio.com');
  library(reshape, warn.conflicts = FALSE);
}


Packages_D <- c("tidyverse","lubridate","magrittr","scales","dplyr")
sapply(Packages_D, library, character.only = TRUE)

if (!require('dplyr', warn.conflicts = FALSE)) 
{
  install.packages('dplyr',repos='https://cran.rstudio.com');
  library(dplyr, warn.conflicts = FALSE);
}



if (!require('data.table', warn.conflicts = FALSE)) 
{
  install.packages('data.table',repos='https://cran.rstudio.com');
  library(data.table, warn.conflicts = FALSE);#fread會用到的package
}

if (!require('ggplot2', warn.conflicts = FALSE)) 
{
  install.packages('ggplot2',repos='https://cran.rstudio.com');
  library(ggplot2, warn.conflicts = FALSE);
}

if (!require('kableExtra', warn.conflicts = FALSE)) 
{
  install.packages('kableExtra',repos='https://cran.rstudio.com');
  library(kableExtra, warn.conflicts = FALSE);#產製表格:data %>% kable() %>% kable_styling()
}


#安裝、載入RODBC套件
if (!require('odbc', warn.conflicts = FALSE)) 
{
  install.packages('odbc',repos='https://cran.rstudio.com');
  library(odbc, warn.conflicts = FALSE);
}




#================================================================================================
# normal_outlier_range(): self-defined function for outlier detection under normal distribution
#==============================================================================================
normal_outlier_range <- function(df, sinfo){
  
  #p = .001
  if (!is.na(sinfo$NORMAL_P)) {p = sinfo$NORMAL_P}; k = qnorm(1-p/2)
  
  df %<>% dplyr::summarise(lower_bound = mean(.data[[sinfo$VALUE_COL]]) - k*sd(.data[[sinfo$VALUE_COL]]),
                           upper_bound = mean(.data[[sinfo$VALUE_COL]]) + k*sd(.data[[sinfo$VALUE_COL]]))
  return(df)
}

#=============================================================================
# param_estim_gamma(): self-defined function for gamma parameter estimation
#=============================================================================
param_estim_gamma = function(x){
  
  x[which(x==0)] = min(x[which(x!=0)])/2
  s = log(mean(x)) - mean(log(x)); k = (3-s+sqrt((s-3)^2+24*s))/(12*s)
  theta = mean(x)/k
  return(c(k, 1/theta))
}

#=================================================================================
# gamma_outlier_range(): self-defined function for outlier detection under gamma
#==================================================================================
gamma_outlier_range <- function(df, sinfo){
  
  # p = .001
  #if(!is.null(sinfo)){
    if (!is.na(sinfo$GAMMA_P)) {p = sinfo$GAMMA_P}
  #}
  
  #x = .data[[sinfo$VALUE_COL]]
  var_name = sinfo$VALUE_COL; x = df[[var_name]]; negative_data = FALSE
  if(max(x, na.rm = TRUE) <= 0 & min(x, na.rm = TRUE) <= 0){
    x = -x; negative_data = TRUE
  }
  param = param_estim_gamma(x)
  
  lower = FALSE
  if(!lower){
    if(!negative_data){
      df1 = df %>%
        dplyr::summarise(lower_bound = 0,
                         upper_bound = qgamma(1-p, param[1], param[2]))
    }else{
      df1 = df %>%
        dplyr::summarise(lower_bound = -qgamma(1-p, param[1], param[2]),
                         upper_bound = 0)
    }
  }else{
    if(!negative_data){
      df1 = df %>%
        dplyr::summarise(lower_bound = qgamma(p/2, param[1], param[2]),
                         upper_bound = qgamma(1-p/2, param[1], param[2]))
    }else{
      df1 = df %>%
        dplyr::summarise(lower_bound = -qgamma(1-p/2, param[1], param[2]),
                         upper_bound = -qgamma(p/2, param[1], param[2]))
    }
  }
  return(df1)
}





### 盒狀圖1.5倍IQR外的outlier -------------------------
IQRoutlier_range <- function(df,sinfo){
  
  df1 = df %>%
    dplyr::summarise(lower_bound=quantile(.data[[sinfo$VALUE_COL]],probs = 0.25)-1.5*IQR(.data[[sinfo$VALUE_COL]]),
                     upper_bound=quantile(.data[[sinfo$VALUE_COL]],probs = 0.75)+1.5*IQR(.data[[sinfo$VALUE_COL]]))
  return(df1)
}




###chebyshev正常範圍異常值 ---------------------------------
#chebyshev正常範圍上下界
#p1:有多少比例為潛在的離群值，由p1計算k，可得truncated data的上下界(避免異常值所造成的bias)
#p2:預期會有多少比例的outlier，通常比p1小，outlier比例不超過p2
chebyshev_range <- function(df, sinfo){
  
  p1 = 0.2
  p2 = 0.5
  if (!is.na(sinfo$CHE_P1)) {
    p1 = sinfo$CHE_P1
  }
  
  if (!is.na(sinfo$CHE_P2)) {
    p2 = sinfo$CHE_P2
  }
  
  k1 = 1/sqrt(p1)
  ODV_1LU = df %>% 
    dplyr::summarise(Mean_all = mean(.data[[sinfo$VALUE_COL]]),
                     SD_all = sd(.data[[sinfo$VALUE_COL]])) %>%
    mutate(ODV_1L = Mean_all-k1*SD_all,
           ODV_1U = Mean_all+k1*SD_all)
  
  k2 = 1/sqrt(p2)
  ODV_LU = df %>% 
    filter(between(.data[[sinfo$VALUE_COL]], ODV_1LU$ODV_1L, ODV_1LU$ODV_1U)) %>%
    dplyr::summarise(Mean_trun = mean(.data[[sinfo$VALUE_COL]]),
                     SD_trun = sd(.data[[sinfo$VALUE_COL]])) %>%
    mutate(ODV_L = Mean_trun-k2*SD_trun,
           ODV_U = Mean_trun+k2*SD_trun)
  
  ODV_LU
}


#1.前後跳動過大
###na補每月中位數 ----------
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
    mutate("ltime" = data[[as.character(sinfo$TIME_COL)]],"氣象變數"=var,"year"=year(data[[as.character(sinfo$TIME_COL)]]),"month"=month(data[[as.character(sinfo$TIME_COL)]]),
           "是否遺漏"=ifelse(is.na(data[[var]]),1,0))
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
  
  DATA
  
  
}




###補值完算前後差異，使用chebyshev找出跳動最大值 -----------
#前後差可容忍跳動最大值
# p1目前取0.1還不錯，p2取0.005
chebyshev_jumprange_na <- function(df, sinfo, p1 = 0.1, p2){
  

  
  ###var需變成字串
  var = as.character(sinfo$VALUE_COL)
  #var = as.character(sinfo$SENSOR_NAME)
  
  #print("before na_median")
  data0=na_median(df,var,sinfo)
  #print("data0")
  #print(data0)
  
  #可能讀進來為字串，需轉數字型態
  data0[[var]] <- as.numeric(data0[[var]])
  
  data=data0 %>%
    mutate("前時間" = lag(ltime,default=ltime[1]),
           "前面一個" = lag(data0[[var]],default=data0[[var]][1]),
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
  
  
  ODV_LU
}

###在chebyshev前後差可容忍跳動最大值外outlier的資料時間點 ------------
chebyshev_jumpdata_na <- function(df, sinfo, var){
  
  # p21 = 0.1  #代號p21統一成p1
  # p22 = 0.05 #代號p22統一成p2

    p1 = 0.1; p2 = 0.05
  
  if (!is.na(sinfo$JUMP_P1)) {p1 = sinfo$JUMP_P1}
  if (!is.na(sinfo$JUMP_P2)) {p2 = sinfo$JUMP_P2}
  
  #ODV_LU=chebyshev_jumprange_na(df,sinfo,p21,p22)  #修改統一代號p21->p1，p22->p2
  ODV_LU = chebyshev_jumprange_na(df, sinfo, p1, p2)
  
  ODV_LU
  
  
  
}

#temp_outlier = chebyshev_jumpdata_na(data.list.WL[[1]],"Temp",0.1,0.005)


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

#===================================================
# writeLog(): self-defined function for 寫log
#================================================
writeLog <- function(msg, mainDir){
  
  # 寫入檔案
  fileConn <- file(mainDir)
  logMsg <- paste(format(Sys.time(), "%F %R :"), msg, sep = " ")
  write(logMsg, file = mainDir, append = TRUE)
  close(fileConn)
}

#設定資料庫連線
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


#組合查詢字串
getSensorSQL <- function(tbName,time_Col,value_col,sensorID,sid_Col,sdate,edate){
  sqlStr <- paste("SELECT ",sid_Col,",",value_col,",",time_Col," FROM ", sep="")
  #sqlStr <- "SELECT * FROM "
  sqlStr <- paste(sqlStr, tbName , " where ",sid_Col," = '",sensorID,"' and  ",time_Col," between '",sdate,"' and '", edate,"'", sep="" )
  
  sqlStr
}

#=========================================================================================
# changeDetect(): self-defined function for change point detection used in calResult()
#========================================================================================
changeDetect = function(data, sensorInfo){
  DATA = data
  data = DATA[[sensorInfo$VALUE_COL]]; message(typeof(data)) #message(paste0("is.vector(data): ", is.vector(data)))
  time = DATA[[sensorInfo$TIME_COL]]; message(typeof(time)) #message(paste0("is.vector(time): ", is.vector(time)))
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
        
        c1 = data[1:cut]; c2 = data[cut+1:n]
        r1 = max(c1, na.rm = TRUE)-min(c1, na.rm = TRUE); #message(paste0("left range ( ~ ", time[cut],"): ", r1))
        r2 = max(c2, na.rm = TRUE)-min(c2, na.rm = TRUE); #message(paste0("right range (", time[cut+1]," ~ ): ", r2))
        if(r1 > r2){max = cut}else{min = cut}
        message(paste0("(max-min)==", max-min))
        
        # check whether to break the while(TRUE) loop
        if((max-min)==1){
          c1 = data[1:min]; c2 = data[min+1:n]
          r1 = max(c1, na.rm = TRUE)-min(c1, na.rm = TRUE)
          r2 = max(c2, na.rm = TRUE)-min(c2, na.rm = TRUE)
          r_min = abs(r1-r2)
          c1 = data[1:max]; c2 = data[max+1:n]
          r1 = max(c1, na.rm = TRUE)-min(c1, na.rm = TRUE)
          r2 = max(c2, na.rm = TRUE)-min(c2, na.rm = TRUE)
          r_max = abs(r1-r2)
          if(r_min < r_max){cut = min}else{cut = max}
          message("\nBreak the while(TRUE) loop!"); break
        }
      }
    }else{message(paste0("length(data)==", length(data)))}
    
    message("==> CPD results:")
    
    data1 = data[1:cut]; data2 = data[cut+1:n]
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
      data = data[cut+1:n]; time = time[cut+1:n]; CHANGE_TIME = time[1]
      #message(paste(source, "has a change point:", cut, "\n"))
      message(paste0("Detected change point: ", CHANGE_TIME, "\n"))
    }else{
      message("Break the while(stop==0) loop!")
    }
  } # end of the while(stop==0) loop
  
  time = time[cut+1:n]; CANDIDATE_TIME = time[1]
  message(paste0("Possible/suspicious/candidate change point: ", CANDIDATE_TIME))
  DATA %<>% filter(.data[[sensorInfo$TIME_COL]] >= CHANGE_TIME)
  return(list("DATA" = DATA, "CPD" = CPD, "CHANGE_TIME" = CHANGE_TIME))
}

#計算結果
#calResult <- function(data,col,sn){
calResult <- function(data, sensorInfo){  
  
  message("===Start CPD===")
  CPD_result = changeDetect(data, sensorInfo)
  message("===Finish CPD===")
  
  data = CPD_result$DATA
  changeFound = CPD_result$CPD
  if(changeFound==0){CHANGE_TIME = NULL}else{CHANGE_TIME = CPD_result$CHANGE_TIME}
  
  SENSOR_UP = sensorInfo$SENSOR_UP
  SENSOR_DOWN = sensorInfo$SENSOR_DOWN
  
  if (!is.na(SENSOR_DOWN)) {min = SENSOR_DOWN}
  if (!is.na(SENSOR_UP)) {max = SENSOR_UP}
  data %<>% filter(between(.data[[sensorInfo$VALUE_COL]], min, max))
  
  
  #CHECK_LIST = sensorInfo$CHECK_LIST
  
  if(SENSOR_UP <= 0 | SENSOR_DOWN >= 0){
    message("Start G")
    gammaResult = gamma_outlier_range(data, sensorInfo)
    GAMMA_UP = gammaResult$upper_bound; GAMMA_DOWN = gammaResult$lower_bound
    if(is.numeric(GAMMA_UP)){GAMMA_UP <- round(GAMMA_UP , 3)}
    if(is.numeric(GAMMA_DOWN)){GAMMA_DOWN <- round(GAMMA_DOWN , 3)}
    message("Finish G")
  }else{
    GAMMA_UP = GAMMA_DOWN = "NULL"; message("G not run")
  }
  
  #if(str_detect(CHECK_LIST, "9")){}
  
  normalResult <- normal_outlier_range(data, sensorInfo)
  IQRResult <-  IQRoutlier_range(data,sensorInfo)
  chebyshevResult <- chebyshev_range(data,sensorInfo)
  
  
  temp_outlier = chebyshev_jumpdata_na(data,sensorInfo,sensorInfo$VALUE_COL)
  #print(temp_outlier$可容忍跳動最大值[1])
  #writeLog(temp_outlier$可容忍跳動最大值[1])
  
  CHE_UP <- chebyshevResult$ODV_U; CHE_DOWN <- chebyshevResult$ODV_L
  BOX_UP <- IQRResult$upper_bound; BOX_DOWN <- IQRResult$lower_bound
  
  #NORMAL_UP <- sigma3Result$upper_bound; NORMAL_DOWN <- sigma3Result$lower_bound
  NORMAL_UP <- normalResult$upper_bound; NORMAL_DOWN <- normalResult$lower_bound
  
  
  #JUMP_VALUE <- temp_outlier$可容忍跳動最大值[1]
  JUMP_VALUE <- temp_outlier$ODV_U
  
  if(is.numeric(CHE_UP)){CHE_UP <- round(CHE_UP , 3)}
  if(is.numeric(CHE_DOWN)){CHE_DOWN <- round(CHE_DOWN , 3)}
  
  if(is.numeric(BOX_UP)){BOX_UP <- round(BOX_UP , 3)}
  if(is.numeric(BOX_DOWN)){BOX_DOWN <- round(BOX_DOWN , 3)}
  
  if(is.numeric(NORMAL_UP)){NORMAL_UP <- round(NORMAL_UP , 3)}
  if(is.numeric(NORMAL_DOWN)){NORMAL_DOWN <- round(NORMAL_DOWN , 3)}
  
  if(is.numeric(JUMP_VALUE)){
    JUMP_VALUE <- round(JUMP_VALUE , 3)
  }
  
  if(is.na(JUMP_VALUE)){
    JUMP_VALUE <- 0
  }
  
  if(is.null(CHANGE_TIME)){CHANGE_TIME = "NULL"}else{CHANGE_TIME = paste0("'",CHANGE_TIME,"'")}
  
  JUMP_VALUE_GAMMA = "NULL"
    
    
  # 資料更新回資料庫
  sqlr_Update <- "UPDATE [dbo].[Sensor_Info] set "
  sqlr_Update <- paste(sqlr_Update, "[CHE_UP] = " ,CHE_UP,", [CHE_DOWN] = ",CHE_DOWN,", ")
  sqlr_Update <- paste(sqlr_Update, "[BOX_UP] = " ,BOX_UP,", [BOX_DOWN] = ",BOX_DOWN,", ")
  sqlr_Update <- paste(sqlr_Update, "[NORMAL_UP] = " , NORMAL_UP,", [NORMAL_DOWN] = ", NORMAL_DOWN,", ")
  sqlr_Update <- paste(sqlr_Update, "[GAMMA_UP] = " , GAMMA_UP,", [GAMMA_DOWN] = ", GAMMA_DOWN,", ")
  sqlr_Update <- paste(sqlr_Update, "[JUMP_VALUE] = " ,JUMP_VALUE,", ")
  sqlr_Update <- paste(sqlr_Update, "[CHANGE_TIME] = ", CHANGE_TIME)
  sqlr_Update <- paste(sqlr_Update, " where SN = ", sensorInfo$SN)
  print(sqlr_Update)
  dbGetQuery(basicConn, sqlr_Update)
  
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
  print(sqlr_Insert)
  dbGetQuery(basicConn, sqlr_Insert)
}



#步驟
#1.先取出資料集
#2.算出最大最小值
#3.計算結果
#4.儲存結果


mainDir_txt = get_mainDir(type = "txt")
mainDir_Rout = get_mainDir(type = "Rout")
writeLog("Start RangeNow Process", mainDir_txt)

errorCatch <- file(mainDir_Rout, open = "wt")
sink(errorCatch, type = "message")








#資料庫連線
basicConn <- dbConnect(odbc(),
                       Driver = "{SQL Server Native Client 11.0}",
                       Server = "59.120.223.165",
                       Database = "SensorWebAD",
                       UID = "idmm",
                       PWD = "wj/3ck6tj4",
                       Port = 1433)

#取得需要做計算的儀器
realtimeQuery = "select top 1 [SN],[IP],[DB_NAME],[USERNAME],[PASSWORD],"
realtimeQuery = paste0(realtimeQuery, "[TABLE_NAME],[TIME_COL],[VALUE_COL],[SENSOR_ID],[SENSOR_ID_COL],")
realtimeQuery = paste0(realtimeQuery, "[DATA_RANGE],[SDATE],[EDATE],[SENSOR_UP],[SENSOR_DOWN],[CHE_P1],[CHE_P2],")
realtimeQuery = paste0(realtimeQuery, "[JUMP_P1],[JUMP_P2],[NORMAL_P],[GAMMA_P], [JUMP_P_GAMMA] ")
realtimeQuery = paste0(realtimeQuery, "from V_Sensor_Info where AUTO_UPDATE = 1 and update_time > getdate()-0.01 order by update_time desc")
querySensor <- dbSendQuery(basicConn, realtimeQuery)
# select * from V_Sensor_Info where update_time > getdate()-update_FQ

SensorInfoList <- dbFetch(querySensor)
dbClearResult(querySensor)
print(nrow(SensorInfoList))
#print(length(SensorInfoList))


for (idx in 1:nrow(SensorInfoList)) {
  #print(SensorInfoList[2])

  SensorConnect <- setDBConnect(SensorInfoList$IP[idx],
                                SensorInfoList$DB_NAME[idx],
                                SensorInfoList$USERNAME[idx],
                                SensorInfoList$PASSWORD[idx])
  queryStr <- getSensorSQL(SensorInfoList$TABLE_NAME[idx],
                           SensorInfoList$TIME_COL[idx],
                           SensorInfoList$VALUE_COL[idx],
                           SensorInfoList$SENSOR_ID[idx],
                           SensorInfoList$SENSOR_ID_COL[idx],
                           SensorInfoList$SDATE[idx],
                           SensorInfoList$EDATE[idx])
  print(queryStr)
  print(SensorInfoList$SENSOR_ID[idx])
  print(SensorInfoList$SENSOR_ID_COL[idx])
  query <- dbSendQuery(SensorConnect, queryStr)
  data <- dbFetch(query)
  #print(nrow(data))
  dbClearResult(query)
  #calResult(data,SensorInfoList$COLUMN_NAME[idx],SensorInfoList$SN[idx])
  #aa = SensorInfoList[idx,]
  #print("aa is ")
  #print(aa$TABLE_NAME)
  
  if(dim(data)[1]==0){message(paste0("No data: [TABLE_NAME] = ", SensorInfoList$TABLE_NAME[idx], ", ",
                                  "[VALUE_COL] = ", SensorInfoList$VALUE_COL[idx], ", ",
                                  "[SDATE] = ", SensorInfoList$SDATE[idx], ", ",
                                  "[EDATE] = ", SensorInfoList$EDATE[idx]))
  }else{
    calResult(data, SensorInfoList[idx,])
  }
  
  #print(queryStr)
}

writeLog("Finish RangeNow Process", mainDir_txt)
sink(type = "message")
close(errorCatch)






dbDisconnect(basicConn)
dbDisconnect(SensorConnect)








