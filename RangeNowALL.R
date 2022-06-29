

#統整合理範圍上下界

# 程式碼最後更新時間:
#   2022/01/13 please see "show log"
#   2022/01/13 取消寫入[CALCUlATE_TIME] = getdate()
#   2022/01/13 新增normal_outlier_range()取代sigma3outlier_range()
#   2020/12/22

#版本: V2_Paison, V1_Keny

source("C:/Project/ReusedFunctions.R")

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





mainDir_txt = get_mainDir(type = "txt")
writeLog("Start RangeNowALL Process", mainDir_txt)






mainDir_Rout = get_mainDir(type = "Rout")
errorCatch <- file(mainDir_Rout, open = "wt")
sink(errorCatch, type = "message")


#=====================
# 資料庫連線
#=================================
basicConn <- dbConnect(odbc(),
                       Driver = "{SQL Server Native Client 11.0}",
                       Server = "59.120.223.165",
                       Database = "SensorWebAD",
                       UID = "idmm",
                       PWD = "wj/3ck6tj4",
                       Port = 1433)

allQuery0 = readr::read_file("C:/Project/RangeNowALL_SQL.txt")


#===========================
# 取得需要做計算的儀器
#========================
allQuery = "SELECT [SN],[IP],[DB_NAME],[USERNAME],[PASSWORD],[TABLE_NAME],[TIME_COL],[VALUE_COL],[SENSOR_ID],"
allQuery = paste0(allQuery, "[SENSOR_ID_COL],[DATA_RANGE],[SDATE],[EDATE],[SENSOR_DOWN],[SENSOR_UP],[CHE_P1],[CHE_P2],")
allQuery = paste0(allQuery, "[JUMP_P1],[JUMP_P2],[NORMAL_P],[GAMMA_P],[JUMP_P_GAMMA],[VALUE_FEATURE],[MODE] ")
allQuery = paste0(allQuery, "FROM V_Sensor_Info WHERE [AUTO_UPDATE] = 1")
allQuery = enc2native(paste(allQuery, allQuery0, sep = " AND "))

querySensor <- dbSendQuery(basicConn, allQuery)

# select * from V_Sensor_Info where update_time > getdate()-update_FQ

SensorInfoList <- dbFetch(querySensor)
dbClearResult(querySensor)
print(nrow(SensorInfoList))
#print(length(SensorInfoList))

sink(type = "message")
close(errorCatch)
Sys.sleep(1)

for (idx in 1:nrow(SensorInfoList)) {
  
  mainDir_Rout = get_mainDir(type = "Rout")
  errorCatch <- file(mainDir_Rout, open = "wt")
  sink(errorCatch, type = "message")
  
  tryCatch({
    
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
    dbClearResult(query)
    
    
    if(dim(data)[1]==0){writeLog(paste0("No data: [TABLE_NAME] = ", SensorInfoList$TABLE_NAME[idx], ", ",
                                       "[VALUE_COL] = ", SensorInfoList$VALUE_COL[idx], ", ",
                                       "[SDATE] = ", SensorInfoList$SDATE[idx], ", ",
                                       "[EDATE] = ", SensorInfoList$EDATE[idx], ", ",
                                       "[SN] = ", SensorInfoList$SN[idx]), mainDir_txt)
    }else{
      calResult(data, SensorInfoList[idx,])
    }
    
    
  }, error = function(e){
    errorLog = paste0("There's something wrong with [SN] = ", SensorInfoList$SN[idx], " in [SensorWebAD].[dbo].[Sensor_Info]. ")
    errorLog = paste0(errorLog, "Please see ", mainDir_Rout, " for details: ", e)
    writeLog(errorLog, mainDir_txt)
    Sys.sleep(1)
    })
  
  sink(type = "message")
  close(errorCatch)
  
}

writeLog("Finish RangeNowALL Process", mainDir_txt)







dbDisconnect(basicConn)
dbDisconnect(SensorConnect)








