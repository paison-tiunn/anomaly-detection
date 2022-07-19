#統整合理範圍上下界
#程式碼最後更新時間:   2020/12/22
#版本:V1_Keny


# 自訂函數使用:
# station_name():選監測站站名中文  
# description():選監測站中文與變數最大最小值

source("C:/Project/ReusedFunctions.R")

# IQRoutlier_range():IQR合理範圍上下界
# IQRoutlier_data():盒狀圖1.5倍IQR外的outlier的資料與時間點 
# IQRoutlier_count():依年月份算出1.5倍IQR外的outlier個數

# chebyshev_range():chebyshev正常範圍上下界 (p1、p2決定)
# chebyshevoutlier_data():chebyshev正常範圍外outlier的資料時間點
# chebyshevoutlier_count():依年月份算出在chebyshev正常範圍外outlier個數

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
  
  min=0#description(df,variable)$min
  max=9999#description(df,variable)$max
  #Station=description(df,variable)$Station
  if (!is.na(sinfo$SENSOR_DOWN)) {
    min = sinfo$SENSOR_DOWN
  }
  
  if (!is.na(sinfo$SENSOR_UP)) {
    max = sinfo$SENSOR_UP
  }
  
  df1 =  df %>%
    #mutate(year=year(LocalTime),month=month(LocalTime),監測站站名=Station) %>%
    filter(between(.data[[sinfo$VALUE_COL]],min,max) ) %>%
    dplyr::summarise(lower_bound=quantile(.data[[sinfo$VALUE_COL]],probs = 0.25)-1.5*IQR(.data[[sinfo$VALUE_COL]]),
                     upper_bound=quantile(.data[[sinfo$VALUE_COL]],probs = 0.75)+1.5*IQR(.data[[sinfo$VALUE_COL]])) #%>% 
    #mutate("監測站站名"=Station,"變數"=variable)
  
  df1
  
}




###chebyshev正常範圍異常值 ---------------------------------
#chebyshev正常範圍上下界
#p1:有多少比例為潛在的離群值，由p1計算k，可得truncated data的上下界(避免異常值所造成的bias)
#p2:預期會有多少比例的outlier，通常比p1小，outlier比例不超過p2
chebyshev_range <-function(df,sinfo){
  
  min=0 #description(df,var)$min
  max=9999 #description(df,var)$max
  #Station=description(df,var)$Station
  if (!is.na(sinfo$SENSOR_DOWN)) {
    min = sinfo$SENSOR_DOWN
  }
  
  if (!is.na(sinfo$SENSOR_UP)) {
    max = sinfo$SENSOR_UP
  }
  p1 = 0.2
  p2 = 0.5
  if (!is.na(sinfo$CHE_P1)) {
    p1 = sinfo$CHE_P1
  }
  
  if (!is.na(sinfo$CHE_P2)) {
    p2 = sinfo$CHE_P2
  }
  
  k1=1/sqrt(p1)
  ODV_1LU=df %>% 
    filter(between(.data[[sinfo$VALUE_COL]],min,max)) %>%
    dplyr::summarise(Mean_all=mean(.data[[sinfo$VALUE_COL]]),SD_all=sd(.data[[sinfo$VALUE_COL]])) %>%
    mutate(ODV_1L=Mean_all-k1*SD_all,ODV_1U=Mean_all+k1*SD_all)
  
  k2=1/sqrt(p2)
  ODV_LU=df %>% 
    filter(between(.data[[sinfo$VALUE_COL]],ODV_1LU$ODV_1L,ODV_1LU$ODV_1U)) %>%
    dplyr::summarise(Mean_trun=mean(.data[[sinfo$VALUE_COL]]),SD_trun=sd(.data[[sinfo$VALUE_COL]])) %>%
    mutate(ODV_L=Mean_trun-k2*SD_trun,ODV_U=Mean_trun+k2*SD_trun)
  
  ODV_LU
}


#1.前後跳動過大
###na補每月中位數 ----------
na_median <- function(data,var,sinfo){
  
  min=0#description(data,var)$min
  max=9999#description(data,var)$max
  #Station=description(data,var)$Station
  if (!is.na(sinfo$SENSOR_DOWN)) {
    min = sinfo$SENSOR_DOWN
  }
  
  if (!is.na(sinfo$SENSOR_UP)) {
    max = sinfo$SENSOR_UP
  }

  #print("na_median da")
  #print(var)
  #print(year(data[[sinfo$TIME_COL]]))
  
  da= data %>%
    select(.data[[var]]) %>%
    #mutate("ltime" = .data[[sinfo$TIME_COL]]) %>%
    #dplyr::rename(sinfo$TIME_COL="ltime") #%>% 
    mutate("ltime" = data[[sinfo$TIME_COL]],"氣象變數"=var,"year"=year(data[[sinfo$TIME_COL]]),"month"=month(data[[sinfo$TIME_COL]]),
           "是否遺漏"=ifelse(is.na(data[[var]]),1,0)) 
  #print(da)

  m= da %>%
    group_by(year=year(ltime),month=month(ltime)) %>%
    dplyr::summarise("遺漏總和"=sum(是否遺漏)) 
  
  #print("after na_median da")
  #每個月的中位數，整月若為NA，則不會顯現
  Median=  da %>%
    filter(between(.data[[var]],min,max)) %>%
    group_by(year=year(ltime),month=month(ltime)) %>%
    dplyr::summarise("median"=median(.data[[var]],na.rm = TRUE))
  
  
  
  
  m1=left_join(m,Median,by=c("year","month")) %>% 
    select(year,month,median)
  
  
  #上面月份為na的使用相同月分的最後一筆資料補起來，這樣每個月都有值
  #相同月分的最後一筆資料，可用slice() n():最後一筆
  last= Median %>%
    filter(median!="NA") %>%
    group_by(month) %>%
    slice(n()) %>%
    select(month,median) %>%
    reshape::rename(c(median="median.month"))
  
  
  #將NA補中位數
  lm1=left_join(m1,last,by=c("month"))%>%
    mutate(median = ifelse(is.na(median),median.month,median)) %>%
    select(year,month,median)
  
  
  #原始資料na補中位數
  h=data[[var]]
  
  DATA= left_join(da,lm1,by=c("year","month")) %>%
    mutate( var = ifelse(is.na(h),median,h),"中位數補值"=ifelse(is.na(h),1,0),
            "前一筆補中位數"=lag(中位數補值,default=中位數補值[1])) %>%
    select(氣象變數,ltime,var,中位數補值,前一筆補中位數) %>%
    reshape::rename(c(var=var))
  
  DATA
}


###補值完算前後差異，使用chebyshev找出跳動最大值 -----------
#前後差可容忍跳動最大值
# p1目前取0.1還不錯，p2取0.005
chebyshev_jumprange_na <-function(df,sinfo,p1=0.1,p2){
  
  #print("chebyshev_jumprange_na")
  min=0 #description(df,var)$min
  max=9999 #description(df,var)$max
  #Station=description(df,var)$Station
  if (!is.na(sinfo$SENSOR_DOWN)) {
    min = sinfo$SENSOR_DOWN
  }
  
  if (!is.na(sinfo$SENSOR_UP)) {
    max = sinfo$SENSOR_UP
  }
  
  var = sinfo$VALUE_COL
  #print("before na_median")
  data0=na_median(df,var,sinfo)
  #print("data0")
  #print(data0)
  
  data=data0 %>%
    mutate("前時間"=lag(ltime,default=ltime[1]),
           "前面一個"=lag(data0[[var]],default=data0[[var]][1]),
           "前後差" = abs(前面一個 - data0[[var]])
           ) %>%
    filter(between(data0[[var]],min,max) )

  
  k1=1/sqrt(p1) 
  ODV_1LU=data  %>%
    dplyr::summarise(前後差平均_all=mean(前後差),前後差標準差_all=sd(前後差)) %>%
    mutate(ODV_1L=前後差平均_all-k1*前後差標準差_all,ODV_1U=前後差平均_all+k1*前後差標準差_all)
  
  k2=1/sqrt(p2)
  ODV_LU=data %>%
    filter(between(前後差,ODV_1LU$ODV_1L,ODV_1LU$ODV_1U)) %>%
    dplyr::summarise(前後差平均_trun=mean(前後差),前後差標準差_trun=sd(前後差)) %>%
    mutate(ODV_L=前後差平均_trun-k2*前後差標準差_trun,ODV_U=前後差平均_trun+k2*前後差標準差_trun)
  
  
  ODV_LU
}

###在chebyshev前後差可容忍跳動最大值外outlier的資料時間點 ------------
chebyshev_jumpdata_na <- function(df,sinfo,var){
  min=0 #description(df,var)$min
  max=9999 #description(df,var)$max
  #Station=description(df,var)$Station
  if (!is.na(sinfo$SENSOR_DOWN)) {
    min = sinfo$SENSOR_DOWN
  }
  
  if (!is.na(sinfo$SENSOR_UP)) {
    max = sinfo$SENSOR_UP
  }
  
  p21 = 0.1
  p22 = 0.05
  if (!is.na(sinfo$JUMP_P1)) {
    p1 = sinfo$JUMP_P1
  }
  
  if (!is.na(sinfo$JUMP_P2)) {
    p2 = sinfo$JUMP_P2
  }
  
  ODV_LU=chebyshev_jumprange_na(df,sinfo,p21,p22)
  ODV_LU
  
  #data0=na_median(df,var,sinfo)
  
  ##print("da")
  
  ##可能讀進來為字串，需轉數字型態
  #data0[[var]] <- as.numeric(data0[[var]])
  
  #da=data0 %>%
  #  mutate("前時間"=lag(ltime,default=ltime[1]),
  #         "前面一個"=lag(data0[[var]],default=data0[[var]][1]),
  #         "前後差" = abs(前面一個 - data0[[var]]),
  #         #"變動下限"=前面一個-ODV_LU$ODV_U,
  #         #"變動上限"=前面一個+ODV_LU$ODV_U,
  #         "可容忍跳動最大值"=ODV_LU$ODV_U,
  #         #"chebyshev正常範圍下界"=LU$chebyshev下界,
  #         #"chebyshev正常範圍上界"=LU$chebyshev上界,
  #         "chebyshev跳動異常值"=1) %>%
  #  filter(between(data0[[var]],min,max))
  #da2=reshape::rename(da,c(chebyshev跳動異常值=paste(var,"_chebyshev跳動異常值",sep="")))
  
  ##& !between(da[[var]],LU$chebyshev下界,LU$chebyshev上界)排除落於chebyshev正常範圍內
  #outlier=da2 %>%
  #  filter( 前後差 > ODV_LU$ODV_U & 中位數補值!=1 & 前一筆補中位數!=1)
  #outlier
  
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


# #=======================
# # 組合查詢字串(舊的)
# #==============================
# getSensorSQL <- function(tbName, time_Col, value_col, sid_Col, sensorID, period){
#   sqlStr <- paste0("SELECT ",sid_Col,",",value_col,",",time_Col," FROM ")
#   #sqlStr <- "SELECT * FROM "
#   sqlStr <- paste(sqlStr, tbName , " where ",sid_Col," = '",sensorID,"' and  ",time_Col," > getdate()-", period, sep="" )
#   
#   sqlStr
# }





mainDir_txt = get_mainDir(type = "txt")
mainDir_Rout = get_mainDir(type = "Rout")
writeLog("Start RangeNow Process", mainDir_txt)

errorCatch <- file(mainDir_Rout, open = "wt")
sink(errorCatch, type = "message")

#======================================
# 資料庫連線
#============================================================
basicConn <- dbConnect(odbc(),
                 Driver = "{SQL Server Native Client 11.0}",
                 Server = "59.120.223.165",
                 Database = "SensorWebAD",
                 UID = "idmm",
                 PWD = "wj/3ck6tj4",
                 Port = 1433)

#=================================
# 取得需要做計算的儀器
#====================================================
dailycheckQuery = "select * from V_Sensor_Info where [AUTO_UPDATE] = 1 and [UPDATE_FQ] <> 0 and ([TIMESTAMP] + [UPDATE_FQ]) < GETDATE()"
querySensor <- dbSendQuery(basicConn, dailycheckQuery)
SensorInfoList <- dbFetch(querySensor)
dbClearResult(querySensor)

if(nrow(SensorInfoList) > 0){
  
  for (idx in 1:nrow(SensorInfoList)) {
    
    SensorConnect <- setDBConnect(SensorInfoList$IP[idx],
                                  SensorInfoList$DB_NAME[idx],
                                  SensorInfoList$USERNAME[idx],
                                  SensorInfoList$PASSWORD[idx])
    # queryStr <- getSensorSQL(SensorInfoList$TABLE_NAME[idx],
    #                          SensorInfoList$TIME_COL[idx],
    #                          SensorInfoList$VALUE_COL[idx],
    #                          SensorInfoList$SENSOR_ID_COL[idx],
    #                          SensorInfoList$SENSOR_ID[idx],
    #                          SensorInfoList$DATA_RANGE[idx])
    
    queryStr <- getSensorSQL(SensorInfoList$TABLE_NAME[idx],
                             SensorInfoList$TIME_COL[idx],
                             SensorInfoList$VALUE_COL[idx],
                             SensorInfoList$SENSOR_ID[idx],
                             SensorInfoList$SENSOR_ID_COL[idx],
                             as.Date(SensorInfoList$SDATE[idx]) + SensorInfoList$UPDATE_FQ[idx],
                             as.Date(SensorInfoList$EDATE[idx]) + SensorInfoList$UPDATE_FQ[idx])
    
    query <- dbSendQuery(SensorConnect, queryStr)
    data <- dbFetch(query)
    
    dbClearResult(query)
    
    calResult(data, SensorInfoList[idx,], AutoTriggered = TRUE)
    
    #writeLog(queryStr, mainDir_txt)
    
  }
}else{
  writeLog("No data need to be updated for now.", mainDir_txt)
  message("No data need to be updated for now.")
}



writeLog("Finish Range Process", mainDir_txt)
sink(type = "message")
close(errorCatch)

dbDisconnect(basicConn)
dbDisconnect(SensorConnect)



