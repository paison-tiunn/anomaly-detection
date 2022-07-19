

#================================================
# 統整合理範圍上下界
# 程式碼最後更新時間:   2022/07/19, 2020/12/22
# 版本:V2_Paison, V1_Keny
#==========================================================

#=======================================================
# Source the common function we will use below
#===================================================
source("C:/Project/ReusedFunctions.R")

#=================================
# Load the required packages
#================================================
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
  library(kableExtra, warn.conflicts = FALSE)
}

if (!require('odbc', warn.conflicts = FALSE)) 
{
  install.packages('odbc',repos='https://cran.rstudio.com');
  library(odbc, warn.conflicts = FALSE);
}








#================================
# 和寫 log 相關之路徑設定
#===========================
mainDir_txt = get_mainDir(type = "txt")
mainDir_Rout = get_mainDir(type = "Rout")

#===================
# 開始寫 log
#================
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



