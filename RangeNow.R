

# 統整合理範圍上下界

# 程式碼最後更新時間:
#   2022/01/13 please see "show log"
#   2022/01/13 取消寫入[CALCUlATE_TIME] = getdate()
#   2022/01/13 新增normal_outlier_range()取代sigma3outlier_range()
#   2020/12/22

#版本: V2_Paison, V1_Keny



source("C:/Project/ReusedFunctions.R")











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


Packages_D <- c("tidyverse","lubridate","magrittr","scales","dplyr","purrr")
sapply(Packages_D, library, character.only = TRUE)

if (!require('dplyr', warn.conflicts = FALSE)) 
{
  install.packages('dplyr',repos='https://cran.rstudio.com');
  library(dplyr, warn.conflicts = FALSE);
}



if (!require('data.table', warn.conflicts = FALSE)) 
{
  install.packages('data.table',repos='https://cran.rstudio.com');
  library(data.table, warn.conflicts = FALSE)
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







#=================
# 資料庫連線
#====================================
basicConn <- dbConnect(odbc(),
                       Driver = "{SQL Server Native Client 11.0}",
                       Server = "59.120.223.165",
                       Database = "SensorWebAD",
                       UID = "idmm",
                       PWD = "wj/3ck6tj4",
                       Port = 1433)

#==========================================
# 取得需要做計算的儀器
#========================================================
realtimeQuery = "select top 1 [SN],[IP],[DB_NAME],[USERNAME],[PASSWORD],"
realtimeQuery = paste0(realtimeQuery, "[TABLE_NAME],[TIME_COL],[VALUE_COL],[SENSOR_ID],[SENSOR_ID_COL],")
realtimeQuery = paste0(realtimeQuery, "[DATA_RANGE],[SDATE],[EDATE],[SENSOR_UP],[SENSOR_DOWN],[CHE_P1],[CHE_P2],")
realtimeQuery = paste0(realtimeQuery, "[JUMP_P1],[JUMP_P2],[NORMAL_P],[GAMMA_P],[JUMP_P_GAMMA],[VALUE_FEATURE],[MODE] ")
realtimeQuery = paste0(realtimeQuery, "from V_Sensor_Info where [AUTO_UPDATE] = 1 and [UPDATE_TIME] > getdate()-0.01 order by [UPDATE_TIME] desc")
querySensor <- dbSendQuery(basicConn, realtimeQuery)





SensorInfoList <- dbFetch(querySensor)
dbClearResult(querySensor)


for (idx in 1:nrow(SensorInfoList)) {

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
  
  query <- dbSendQuery(SensorConnect, queryStr)
  
  data <- dbFetch(query)
  dbClearResult(query)
  
  if(dim(data)[1]==0){message(paste0("No data: [TABLE_NAME] = ", SensorInfoList$TABLE_NAME[idx], ", ",
                                  "[VALUE_COL] = ", SensorInfoList$VALUE_COL[idx], ", ",
                                  "[SDATE] = ", SensorInfoList$SDATE[idx], ", ",
                                  "[EDATE] = ", SensorInfoList$EDATE[idx]))
  }else{
    calResult(data, SensorInfoList[idx,])
  }
}

writeLog("Finish RangeNow Process", mainDir_txt)
sink(type = "message")
close(errorCatch)






dbDisconnect(basicConn)
dbDisconnect(SensorConnect)








