







#=====================================================================================
# Range.R       會在排程檢查確認需要自動更新時被觸發
# RangeNow.R    會在使用者點選 [儲存並立即更新] 時被觸發
# RangeNowALL.R 不會被自動觸發, 為內部想要更新特定站別時搭配 RangeNowALL_SQL.txt 使用 
#=======================================================================================



#============================================================
# Current developer and maintainer: Paison (2021/11-2022/7)
# Former developers: Keny & Tina (-2021/11)
#=======================================================

#==================================================
# 讀取相關自訂函數
#============================================

source("C:/Project/ReusedFunctions.R")



#===================================================
# 讀取套件
#===============================================

RequiredPackages = c("lubridate","magrittr","scales","reshape","purrr","dplyr","ggplot2","odbc","data.table","kableExtra")

for(i in RequiredPackages){
  if(!require(i, warn.conflicts = FALSE)){
    install.packages(i, repos = "https://cran.rstudio.com")
    library(i, warn.conflicts = FALSE)
  }
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








