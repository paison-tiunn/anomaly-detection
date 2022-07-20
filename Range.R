




#=====================================================================================
# Range.R       會在排程檢查確認需要自動更新時被觸發
# RangeNow.R    會在使用者點選 [儲存並立即更新] 時被觸發
# RangeNowALL.R 不會被自動觸發, 為內部想要更新特定站別時搭配 RangeNowALL_SQL.txt 使用 
#=======================================================================================



#============================================================
# Current developer and maintainer: Paison (2021/11-2022/7)
# Former developers: Keny & Tina (-2021/11)
#=======================================================






#=======================================================
# Source the common function we will use below
#===================================================
source("C:/Project/ReusedFunctions.R")





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


#=================================
# Load the required packages
#================================================

RequiredPackages = c("lubridate","magrittr","scales","reshape","purrr","dplyr","ggplot2","odbc","data.table","kableExtra")

for(i in RequiredPackages){
  if(!require(i, warn.conflicts = FALSE, character.only = TRUE)){
    install.packages(i, repos = "https://cran.rstudio.com")
    library(i, warn.conflicts = FALSE, character.only = TRUE)
  }
}







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



