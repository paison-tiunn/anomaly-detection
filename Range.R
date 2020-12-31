#統整合理範圍上下界
#程式碼最後更新時間:   2020/12/22
#版本:V1_Keny


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

Packages_D <- c("tidyverse","lubridate","magrittr","scales","dplyr")
sapply(Packages_D, library, character.only = TRUE)

if (!require('dplyr', warn.conflicts = FALSE)) 
{
  install.packages('dplyr',repos='https://cran.rstudio.com');
  library(dplyr, warn.conflicts = FALSE);
}


library(plyr)
library(data.table)   #fread會用到的package
library(ggplot2)
library(kableExtra)   #產製表格:data %>% kable() %>% kable_styling()

#安裝、載入RODBC套件
if (!require('odbc', warn.conflicts = FALSE)) 
{
  install.packages('odbc',repos='https://cran.rstudio.com');
  library(odbc, warn.conflicts = FALSE);
}



### 3倍標準差外的outlier ------------------------------
sigma3outlier_range <- function(df,variable){   
  
  min=0#description(df,variable)$min
  max=999#description(df,variable)$max
  #Station=description(df,variable)$Station
  
  
  df1=df %>%
    #mutate(year=year(LocalTime),month=month(LocalTime),監測站站名=Station) %>%
    filter(between(.data[[variable]],min,max)) %>%
    dplyr::summarise(lower_bound=mean(.data[[variable]])-3*sd(.data[[variable]]),upper_bound=mean(.data[[variable]])+3*sd(.data[[variable]])) 
    #mutate("監測站站名"=Station,"變數"=variable)
  df1
  
}




### 盒狀圖1.5倍IQR外的outlier -------------------------
IQRoutlier_range <- function(df,variable){
  
  min=0#description(df,variable)$min
  max=999#description(df,variable)$max
  #Station=description(df,variable)$Station
  
  df1 =  df %>%
    #mutate(year=year(LocalTime),month=month(LocalTime),監測站站名=Station) %>%
    filter(between(.data[[variable]],min,max) ) %>%
    dplyr::summarise(lower_bound=quantile(.data[[variable]],probs = 0.25)-1.5*IQR(.data[[variable]]),
                     upper_bound=quantile(.data[[variable]],probs = 0.75)+1.5*IQR(.data[[variable]])) #%>% 
    #mutate("監測站站名"=Station,"變數"=variable)
  
  df1
  
}




###chebyshev正常範圍異常值 ---------------------------------
#chebyshev正常範圍上下界
#p1:有多少比例為潛在的離群值，由p1計算k，可得truncated data的上下界(避免異常值所造成的bias)
#p2:預期會有多少比例的outlier，通常比p1小，outlier比例不超過p2
chebyshev_range <-function(df,var,p1,p2){
  
  min=0 #description(df,var)$min
  max=999 #description(df,var)$max
  #Station=description(df,var)$Station
  
  k1=1/sqrt(p1)
  ODV_1LU=df %>% 
    filter(between(.data[[var]],min,max)) %>%
    dplyr::summarise(Mean_all=mean(.data[[var]]),SD_all=sd(.data[[var]])) %>%
    mutate(ODV_1L=Mean_all-k1*SD_all,ODV_1U=Mean_all+k1*SD_all)
  
  k2=1/sqrt(p2)
  ODV_LU=df %>% 
    filter(between(.data[[var]],ODV_1LU$ODV_1L,ODV_1LU$ODV_1U)) %>%
    dplyr::summarise(Mean_trun=mean(.data[[var]]),SD_trun=sd(.data[[var]])) %>%
    mutate(ODV_L=Mean_trun-k2*SD_trun,ODV_U=Mean_trun+k2*SD_trun)
  
  ODV_LU
}



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


getSensorSQL <- function(tbName,time_Col,sid_Col,sensorID,period){
  
  sqlStr <- "SELECT * FROM "
  sqlStr <- paste(sqlStr, tbName , " where ",sid_Col," = '",sensorID,"' and  ",time_Col," = getdate()-", period, sep="" )
  
  sqlStr
}


calResult <- function(data,col,sn){
  
  sigma3Result <-  sigma3outlier_range(data,col)
  IQRResult <-  IQRoutlier_range(data,col)
  chebyshevResult <- chebyshev_range(data,col,0.2,0.05)
  
  
  #資料更新回資料庫
  sqlr_Update <- "UPDATE [dbo].[Sensor_Info] set "
  sqlr_Update <- paste(sqlr_Update, "[CHE_UP] = " ,chebyshevResult$ODV_U,", [CHE_DOWN] = ",chebyshevResult$ODV_L,", ")
  sqlr_Update <- paste(sqlr_Update, "[BOX_UP] = " ,IQRResult$upper_bound,", [BOX_DOWN] = ",IQRResult$lower_bound,", ")
  sqlr_Update <- paste(sqlr_Update, "[NORMAL_UP] = " ,sigma3Result$upper_bound,", [NORMAL_DOWN] = ",sigma3Result$lower_bound,", ")
  sqlr_Update <- paste(sqlr_Update, "[UPDATE_TIME] = getdate() ")
  sqlr_Update <- paste(sqlr_Update, " where SN = 1")
  print(sqlr_Update)
  dbGetQuery(basicConn, sqlr_Update)
}



#步驟
#1.先取出資料集
#2.算出最大最小值
#3.計算結果
#4.儲存結果

#資料庫連線
basicConn <- dbConnect(odbc(),
                 Driver = "{SQL Server Native Client 11.0}",
                 Server = "59.120.223.165",
                 Database = "SensorAD",
                 UID = "idmm",
                 PWD = "wj/3ck6tj4",
                 Port = 1433)

#取得需要做計算的儀器
querySensor <- dbSendQuery(basicConn,"select * from V_Sensor_Info where update_time > getdate()-update_FQ")
# select * from V_Sensor_Info where update_time > getdate()-update_FQ
SensorInfoList <- dbFetch(querySensor)

#print(nrow(SensorInfoList))
#print(length(SensorInfoList))

for (idx in 1:nrow(SensorInfoList)) {
  #print(SensorInfoList[2])
  SensorConnect <- setDBConnect(SensorInfoList$IP,SensorInfoList$DB_NAME,SensorInfoList$USERNAME,SensorInfoList$PASSWORD)
  queryStr <- getSensorSQL(SensorInfoList$TABLE_NAME ,SensorInfoList$TIME_COL,"username",SensorInfoList$SENSOR_ID,SensorInfoList$UPDATE_FQ)
  query <- dbSendQuery(SensorConnect, queryStr)
  data <- dbFetch(query)
  calResult(data,SensorInfoList$COLUMN_NAME,SensorInfoList$SN)
  #print(queryStr)
}







#--------------------------------------------------------------------------------------------------
SensorConnect <- dbConnect(odbc(),
                         Driver = "{SQL Server Native Client 11.0}",
                         Server = "192.168.51.72",
                         Database = "BochObservation",
                         UID = "ricky",
                         PWD = "ricky",
                         Port = 1433)

#server=192.168.51.72;database=BochObservation;uid=ricky;pwd=ricky

#dbReadTable(con, "Person")

query <- dbSendQuery(SensorConnect, "SELECT * FROM tblWeatherLink_5min where username='boch007' and localtime > getdate()-180")
data <- dbFetch(query)
#dbClearResult(query)
#print(data)





  #sigma3outlier_range(query,"UV")
print(sigma3Result$lower_bound)
print(IQRResult)
print(chebyshevResult)














