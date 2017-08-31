setwd("C:/Users/treneau/Dropbox/WCMCseed/MSD/MSD_csvs")

decimal <- 4
file_list <- list.files(getwd())
final_df <- as.data.frame(matrix(nrow = length(file_list),ncol = 34))
DaysVec <- seq(as.Date("2017-07-24"), as.Date("2017-08-26"), by="days")
final_df[1,] <- as.character(DaysVec)
final_df[1,1] <- "subject_ID"
rowi <- 1

for(a in 1:length(file_list)){
  rowi <- rowi + 1
  final_df[rowi,1] <- substr(file_list[a],1,6)
  temp_df <- read.csv(file_list[a])
  startDate <- substr(temp_df[1,12],1,10)
  endDate <- substr(temp_df[nrow(temp_df),12],1,10)
  totalDays <- as.Date(endDate)-as.Date(startDate)+1
  LonVec <- round(as.numeric(temp_df[,3]), digits = decimal)
  LatVec <- round(as.numeric(temp_df[,2]), digits = decimal)
  TypeVec <- temp_df[,9]
  DateVec <- substr(temp_df[,12],1,10)
  TimeVec <- substr(temp_df[,12],12,19)
  DateTimeVec <- substr(temp_df[,12],1,19)
  positionMatrix = matrix(
    c(LonVec,LatVec),
    nrow=length(LonVec),
    ncol=2)
  
  #remove network coordinates#
  for(k in 1:length(TimeVec)){
    if(grepl("Network",TypeVec[k])){
      positionMatrix[k,1]<-NA
      positionMatrix[k,2]<- NA
      DateTimeVec[k] <- NA
      DateVec[k] <- NA
      TimeVec[k] <- NA
      TypeVec[k] <- NA
    }
  }
  positionMatrix <- positionMatrix[!is.na(positionMatrix[,1]),]
  DateTimeVec <-DateTimeVec[!is.na(DateTimeVec)]
  DateVec <- DateVec[!is.na(DateVec)]
  TypeVec <- TypeVec[!is.na(TypeVec)]
  TimeVec <- TimeVec[!is.na(TimeVec)]
  
  
  date1 <- startDate
  for(b in 1:totalDays) {
    
    #initialize day matrix #
    dmsize = 1440
    dayMatrix = matrix(
      nrow=dmsize,
      ncol=2)
    
    #find data points with given date#
    dataLocation <- character(2000) # preallocating for max possible number of trackpoints in a day
    f <- 1
    for (t in 1:length(DateVec)){
      if(DateVec[t]==date1){
        dataLocation[f] <- t
        f <- f +1
      }
      if(DateVec[t]>date1)
        break
    }
    dataLocation <- dataLocation[!dataLocation %in% ""]
    
    beginDate <- as.POSIXct(as.character(date1))
    
    #place coordinates in proper timeline (each row of a matrix represents a minute of the timeframe of collected data)#
    if(length(dataLocation) >= 1)
    {
      
      #check for daylight savings#
      if(difftime(as.POSIXct(DateTimeVec[as.numeric(dataLocation[length(dataLocation)])],format = "%Y-%m-%d %H:%M:%S"),as.POSIXct(as.character(date1)),units = "mins")>1440){
        dmsize <- 1500
        dayMatrix = matrix(
          nrow=dmsize,
          ncol=2
        )
      }
      
      startPoint <- 1
      for(u in 1:length(dataLocation)){
        tTest <- as.POSIXct(DateTimeVec[as.numeric(dataLocation[u])],format = "%Y-%m-%d %H:%M:%S")
        timeInMinutes <- floor(as.numeric(difftime(tTest,beginDate,units = "mins")))+1
        
        if(startPoint==1){
          for(v in 1:(timeInMinutes-1)){
            dayMatrix[v,] <- positionMatrix[as.numeric(dataLocation[u]),]
          }
        }
        
        if(u==length(dataLocation)&&startPoint<=dmsize){
          for(w in startPoint:dmsize){
            dayMatrix[w,] <- positionMatrix[as.numeric(dataLocation[u]),]
          }
        }
        
        if(timeInMinutes-startPoint>0&&u!=1&&u!=length(dataLocation)){
          
          previous_lat <- as.numeric(positionMatrix[as.numeric(dataLocation[u])-1,1])
          current_lat <- as.numeric(positionMatrix[as.numeric(dataLocation[u]),1])
          previous_lon <- as.numeric(positionMatrix[as.numeric(dataLocation[u])-1,2])
          current_lon <- as.numeric(positionMatrix[as.numeric(dataLocation[u]),2])
          previous_time <- startPoint-1
          current_time <- timeInMinutes
          avg_lat_velocity <- (current_lat-previous_lat)/(current_time-previous_time)
          avg_lon_velocity <- (current_lon-previous_lon)/(current_time-previous_time)
          
          for(z in startPoint:(timeInMinutes-1)){
            dayMatrix[z,1] <- round((avg_lat_velocity*(z-previous_time)+previous_lat), digits = decimal)
            dayMatrix[z,2] <- round((avg_lon_velocity*(z-previous_time)+previous_lon), digits = decimal)
          }
        }
        
        dayMatrix[timeInMinutes,] <- positionMatrix[as.numeric(dataLocation[u]),]
        startPoint <- timeInMinutes+1  
      } 
    }
    
    #convert matrix to dataframe#
    df <- as.data.frame(dayMatrix,row.names=NULL)
    colnames(df) <- c("lon","lat")
    
    #calculate roaming entropy for that date and place it in RE vector#
    library(plyr)
    uniquePositions <- ddply(df,.(lon,lat),nrow)
    uniquePositions$p <- uniquePositions$V1/dmsize
    uniquePositions$plogp <- uniquePositions$p*log2(uniquePositions$p)
    sampleRE = (-sum(uniquePositions$plogp))
    for(g in 1:ncol(final_df)){
      if(grepl(date1,final_df[1,g]))
        final_df[rowi,g] <- sampleRE
    }
    date1 <- as.character(as.Date(date1)+1)
  }
}
setwd("C:/Users/treneau/Dropbox/WCMCseed/MSD")
write.csv(final_df,file = "retroactive_followmee_entropy_network_excluded.csv", row.names = FALSE)