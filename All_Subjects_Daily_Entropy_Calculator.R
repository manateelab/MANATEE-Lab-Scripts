#set working directory location to where gpx files are located#
setwd("C:/Users/treneau/Dropbox/WCMCseed/Analysis/GPX_AnalysisReneau/workspace")

#read file that contains dates with 10%+ data missing#
tod_df <- read.csv("throw_out_dates_all_cohorts.csv",stringsAsFactors = FALSE)
re_matrix <- matrix(nrow = 0, ncol = 3)

#decimal to round coordinates to#
decimal <- 4

#create a vector of subject names#
rawData3 <- c("01","02","03","05","06","08","09",10,14,15,16,18,19,24,26,31,32,39,40,41,44,49,50,55,59,60,63,65,67,69,70,71,76,81,83,84,91,92,97,98,99)
rawData1 <- c(1287,1530,1533,1555,1673,1738,1749,1837,1879,1958,1965,1996,2020,2022,2024,2025,2026,2027,2028,2029,2030,2032,2033,2034,2037,2039,2040,2041,2042,2043,2044)
rawData2 <- c(62,60,73,85,74,39,42,14,13,"09",34,48,26,21,19,49,"07",50,"03",27,36,16,22,"08",40,18,"06",53,55,"04","05",41,59,65,23,37,31,61,69,63,72,30,81,79,86,76,89,84,68,87,77,83,82,67,80,88,"01",75,12,17)
#  rawData <- c(1287)
#  cohortIdentifier <- "SC0"


#loop through cohorts#
for(a in 1:3){
  if(a==1){
    rawData <- rawData3
    cohortIdentifier <- "MM10"
  }
  if(a==3){
    rawData <- rawData1
    cohortIdentifier <- "SC0"
  }
  if(a==2){
    rawData <- rawData2
    cohortIdentifier <- "MS10"
  }

#sift through each subject file to extract coordinates and times#
for(q in 1:length(rawData)){
  
  #scan file#
  subjString <- paste(cohortIdentifier,rawData[q], sep = "")
  dataString <- gsub("XX",subjString,"XX_storyline.gpx")
  n <- scan(dataString, character(0), sep = "\n")
  
  #create vectors for data#
  LonVec <- rep(NA,100000)
  LatVec <- rep(NA,100000)
  DateVec <- rep(NA,100000)
  TimeVec <- rep(NA,100000)
  DateTimeVec <- rep(NA,100000)
  TypeVec <- rep(NA,100000)
  c <- 1  # index for parsing of GPS code
  
  #find data in file and place it in vectors#
  for(i in 1:length(n)){
    if(grepl("<trkpt ",n[i])){
      
      lonStart <- regexpr('lon="',n[i])[1]
      lonEnd <- regexpr('" ',n[i])[1]
      x <- round(as.numeric(substr(n[i],lonStart+5,lonEnd-1)), digits = decimal)
      
      latStart <- regexpr('lat="',n[i])[1]
      latEnd <- regexpr('">',n[i])[1]
      y <- round(as.numeric(substr(n[i],latStart+5,latEnd-1)), digits = decimal)
      
      dateStart <- regexpr("<time>",n[i+1])[1]
      timeEnd <- regexpr("</time>",n[i+1])[1]
      dateEnd <- regexpr("T",n[i+1])[1]
      dataDate <- substr(n[i+1],dateStart+6,dateEnd-1)
      dataTime <- substr(n[i+1],dateStart+17,timeEnd-1)
      dataDateTime <- substr(n[i+1],dateStart+6,timeEnd-1)
      
      typeEnd <- regexpr("</type>",n[i+2])[1]
      dataType <- substr(n[i+2],15,(typeEnd-1))
      
      #add information to vectors#
        LonVec[c] <- x
        LatVec[c] <- y
        TypeVec[c] <- dataType
        DateVec[c] <- dataDate
        TimeVec[c] <- dataTime
        DateTimeVec[c] <- dataDateTime
        c <- c+1
    }
  }
  LonVec <- LonVec[!is.na(LonVec)]
  LatVec <- LatVec[!is.na(LatVec)]
  DateVec <- DateVec[!is.na(DateVec)]
  TimeVec <- TimeVec[!is.na(TimeVec)]
  DateTimeVec <- DateTimeVec[!is.na(DateTimeVec)]
  TypeVec <- TypeVec[!is.na(TypeVec)]
  
  
  #create matrix of all coordinates#
  positionMatrix = matrix(
    c(LonVec,LatVec),
    nrow=length(LonVec),
    ncol=2)
  
  #calculate timeframe#
  startDate <- DateVec[1]
  endDate <- DateVec[length(DateVec)]
  totalDays <- as.Date(endDate)-as.Date(startDate)+1
  dayCheck <- 0
  
  #for all dates which data was collected check to see if it's missing more than the cutoff percentage and if so add it a vector#
  numSubs <- length(rawData3) + length(rawData2) + length(rawData1)
  for(idx in 1:numSubs)
  {
    if(colnames(tod_df)[idx]==subjString)
      throwOutDates <- tod_df[,idx]
  }
  throwOutDates <- throwOutDates[!is.na(throwOutDates)]
  
  #calculate total number of minutes during the time of data collection#
  totalMinutes <- as.numeric(totalDays*1440)
  
  for(l in 1:length(throwOutDates)){
    throwOutDates[l] <- as.character(as.Date(as.POSIXct(throwOutDates[l], format = "%m/%d/%Y")))
  }
  
  #find coordinates with dates that are going to be thrown out#
  for(o in 1:length(throwOutDates)){
    for(p in 1:length(DateVec)){
      if(grepl(DateVec[p],throwOutDates[o])){
        positionMatrix[p,1]<-"remove"
        positionMatrix[p,2]<-"remove"
        DateTimeVec[p] <- "remove"
        DateVec[p] <- "remove"
        TypeVec[p] <- "remove"
      }
    }
  }
  
  #find coordinates with duplicate minutes (exclude place trackpoints)#
  uniqueMinutes <- 0
  for(k in 1:length(TimeVec)){
    if(grepl(substr(TimeVec[k],1,5),substr(TimeVec[k+1],1,5))&&!grepl("place",TypeVec[k])){
      positionMatrix[k,1]<-"remove"
      positionMatrix[k,2]<-"remove"
      DateTimeVec[k] <- "remove"
      DateVec[k] <- "remove"
      TypeVec[k] <- "remove"
    }
    else
      uniqueMinutes <- uniqueMinutes+1
  }
  
  
  #remove coordinates with duplicate minutes or thrown out#
  positionMatrix <- positionMatrix[positionMatrix[,1]!="remove",]
  DateTimeVec <-DateTimeVec[DateTimeVec!="remove"]
  DateVec <- DateVec[DateVec!="remove"]
  TypeVec <- TypeVec[TypeVec!="remove"]
  
  #create vector to hold roaming entropy values for all days for that subject#
  RE <- character(totalDays)
  dates <- character(totalDays)
  subjectids <- character(totalDays)
  e <- 1 
  
  #calculate entropy for each day#
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
      if(difftime(as.POSIXct(DateTimeVec[as.numeric(dataLocation[length(dataLocation)])],format = "%Y-%m-%dT%H:%M:%S"),as.POSIXct(as.character(date1)),units = "mins")>1440){
      dmsize <- 1500
      dayMatrix = matrix(
        nrow=dmsize,
        ncol=2
      )
      }
      
      #check for first day#
      if(as.Date(date1)==startDate){
        dmsize <- as.numeric(difftime(as.POSIXct(as.character((as.Date(date1)+1))),as.POSIXct(DateTimeVec[as.numeric(dataLocation[1])],format = "%Y-%m-%dT%H:%M"), units = "mins"))+1
        dayMatrix = matrix(
          nrow=dmsize,
          ncol=2
        )
        beginDate <- as.POSIXct(DateTimeVec[as.numeric(dataLocation[1])],format = "%Y-%m-%dT%H:%M")
      }
      
      startPoint <- 1
      for(u in 1:length(dataLocation)){
        tTest <- as.POSIXct(DateTimeVec[as.numeric(dataLocation[u])],format = "%Y-%m-%dT%H:%M:%S")
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
    if(length(dataLocation)==0)
      sampleRE <- NA
    RE[e] <- sampleRE
    dates[e] <- date1
    subjectids[e] <- subjString
    e <- e+1
    date1 <- as.character(as.Date(date1)+1)
  }
  
  #put daily entropy data into data frame#
  df <- matrix(nrow = 0, ncol=0)
  df <- cbind(RE,dates,subjectids)
  re_matrix <- rbind(re_matrix,df)
}
}
write.csv(re_matrix,file = "re_matrix.csv")