#calculates days with more than a certain percent missing#

setwd("C:/Users/treneau/Dropbox/WCMCseed/Analysis/GPX_AnalysisReneau/workspace")
missing_matrix <- matrix(nrow = 0, ncol = 3)

#decimal to round coordinates to#
decimal <- 4
#set cutoff point for how much data can be missing for a day to be included#
percentMissing <- .1

tom <- matrix(nrow = 200, ncol = 132)
d <- 1
e <- 1
timeDiff <- 0

#create a vector of subject names#
rawData1 <- c("01","02","03","05","06","08","09",10,14,15,16,18,19,24,26,31,32,39,40,41,44,49,50,55,59,60,63,65,67,69,70,71,76,81,83,84,91,92,97,98,99)
rawData2 <- c(1287,1530,1533,1555,1673,1738,1749,1837,1879,1958,1965,1996,2020,2022,2024,2025,2026,2027,2028,2029,2030,2032,2033,2034,2037,2039,2040,2041,2042,2043,2044)
rawData3 <- c(62,60,73,85,74,39,42,14,13,"09",34,48,26,21,19,49,"07",50,"03",27,36,16,22,"08",40,18,"06",53,55,"04","05",41,59,65,23,37,31,61,69,63,72,30,81,79,86,76,89,84,68,87,77,83,82,67,80,88,"01",75,12,17)
#rawData <- c(1287)
#cohortIdentifier <- "SC0"

for(a in 1:3){
  if(a==1){
    rawData <- rawData1
    cohortIdentifier <- "MM10"
  }
  if(a==2){
    rawData <- rawData2
    cohortIdentifier <- "SC0"
  }
  if(a==3){
    rawData <- rawData3
    cohortIdentifier <- "MS10"
  }

#sift through each subject file to extract coordinates and times#
for(q in 1:length(rawData)){
  #scan file#
  subjString <- paste(cohortIdentifier,rawData[q], sep = "")
  dataString <- gsub("XX",subjString,"XX_storyline.gpx")
  n <- scan(dataString, character(0), sep = "\n")
  LonVec <- rep(NA,100000)
  LatVec <- rep(NA,100000)
  DateVec <- rep(NA,100000)
  TimeVec <- rep(NA,100000)
  DateTimeVec <- rep(NA,100000)
  c <- 1
  
  #finding data in file and placing it in vectors#
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
      
      if(!is.na(x)&&!is.na(y)&&!is.na(dataDateTime)){
        LonVec[c] <- x
        LatVec[c] <- y
        DateVec[c] <- dataDate
        TimeVec[c] <- dataTime
        DateTimeVec[c] <- dataDateTime
        c <- c+1
      }
    }
  }
  
  LonVec <- LonVec[!is.na(LonVec)]
  LatVec <- LatVec[!is.na(LatVec)]
  DateVec <- DateVec[!is.na(DateVec)]
  TimeVec <- TimeVec[!is.na(TimeVec)]
  DateTimeVec <- DateTimeVec[!is.na(DateTimeVec)]
  
  #calculate timeframe#
  date1 <- DateVec[1]
  startDate <- DateVec[1]
  endDate <- DateVec[length(DateVec)]
  totalDays <- as.Date(endDate)-as.Date(startDate)+1
  dayCheck <- 0
  throwOutDates <- rep(NA,totalDays)
  
  min_miss <- character(totalDays)
  dates <- character(totalDays)
  subjectids <- character(totalDays)
  f <- 1 
  
  #for all dates which data was collected check to see if it's missing more than the cutoff percentage and if so add it a vector#
  while(as.Date(date1)<=as.Date(endDate)){
    check <- 24
    if(as.Date(date1)==as.Date(startDate)){
      for(s in 1:length(n)){
        if(grepl("<trk>",n[s])&&grepl(date1,n[s+4])){
          timeString1 <- paste(as.character(as.Date(date1)+1),"00:00:00.000",sep = "T")
          time1 <- as.POSIXct(timeString1,format = "%Y-%m-%dT%H:%M:%S")
          dateStart <- regexpr("<time>",n[s+4])[1]
          timeString2 <- substr(n[s+4],dateStart+6,dateStart+33)
          time2 <- as.POSIXct(timeString2,format = "%Y-%m-%dT%H:%M:%S")
          check <- as.numeric(difftime(time1,time2,units = "hours"))
        }
      }
    }
    for(s in 1:length(n)){
      if(as.Date(date1)!=as.Date(startDate)&&grepl("<trk>",n[s])&&grepl(date1,n[s+4])){
        dayCheck <- 1
        timeString1 <- paste(as.character(as.Date(date1)),"00:00:00.000",sep = "T")
        time1 <- as.POSIXct(timeString1,format = "%Y-%m-%dT%H:%M:%S")
        dateStart <- regexpr("<time>",n[s+4])[1]
        timeString2 <- substr(n[s+4],dateStart+6,dateStart+33)
        time2 <- as.POSIXct(timeString2,format = "%Y-%m-%dT%H:%M:%S")
        timeDiff <- timeDiff+difftime(time2,time1,units = "hours")
      }
      if(grepl("</trkseg>",n[s])&&(grepl(date1,n[s-3])||grepl(paste(as.character(as.Date(date1)+1),"00:00:00.000",sep = "T"),n[s-3]))){
        if(grepl("<trkseg>",n[s+1])){
          dateStart <- regexpr("<time>",n[s-3])[1]
          timeString1 <- substr(n[s-3],dateStart+6,dateStart+33)
          dateEnd <- regexpr("<time>",n[s+3])[1]
          timeString2 <- substr(n[s+3],dateEnd+6,dateEnd+33)
          time1 <- as.POSIXct(timeString1,format = "%Y-%m-%dT%H:%M:%S")
          time2 <- as.POSIXct(timeString2,format = "%Y-%m-%dT%H:%M:%S")
          timeDiff <- timeDiff+difftime(time2,time1,units = "hours")
          
        }
        if(grepl("</trk>",n[s+1])&&grepl(date1,n[s-7])){
          dateStart <- regexpr("<time>",n[s-3])[1]
          timeString1 <- substr(n[s-3],dateStart+6,dateStart+33)
          time1 <- as.POSIXct(timeString1,format = "%Y-%m-%dT%H:%M:%S")
          timeString2 <- paste(as.character(as.Date(date1)+1),"00:00:00.000",sep = "T")
          time2 <- as.POSIXct(timeString2,format = "%Y-%m-%dT%H:%M:%S")
          timeDiff <- timeDiff+difftime(time2,time1,units = "hours")
        }
      }
    }
    if(dayCheck == 0){
      timeDiff <- 24
    }
    min_miss[f] <- timeDiff
    dates[f] <- date1
    subjectids[f] <- subjString
    f <- f+1
    if(as.numeric(timeDiff)/check > percentMissing){
      throwOutDates[d] <- date1
      d <- d+1
    }
    timeDiff <- 0
    date1 <- as.character(as.Date(date1)+1)
    dayCheck <- 0
  }
  throwOutDates <- throwOutDates[!is.na(throwOutDates)]
  for(a in 2:(length(throwOutDates)+1)){
  tom[1,e] <- subjString
  tom[a,e] <- throwOutDates[a-1]
  }
  e <- e +1
  df <- matrix(nrow = 0, ncol=0)
  df <- cbind(min_miss,dates,subjectids)
  missing_matrix <- rbind(missing_matrix,df)
}
}
tom <- tom[-1,]
write.csv(tom, file = "throw_out_dates_all_cohorts.csv", row.names = FALSE)