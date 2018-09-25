library(tidyverse))

#Get file with all roaming entropy data organized by date.#
longdata <- read.csv("MS1003_storyline_re_matrix.csv")
longdata <- longdata %>% rename(RecordedDate= date)
longdata <- longdata %>% rename(RE_Filtered= roaming.entropy)

PE_dataframe <- data.frame(matrix(ncol = 4, nrow = nrow(longdata)))
colNames <- c("RE_Filtered", "Recorded_Date","Prediction", "PE") #Column names to save.
colnames(PE_dataframe) <- colNames

PE_dataframe$Recorded_Date = longdata$RecordedDate;
PE_dataframe$RE_Filtered = longdata$RE_Filtered;

alpha = 0.1;
GroupAverage = mean(longdata$RE_Filtered, na.rm = TRUE); #Calculate group mean ingoring NA values.
row_counter = 1; #R indexes at 1. Keeps track of current row.

  inside_counter = 1; #R indexes starting at 1. Start here (the first point of data for this participant).
  for(i in 1:nrow(longdata)) #While on the current participant, run this code.
  {
    if (inside_counter == 1) #For first trial per participant, set prediction (for that row) equal to the group average.
    {   
      PE_dataframe$Prediction[row_counter] = GroupAverage;
    }
    else if (inside_counter == 2) #For the second trial, predict the exact same RE as the first trial's true RE.
    {	
      ########################
      #Check if the first trial is NA (if so, make 2nd day's prediction just equal to yesterday's prediction).
      if(!is.na(PE_dataframe$RE_Filtered[row_counter-1]))
      {
        PE_dataframe$Prediction[row_counter] = PE_dataframe$RE_Filtered[row_counter-1];
        PE_dataframe$PE[row_counter] = PE_dataframe$RE_Filtered[row_counter] - PE_dataframe$Prediction[row_counter]; #Prediction is just real - predicted.
        #If current row's RE is NA, then PE will just stay as NA (NA-value = NA).
      }
      
      else
      {
        PE_dataframe$Prediction[row_counter] = PE_dataframe$Prediction[row_counter-1];
      }
    }
    #If not first or second trial, follow these steps.
    else
    {
      
      yesterday_RE = PE_dataframe$RE_Filtered[row_counter-1]; #Find actual RE of trial before.
      yesterday_prediction = PE_dataframe$Prediction[row_counter-1];
      yesterday_error = PE_dataframe$PE[row_counter-1];
      today_RE = PE_dataframe$RE_Filtered[row_counter];
      
      
      if (!is.na(PE_dataframe$RE_Filtered[row_counter]))#check if RE value is NA. If it is NA, it goes to next part.
      {
        
        #Make prediction based on estimated RE of trial before + alpha*prediction error from last trial.
        prediction = sum(alpha*yesterday_error, yesterday_prediction, na.rm=TRUE); #update prediction based on alpha and last trial's prediction error value.
        PE_dataframe$Prediction[row_counter]= prediction;
        PE = today_RE - prediction;
        PE_dataframe$PE[row_counter] = PE; #Save prediction error in current row.
        
      }
      
      
      else #If RE estimate for this trial is "NA", do this.
      {
        
        #Make prediction based on RE of trial before + alpha*prediction error from last trial.
        prediction = sum(alpha*yesterday_error,yesterday_prediction, na.rm=TRUE) #update prediction based on alpha and last trial's prediction error value.
        PE_dataframe$Prediction[row_counter]= prediction;
        #Do nothing about PE (keep it NA).
        
      }
      
    }
    
    inside_counter=inside_counter+1; # Keeps track of current iteration (within this subject). 
    row_counter=row_counter+1; #Should now store next row.
  }
  

setwd("C:/Users/treneau/Documents/Potential Figures")
write.csv(PE_dataframe,file = "MS1003_PE.csv", row.names = FALSE)

