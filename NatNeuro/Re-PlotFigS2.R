library(ggplot2)
setwd("/Users/aheller/Dropbox/Cornell/Research/WCMCseed/Analysis/Manuscript/MovesManuscript_Final/NatNeuro/Scripts")

D1 <- read.table("Missing Data Percent Cutoffs RAW DATA_longForm.csv",header=T, sep=",",as.is=TRUE,strip.white=TRUE,fill=TRUE)
D1$Cohort <- as.factor(D1$Cohort)
D1$Cutoff <- as.factor(D1$Cutoff)
p1 <- ggplot(data=D1, aes(x=Cutoff, y=Percent.Missing, fill=Cohort)) +
  geom_bar(stat="summary", fun.y = "mean", position=position_dodge(width = 0.9), colour="black") +
  geom_point(data=D1, aes(x=Cutoff, y=Percent.Missing, fill=Cohort), position=position_dodge(width = 0.9), alpha = 0.4) + 
  theme_bw() + theme(panel.grid.major = element_blank()) +
  xlab("Within Day Threshold (% of Day Allowed To Be Missing)") +
  ylab("% of Days Removed From Sampling Period") 
p1

p1 <- ggplot(data=D1, aes(x=Cutoff, y=Final.Days, fill=Cohort)) +
  geom_bar(stat="summary", fun.y = "mean", position=position_dodge(width = 0.9), colour="black") +
  geom_point(data=D1, aes(x=Cutoff, y=Final.Days, fill=Cohort), position=position_dodge(width = 0.9), alpha = 0.4) +
  theme_bw() + theme(panel.grid.major = element_blank()) +
  xlab("Within Day Threshold (% of Day Allowed To Be Missing)") +
  ylab("Number of Days Remaining")
p1
