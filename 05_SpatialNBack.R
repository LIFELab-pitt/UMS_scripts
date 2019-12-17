
### load packages
library(lme4);library(lmerTest);library(ggplot2);library(plyr);library(dplyr);library(tidyr); library(Hmisc); library(reshape2); library(ggpubr)

#### Loads data [csv filename may need to be updated FYI]
workingmemory<-read.csv("data_exp_6799-v8_task-fuj2.csv")

#### Remove Extra Rows
#Are we able to use the NoGo trials for anything? Are we interested in times participants clicked when they weren't supposed to? ####
workingmemory<-subset(workingmemory,workingmemory$Response=="go")

### Determine  accuracy by participant
workingmemory_acc_byID <- ddply(workingmemory, c("Schedule.ID"), summarise, mean_acc = mean(Correct))

### Determine OVERALL RT (including incorrect)  
workingmemory_RT_byID <- ddply(workingmemory, c("Schedule.ID"), summarise, mean_RT = mean(Reaction.Time))

### Determine  RT (BUT Breaking it up correct/incorrect) 
workingmemory_RTType_byID <- ddply(workingmemory, c("Schedule.ID", "Correct"), summarise, RT = mean(Reaction.Time))

### Recode Columns
workingmemory_RTType_byID$Correct<-car::recode(workingmemory_RTType_byID$Correct, "1='Correct';0='Incorrect'")
colnames(workingmemory_RTType_byID)[colnames(workingmemory_RTType_byID)=="Correct"] <- "Type"

### Still to do:
# Trim / Clean Based on Long RTs
# Exclude Participants via overall accuracy [some subjects got all incorrect, or very few correct, e.g., 25%] 


