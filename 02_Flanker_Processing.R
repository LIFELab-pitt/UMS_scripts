### load packages
library(lme4);library(lmerTest);library(ggplot2);library(plyr);library(dplyr);library(tidyr); library(Hmisc); library(reshape2); library(ggpubr)

#### Loads data [csv filename may need to be updated FYI]
flanker_data <- read.csv("data_exp_6799-v8_task-jlw3.csv")

#### Remove Flanker Practice Trials
flanker_data<-subset(flanker_data,as.numeric(as.character(flanker_data$Event.Index))>15)

### Only include participants who complet ed the task
flanker_data$include <- ifelse(flanker_data$Trial.Number == "END TASK", 1, 0)
flanker_complete <- ddply(flanker_data, c("Schedule.ID"), summarise, sum_include = sum(include))
flanker_complete <- subset(flanker_complete, flanker_complete$sum_include != 0)
flanker_complete <- as.vector(unique(flanker_complete$Schedule.ID))
flanker_data <- subset(flanker_data, flanker_data$Schedule.ID %in% flanker_complete)

### Subset to include only the necessary rows (exclude practice trials, exclude extra rows)
flanker_data <- subset(flanker_data, flanker_data$display == "Trial")
flanker_data <- subset(flanker_data, flanker_data$Zone.Type == "response_keyboard")
flanker_data <- subset(flanker_data, flanker_data$Type == "Congruent" | flanker_data$Type == "Incongruent")

### Determine OVERALL accuracy at identifying the central arrow 
flanker_data$acc <-(flanker_data$Correct==1)

### Group by participant just to check that nobody is doing the task completely incorrectly
flanker_acc_byID <- ddply(flanker_data, c("Schedule.ID"), summarise, mean_acc = mean(acc))

### Overall Accuracy 
flanker_acc_byIDbyType <- ddply(flanker_data, c("Schedule.ID","Type"), summarise, mean_acc = mean(acc))

### Subset to include only correct responses
flanker_data <- subset(flanker_data, flanker_data$acc == 1)

### REMOVING OUTLIERS BASED ON RT 
### Subset to exclude RTs above 2500 ms
flanker_data <- subset(flanker_data, flanker_data$Reaction.Time < 2500)

### Group RT by Participant ID 
flanker_rt_byID <- ddply(flanker_data, c("Schedule.ID"), summarise,P_rt = mean(Reaction.Time))

### determine the mean RT, and the minimum and maximum participant RTs (3 SDs above or below the mean RT)
flanker_rt_byID$grand_mean <- mean(flanker_rt_byID$P_rt)
sd_rt <- sd(flanker_rt_byID$P_rt)
flanker_rt_byID$min <- flanker_rt_byID$grand_mean - (3*sd_rt)
flanker_rt_byID$max <- flanker_rt_byID$grand_mean + (3*sd_rt)

### Add a column that flags participants whose mean RT is less than the minimum or greater than the maximum
flanker_rt_byID$flag <- ifelse((flanker_rt_byID$P_rt < flanker_rt_byID$min) | (flanker_rt_byID$P_rt > flanker_rt_byID$max), 1, 0)

### Here's a plot of subjects that are included/excluded based on RT
plot(flanker_rt_byID$flag,flanker_rt_byID$P_rt)

### List of all the PIDs that need to be excluded for LDT RTs 
flanker_rt_exclude <- subset(flanker_rt_byID, flanker_rt_byID$flag ==1)
flanker_rt_exclude <- flanker_rt_exclude$Schedule.ID

### Subset to include only participants who don't meet the exclusion criteria
flanker_data <- subset(flanker_data, !(flanker_data$Schedule.ID %in% flanker_rt_exclude))

### Group RT by participant ID 
flanker_byID <- ddply(flanker_data, c("Schedule.ID"), summarise, RT = mean(Reaction.Time))

### Group RT by participant ID and congruity
flanker_byID_congruity <- ddply(flanker_data, c("Schedule.ID", "Type"), summarise, RT = mean(Reaction.Time))

### Put data in long format
flanker_byID_congruity <- spread(flanker_byID_congruity, Type, RT)

### Calculate the difference between incongruent and congruent trials (incongruity cost)
flanker_byID_congruity$flanker_score <- flanker_byID_congruity$Incongruent - flanker_byID_congruity$Congruent



