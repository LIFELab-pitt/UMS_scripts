### load packages
library(lme4);library(lmerTest);library(ggplot2);library(plyr);library(dplyr);library(tidyr); library(Hmisc); library(reshape2); library(ggpubr)

#### Loads data [csv filename may need to be updated FYI]
stroop<-read.csv("data_exp_6799-v8_task-u4v9.csv")

#### Remove Extra Rows
stroop <- subset(stroop, stroop$display == "Trials")
stroop <- subset(stroop, stroop$Zone.Type == "response_keyboard")

#### Recode Trial Types into Congruent / Incongruent
stroop$Stimulus_Type<-car::recode(stroop$Stimulus,'"BLUeblue.jpg"="Congruent";"BLUEblue.jpg"="Congruent";"BLUEgreen.jpg"="Incongruent";"BLUEred.jpg"="Incongruent";"BLUEyellow.jpg"="Incongruent";"GREENblue.jpg"="Incongruent";"GREENgreen.jpg"="Congruent";"GREENred.jpg"="Incongruent";"GREENyellow.jpg"="Incongruent";"REDblue.jpg"="Incongruent";"REDgreen.jpg"="Incongruent";"REDred.jpg"="Congruent";"REDyellow.jpg"="Incongruent";"YELLOWblue.jpg"="Incongruent";"YELLOWgreen.jpg"="Incongruent";"YELLOWred.jpg"="Incongruent";"YELLOWyellow.jpg"="Congruent"')

### Prepping for OVERALL accuracy calculation
stroop$acc <-(stroop$Correct==1)

### REMOVING OUTLIERS BASED ON RT 
### Subset to exclude RTs above 4000 ms
stroop <- subset(stroop, stroop$Reaction.Time < 4000)

### Group OVERALL RT by Participant ID 
stroop_rt_byID <- ddply(stroop, c("Schedule.ID"), summarise,P_rt = mean(Reaction.Time))

### determine the mean RT, and the minimum and maximum participant RTs (3 SDs above or below the mean RT)
stroop_rt_byID$grand_mean <- mean(stroop_rt_byID$P_rt)
sd_rt <- sd(stroop_rt_byID$P_rt)
stroop_rt_byID$min <- stroop_rt_byID$grand_mean - (3*sd_rt)
stroop_rt_byID$max <- stroop_rt_byID$grand_mean + (3*sd_rt)

### Add a column that flags participants whose mean RT is less than the minimum or greater than the maximum
stroop_rt_byID$flag <- ifelse((stroop_rt_byID$P_rt < stroop_rt_byID$min) | (stroop_rt_byID$P_rt > stroop_rt_byID$max), 1, 0)
stroop_rt_exclude <- subset(stroop_rt_byID, stroop_rt_byID$flag != 0)
stroop_rt_exclude <- as.vector(c(unique(stroop_rt_exclude$Schedule.ID)))
stroop <- subset(stroop, !(stroop$Schedule.ID %in% stroop_rt_exclude))

### Determine OVERALL accuracy after removing problematic subs (OVERALL)
stroop_acc_byID <- ddply(stroop, c("Schedule.ID"), summarise, mean_acc = mean(acc))

### Determine OVERALL accuracy after removing problematic subs (by TYPE)
stroop_acc_byID_bycongruity <- ddply(stroop, c("Schedule.ID", "Stimulus_Type"), summarise, acc = mean(acc))


### Determine RT after removing problematic subs (OVERALL)
stroop_byID <- ddply(stroop, c("Schedule.ID"), summarise, RT = mean(Reaction.Time))

### Determine RT after removing problematic subs (by TYPE)
stroop_byID_congruity <- ddply(stroop, c("Schedule.ID", "Stimulus_Type"), summarise, RT = mean(Reaction.Time))

### Put data in long format
stroop_byID_congruity <- spread(stroop_byID_congruity, Stimulus_Type, RT)

### Calculate the difference between incongruent and congruent trials (incongruity cost)
stroop_byID_congruity$stroop_score <- stroop_byID_congruity$Incongruent - stroop_byID_congruity$Congruent


### Still to do:
# Trim / Clean Based on Long RTs
# Exclude Participants via RT [at least one subject has very high stroop_score, e.g., >800 ms)