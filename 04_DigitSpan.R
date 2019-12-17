### load packages
library(lme4);library(lmerTest);library(ggplot2);library(plyr);library(dplyr);library(tidyr); library(Hmisc); library(reshape2); library(ggpubr)

#### Loads data [csv filename may need to be updated FYI]
digitspan<-read.csv("data_exp_6799-v8_task-fehk.csv")

#### Remove Extra Rows
digitspan <- subset(digitspan, digitspan$Zone.Type == "response_text_entry")

#investigating non-numeric characters####


non_numeric_indices <- which(is.na(as.numeric(as.character(digitspan$Response))))
non_numeric_rows <- digitspan[non_numeric_indices,] %>%
  select(Schedule.ID, Reaction.Time, Trial.Number, Response, Correct, Incorrect, answer)

#participant responses that require fixing: c(2270856, 2269762) Needs to be redone for this dataset
digitspan$Response <- str_split(digitspan$Response, "=", simplify = TRUE)[,1] %>%
  str_replace_all(pattern = "\\+", replacement = "")

digitspan$Correct <- digitspan$Response == as.character(digitspan$answer)
digitspan$Incorrect <- !digitspan$Correct

digitspan$Correct <- as.numeric(digitspan$Correct)
digitspan$Incorrect <- as.numeric(digitspan$Incorrect)


### Determine OVERALL accuracy 
digitspan_acc_byID <- ddply(digitspan, c("Schedule.ID"), summarise, mean_acc = mean(Correct), sum_acc = sum(Correct))

### Determine OVERALL RT (including incorrect)  
digitspan_RT_byID <- ddply(digitspan, c("Schedule.ID"), summarise, mean_RT = mean(Reaction.Time))

### Determine  RT (BUT only Correct)  
digitspan_RTType_byID <- ddply(digitspan, c("Schedule.ID", "Correct"), summarise, RT = mean(Reaction.Time))

### Rename Column Names 
digitspan_RTType_byID$Correct<-car::recode(digitspan_RTType_byID$Correct, "1='Correct';0='Incorrect'")
colnames(digitspan_RTType_byID)[colnames(digitspan_RTType_byID)=="Correct"] <- "Type"

### Still to do:
# Trim / Clean Based on Long RTs
# Exclude Participants via overall accuracy [some subjects got all incorrect, or very few correct, e.g., 10%] 


#Investigating outliers on reaction time
rt_outlier_sub_list <- digitspan_RT_byID %>%  
  filter(mean_RT %in% boxplot.stats(digitspan_RT_byID$mean_RT)$out)

rt_outlier <- digitspan %>%
  filter(Schedule.ID %in% rt_outlier_sub_list$Schedule.ID) %>%
  select(Schedule.ID, Reaction.Time, Trial.Number, Response, Correct, Incorrect, answer)

low_acc <- digitspan_acc_byID %>%
  filter(sum_acc <2) 


#This excludes 22 people on outlier rt and less than 2 correct responses########
conservatively_filtered <- digitspan %>% 
  filter(!(Schedule.ID  %in% rt_outlier_sub_list$Schedule.ID)) %>%
  filter(!(Schedule.ID %in% low_acc$Schedule.ID) )




