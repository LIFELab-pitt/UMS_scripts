### load packages
library(lme4);library(lmerTest);library(ggplot2);library(plyr);library(dplyr);library(tidyr); library(Hmisc); library(reshape2); library(ggpubr); library(tidyverse)

#### Loads data [csv filename may need to be updated FYI]
dd<-read.csv("data_exp_6799-v8_task-i689.csv")

### Data cleaning routines
real_dd<-subset(dd,dd$Attempt==1)
real_dd$Trial.Number<-factor(real_dd$Trial.Number) #do we need this line
real_dd$Trial.Number_corrected<-as.numeric(as.character(real_dd$Trial.Number))

#Remove wording, so we just have integers for the delay
choices01_temp<-as.data.frame(str_split(real_dd$Amount1, " in ", simplify = TRUE)) #can we rewrite over the same variable
choices01_temp$temp01<-str_remove(choices01_temp$V1, 'Receive')
choices01_temp$temp02<-str_remove(choices01_temp$temp01, 'today')
choices01_temp$temp03<-str_remove(choices01_temp$temp02, ' ')
choices01_temp$amount<-as.numeric(gsub("\\$", "", choices01_temp$temp03))
choices01_temp$delay<-as.numeric(str_remove(choices01_temp$V2, 'days'))
choices01_temp$delay[is.na(choices01_temp$delay)]<-0
choices01_temp$subjID<-real_dd$Schedule.ID
choices01_temp$Trial.Number_corrected<-real_dd$Trial.Number_corrected

#Repeat: Remove wording, so we just have integers for the delay
choices02_temp<-as.data.frame(str_split(real_dd$Amount2, " in ", simplify = TRUE))
choices02_temp$temp01<-str_remove(choices02_temp$V1, 'Receive')
choices02_temp$temp02<-str_remove(choices02_temp$temp01, 'today')
choices02_temp$temp03<-str_remove(choices02_temp$temp02, ' ')
choices02_temp$amount<-as.numeric(gsub("\\$", "", choices02_temp$temp03))
choices02_temp$delay<-as.numeric(str_remove(choices02_temp$V2, 'days'))
choices02_temp$delay[is.na(choices02_temp$delay)]<-0
choices02_temp$subjID<-real_dd$Schedule.ID
choices02_temp$Trial.Number_corrected<-real_dd$Trial.Number_corrected

choices01_delayed<-subset(choices01_temp,choices01_temp$delay>choices02_temp$delay)
choices02_delayed<-subset(choices02_temp,! choices01_temp$delay>choices02_temp$delay)

choices01_delayed<-select(choices01_delayed,amount,delay,subjID,Trial.Number_corrected)
choices02_delayed<-select(choices02_delayed,amount,delay,subjID,Trial.Number_corrected)

delayed_joined<-full_join(choices01_delayed,choices02_delayed)
delayed_joined<-delayed_joined[order(delayed_joined$subjID, delayed_joined$Trial.Number_corrected),]

colnames(delayed_joined)[colnames(delayed_joined)=="delay"] <- "delay_later"
colnames(delayed_joined)[colnames(delayed_joined)=="amount"] <- "amount_later"

choices01_immediate<-subset(choices01_temp,choices01_temp$delay==0)
choices02_immediate<-subset(choices02_temp,choices02_temp$delay==0)
choices01_immediate<-select(choices01_immediate,amount,delay,subjID,Trial.Number_corrected)
choices02_immediate<-select(choices02_immediate,amount,delay,subjID,Trial.Number_corrected)
immediate_joined<-full_join(choices01_immediate,choices02_immediate)
immediate_joined<-immediate_joined[order(immediate_joined$subjID, immediate_joined$Trial.Number_corrected),]

colnames(immediate_joined)[colnames(immediate_joined)=="delay"] <- "delay_sooner"
colnames(immediate_joined)[colnames(immediate_joined)=="amount"] <- "amount_sooner"

combined<-full_join(immediate_joined,delayed_joined)
colnames(combined)[colnames(combined)=="Trial.Number_corrected"] <- "trial"

combined <- combined[c("subjID", "trial","delay_later","amount_later","delay_sooner","amount_sooner")]

actual_choices_temp<-as.data.frame(str_split(real_dd$Response, " in ", simplify = TRUE))
actual_choices_temp$temp01<-str_remove(actual_choices_temp$V1, 'Receive')
actual_choices_temp$temp02<-str_remove(actual_choices_temp$temp01, 'today')
actual_choices_temp$temp03<-str_remove(actual_choices_temp$temp02, ' ')
actual_choices_temp$chosen_amount<-as.numeric(gsub("\\$", "", actual_choices_temp$temp03))
actual_choices_temp$chosen_delay<-as.numeric(str_remove(actual_choices_temp$V2, 'days'))
actual_choices_temp$chosen_delay[is.na(actual_choices_temp$delay)]<-0
actual_choices_temp$subjID<-real_dd$Schedule.ID
actual_choices_temp$trial<-real_dd$Trial.Number_corrected

actual_choices<-select(actual_choices_temp,subjID,trial,chosen_amount,chosen_delay)

combined$choice<-as.numeric(actual_choices$chosen_amount==combined$amount_later)

rm(actual_choices,actual_choices_temp,choices01_delayed,choices01_delayed,choices01_immediate,choices01_immediate_reduced,choices01_temp,choices02_delayed,choices02_delayed,choices02_immediate,choices02_immediate_reduced,choices02_temp,dd,delayed_joined,delayed_joined,immediate_joined,immediate_joined,real_dd)

### Data output here   
write.table(combined, file = "./delay_dd.txt", sep = "\t",row.names = FALSE)


### Starting hBayes delay modeling here
# Install.packages("hBayesDM",dependencies = TRUE)
library(hBayesDM)
# There's often issues with needing to install command-line tools for OS-X

### currently there's an R-stan issue with the current version of R
data="delay_dd.txt"
output <- dd_cs(data, niter = 2000, nwarmup = 1000, nchain = 4, ncore = 4)

plot(output, type = "trace")
plot(output,type = "simple")
plot(output,type = "dist")

### Still to do:
# Double-check syntax on 
# https://ccs-lab.github.io/hBayesDM/articles/getting_started.html
# Work on pulling out individual/subject-level estimates
