
#### A simple script to simply pull student's ID numbers 

subID <- read.csv("data_exp_6799-v8_questionnaire-9amv.csv")
subID_usable <- subset(subID, subID$Question.Key == "response-2")
write.csv(subID_usable,"subID_usable.csv")

### This will eventually be used to match Participant.Private.ID [from Gorilla] with Student ID

#Coerce Question.Key to character 
subID$Question.Key <- as.character(subID$Question.Key)

temp <- subID %>%
  filter(Question.Key == "response-2") %>% 
  select(Schedule.ID, Response) %>%
  group_by(Schedule.ID) %>%
  count(Response) %>%
  arrange(n)
