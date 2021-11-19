# Example preprocessing script.
#cyber <-security.1_step-activity

data_list2<-cyber.security.2_enrolments
df2 <- data.frame(data_list2)
library(dplyr)
head(df2)
df =na.omit(df2)

df2 <- df  [(!(df2$gender  =="Unknown") & !(df2$age_range  =="Unknown")),]
head(df2)

data_list3<-cyber.security.3_enrolments
df3 <- data.frame(data_list3)
library(dplyr)
head(df3)
df =na.omit(df3)

df3 <- df  [(!(df3$gender  =="Unknown") & !(df3$age_range  =="Unknown")),]
head(df3)
head(df3)

head(cyber.security.2.step.activity)
activity2 =na.omit(cyber.security.2.step.activity)
head(cyber.security.1.step.activity)
nrow(cyber.security.1.step.activity)
activity3 =na.omit(cyber.security.1.step.activity)
nrow(activity3)
 
activity2 <- cyber.security.2.step.activity
activity1 <- cyber.security.1.step.activity

Questionresp4 <- cyber.security.4.question.response

#### week 4 

cyber.security.4.archetype.survey.responses

na.omit(cyber.security.4.leaving.survey.responses)
str(cyber.security.4.leaving.survey.responses)

enrolments4 <- na.omit(cyber.security.4.enrolments)
enrolments4 <- cyber.security.4.enrolments  [(!(cyber.security.4.enrolments$gender  =="Unknown") & !(cyber.security.4.enrolments$age_range  =="Unknown")),]

Questionresp4 <- cyber.security.4.question.response
activity4 <- cyber.security.4.step.activity

members4 <- na.omit(cyber.security.4.team.members)

video4 <- cyber.security.4.video.stats
responses4 <- cyber.security.4.weekly.sentiment.survey.responses
