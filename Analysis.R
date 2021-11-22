#install.packages("ProjectTemplate")
library("ProjectTemplate")
setwd("~/R/CSCFilProject")
load.project()

head(df2)
nrow(df2)
head(df3$learner_id)
nrow(df3)

library(dplyr)
glimpse(df2)
glimpse(df3)

## join the enrolments per week 
data = bind_rows(df2,df3)
## join the activities per week
activity =bind_rows(activity2,activity3)
class(activity)
table(activity$learner_id)

## inner join the enrolmets and activities
DF_innerjoin = merge(x=data,y=activity,by="learner_id")
## to check the activituy for a specific learner
DF_innerjoin[DF_innerjoin$learner_id %in%  "feb41f66-257c-4839-a209-dd4e0f75ecf5" ,] 

## site for plotd : https://sites.harding.edu/fmccown/r/

## 1 show enrollments per week
# Define the cars vector with 5 values
enrloments  <- c(nrow(df2), nrow(df3), 6, 4, 9)
# Graph cars using blue points overlayed by a line 
plot(enrloments, type="o", col="blue")
# Create a title with a red, bold/italic font
title(main="enrloments", col.main="red", font.main=4)

library(ggplot2)
library( dplyr )

data <- data  [(!(data$gender  =="Unknown") & !(data$age_range  =="Unknown")),]
dfg <- data %>%
  count(age= data$age_range,gender=data$gender, sort = TRUE) 

## 5  graph of gender vs highest_education_level
ggplot(data=dfg) + geom_point(aes(x=age  , y=n  ,colour=gender, size = 3  )) +
  geom_text(aes(x=age,label = n,y=n), size = 3)  +
  labs(title = "Enrolments regarding age/gender",
       x = "Age-Range",
       y = "Number of Ocurances")

##Enrolments regarding age/gender
ggplot(dfg ,aes(x = age, y = n   , fill = gender))+
  geom_bar(stat="identity")+
  geom_text(aes(label = n,y=n), size = 3) +
  labs(title = "Enrolments regarding age/gender",
       x = "Age-Range",
       y = "Number of Ocurances")

##Enrolments By Year
dfinserttions  <- data %>%
  count(Enrolemts=  format(as.POSIXct(as.Date(data$enrolled_at ),format = "%m/%d/%Y %H:%M:%S") , format = "%Y") , sort = TRUE) 

ggplot(dfinserttions ,aes(x = Enrolemts, y = n   , fill = Enrolemts   ))+
  geom_bar(stat="identity")+
  geom_text(aes(label = n,y=n), size = 3) +
  labs(title = "Enrolments By Year",
       x = "Year",
       y = "Number of Enrolments")


##  regarding the steps activity
dfsteps <- activity %>%
  count(stepcount= activity$step , sort = TRUE) 

ggplot(dfsteps ,aes(x = stepcount     , y = n   , fill = stepcount        ))+
  geom_bar(stat="identity")+
  geom_text(aes(label = n,y=n), size = 3) +
  labs(title = "Step Activity",
       x = "Steps",
       y = "Number of trials")

activity <- activity  [(!(activity$last_completed_at  =="")  ),]
dfsteps <- activity %>%
  count(stepcount= activity$step , sort = TRUE) 

ggplot(dfsteps ,aes(x = stepcount     , y = n   , fill = stepcount    ,colour=  stepcount    ))+
  geom_bar(stat="identity")+
  geom_text(aes(label = n,y=n), size = 3) +
  labs(title = "Step Activity",
       x = "Steps",
       y = "Number of succsful trials")

##   2 graph of enrolled_at vs gender

ggplot(data=data) + geom_point(aes(x=format(data$enrolled_at, format = "%Y"), y=gender,colour=age_range ))

## 3. grapf of age_range vs gender in enrolmegts

ggplot(data=data) + geom_point(aes(x=age_range, y=gender,colour=age_range))


##graph of age_range vs highest_education_level
dfageeducation <- data %>%
count(age= data$age_range,education=data$highest_education_level, sort = TRUE) 

ggplot(data=dfageeducation) + geom_point(aes(x=age , y=education  ,colour=age, size = 4 ))+
  geom_text(aes(x=age,label = n,y=education), size = 3)+
  labs(title = "Education Per Age",
       x = "Age-Range",
       y = "highest_education_level")

##ggplot(data=data) + geom_point(aes(x=age_range , y=highest_education_level  ,colour=age_range ))

##Education per age by gender
dfageeducation1 <- data %>%
  count(gender= data$gender,education=data$highest_education_level, sort = TRUE) 
ggplot(data=dfageeducation1) + geom_point(aes(x=gender , y=education  ,colour=gender, size = 4 ))+
  geom_text(aes(x=gender,label = n,y=education), size = 3)+
  labs(title = "Education Per Age",
       x = "Gender",
       y = "highest_education_level")

## 6   grpah of gender vs employment_area
ggplot(data=data) + geom_point(aes(x=gender  , y=employment_area  ,colour=gender  ))
#total <- table(data$country)


################################################# 
#####RUN 4
################################################
###str_sub(Questionresp4$quiz_question, 1, 3)
head(Questionresp4)

###Weekly results 
dfQuestionresp4 <- Questionresp4 %>%
  count(week= str_sub(Questionresp4$week_number, 1, 3 ),result=Questionresp4$correct, sort = TRUE) 

ggplot(data=dfQuestionresp4) + geom_point(aes(x=week  , y=n  ,colour=result, size = 4 ))+
  geom_text(aes(x=week ,label = n,y=n    ), size = 3)+
  labs(title = "Weekly results",
       x = "Number of Week's",
       y = "Results")

###Multiple_Choice Results
table(Questionresp4$quiz_question,Questionresp4$question_number)

dfQuestionresp4 <- Questionresp4 %>%
  count(submitted= str_sub(Questionresp4$question_number, 1, 9 ),result=Questionresp4$correct, sort = TRUE) 

  ggplot(data=dfQuestionresp4) + geom_point(aes(x=submitted  , y=n  ,colour=result, size = 4 ))+
    geom_text(aes(x=submitted ,label = n,y=n    ), size = 3)+
    labs(title = "Multiple_Choice Results",
         x = "Question_number",
         y = "Correct")
  
###Quastion/Response of quiz
dfQuestionresp4 <- Questionresp4 %>%
    count(quiz= str_sub(Questionresp4$quiz_question, 1, 3),response=Questionresp4$correct, sort = TRUE) 

ggplot(data=dfQuestionresp4) + geom_point(aes(x=quiz  , y=n  ,colour=response, size = 4 ))+
  geom_text(aes(x=quiz ,label = n,y=n    ), size = 3)+
  labs(title = "Quastion/Response of quiz",
       x = "Quiz-Range",
       y = "Number of Trialsl")



format(data$enrolled_at, format = "%Y")

 
#install.packages("tidyverse")

#plot(table(data$country))

## https://stackoverflow.com/questions/46355263/creating-bar-plot-in-ggplot2-depicting-count-of-particular-value-in-multiple-col
 plot(table (activity$step ))

 plot(table (activity$step, activity$week_number ))
 
 ## https://stackoverflow.com/questions/29034863/apply-a-ggplot-function-per-group-with-dplyr-and-set-title-per-group
 ##https://cran.r-project.org/web/packages/dplyr/vignettes/grouping.html
 ## plot(table (activity$step, activity$learner_id                             ))
 
 
 ###############################################################################
 #############################   4 week ########################################
 ###############################################################################
 #install.packages("tidyverse")
 cyber.security.4.archetype.survey.responses
 
 leaving4 <- na.omit(cyber.security.4.leaving.survey.responses)
 str(cyber.security.4.leaving.survey.responses)
 
 enrolments4 <- na.omit(cyber.security.4.enrolments)
 enrolments4 <- cyber.security.4.enrolments  [(!(cyber.security.4.enrolments$gender  =="Unknown") & !(cyber.security.4.enrolments$age_range  =="Unknown")),]
 
 Questionresp4 <- cyber.security.4.question.response
 activity4 <- cyber.security.4.step.activity
 
 members4 <- na.omit(cyber.security.4.team.members)
 
 video4 <- cyber.security.4.video.stats
 responses4 <- cyber.security.4.weekly.sentiment.survey.responses
 
 ###  1 graph  regarding th ereason why they leaving the course 
 
 #table (leaving4$leaving_reason   )
 #table (leaving4$leaving_reason ,  )
 
 dfleavingReason  <- leaving4 %>%
   count(LeftYear=  format(as.POSIXct(as.Date(leaving4$left_at ),format = "%m/%d/%Y %H:%M:%S") , format = "%Y")  , reason =leaving4$leaving_reason , sort = TRUE)
 
 ggplot(dfleavingReason ,aes(x = LeftYear, y = reason  , fill = LeftYear , colour= LeftYear,size = 3   ))+
   #  geom_bar(stat="identity")+
   geom_text(aes(x=LeftYear,label = n,y=reason), size = 3) +
   labs(title = "Leaving Reason from Course",
        x = "Reason",
        y = "Year Left")
 
 #####  2  enrolments4
 
 dfenrolments4 <- enrolments4 %>%
   count(age= enrolments4$age_range,gender=enrolments4$gender, sort = TRUE) 
 
 ## 5  graph of gender vs highest_education_level
 ggplot(data=dfenrolments4) + geom_point(aes(x=age  , y=n  ,colour=gender, size = 3  )) +
   geom_text(aes(x=age,label = n,y=n), size = 3)  +
   labs(title = "Enrolments regarding age/gender",
        x = "Age-Range",
        y = "Number of Ocurances")
 
 
 ################  Questionresp4
 dfQuestionresp4 <- Questionresp4 %>%
   count(quiz= str_sub(Questionresp4$quiz_question, 1, 3),response=Questionresp4$correct, sort = TRUE) 
 
 ggplot(data=dfQuestionresp4) + geom_point(aes(x=quiz  , y=n  ,colour=response, size = 4 ))+
   geom_text(aes(x=quiz ,label = n,y=n    ), size = 3)+
   labs(title = "Quastion/Response of quiz",
        x = "Quiz-Range",
        y = "Number of Trialsl")
 
 ##############   
 
 table(activity4$step)
 table(activity4$step_number )
 activity4 <- cyber.security.4.step.activity
 
 dfsteps4 <- activity4 %>%
   count(stepcount= str_sub(activity4$step,1,1) , sort = TRUE) 
 
 #dfsteps4 <- activity4 %>%
 #   count(stepcount=  activity4$step_number  , sort = TRUE) 
 
 ggplot(dfsteps4 ,aes(x = stepcount     , y = n   , fill = stepcount        ))+
   geom_bar(stat="identity")+
   geom_text(aes(label = n,y=n), size = 3) +
   labs(title = "Step Activity",
        x = "Steps",
        y = "Number of trials")
 
 
 
 
 ###############################
 ##more investigation
 cyber.security.4.enrolments  [ cyber.security.4.enrolments$learner_id  == "091df104-705f-4ee2-a67b-9d90043c4f56",]
 
 cyber.security.4.step.activity  [ cyber.security.4.step.activity$learner_id  == "091df104-705f-4ee2-a67b-9d90043c4f56",]
 
 cyber.security.4.question.response  [ cyber.security.4.question.response$learner_id  == "091df104-705f-4ee2-a67b-9d90043c4f56",]
 
 
 cyber.security.4.weekly.sentiment.survey.responses  [ cyber.security.4.weekly.sentiment.survey.responses$learner_id  == "091df104-705f-4ee2-a67b-9d90043c4f56",]
 
 
 
