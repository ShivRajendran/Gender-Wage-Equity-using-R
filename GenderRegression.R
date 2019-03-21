library(stargazer)
library(broom)
library(dplyr)

#load csv
setwd("-----------------------")
data=read.csv("------------.csv",stringsAsFactors=FALSE)

#create age groups because we want to minimize the amount of randomness due to having so many unique age groups
data$age_Group=0
data$age_Group=ifelse(data$Age<25,1,data$age_Group)
data$age_Group=ifelse(data$Age>=25 & data$Age<35,2,data$age_Group)
data$age_Group=ifelse(data$Age>=35 & data$Age<45,3,data$age_Group)
data$age_Group=ifelse(data$Age>=45 & data$Age<55,4,data$age_Group)
data$age_Group=ifelse(data$Age>=55,5,data$age_Group)

#log base for Pay gap to make it a more linear model
data$log_base <- log(data$Annual.Salary)

#setup dummy variable
data$male=ifelse(data$Gender=="Male",1,0)

#run simple squares lin regression model
#no controls first
model1=lm(log_base~male,data=data)

#controlling with age and tenure
model2=lm(log_base~male+Age+Tenure,data=data)

#controlling with job title and department
model3=lm(log_base~male+Age+Tenure+Job.Title.Description+Department.Description,data=data)

#only showing things we want to show here (cleanup final results)
x=attributes(model3$coefficients)$names
lst <- lapply(x, grepl, pattern=("Department.Description|Job.Title.Description"))
omitlist=x[grepl("Department.Description|Job.Title.Description",x)==TRUE]

#final html file created using stargazer
stargazer(model1,model2,model3,type = "html",out="Genderresults.htm", column.labels=c("Unadjusted","Partialy Adjusted for Age Tenure","Adjusted"),omit=omitlist )
