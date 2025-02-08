library(tidyverse)
library(dplyr)
library(readr)
library(stringr)
library(caret)
library(kernlab)
library(rpart.plot)
library(ggplot2)
library(e1071)

hrData <-read_csv(choose.files())
##Dataset and data dictionary can be found: 
#https://www.kaggle.com/datasets/rhuebner/human-resources-data-set?resource=download


summary(hrData)

##Creation of Variables----------------------------------------------------------------------------------------------------

##creation of Years of Service requires reformatting date of hire to that of date of termination, using the difftime fuction to get
#days between 2 dates, stripping the days out of the field, converting to numeric format from char, and then dividing by 365
hrData$DateofHire <- format(as.Date(hrData$DateofHire, format = "%m/%d/%Y"), "%Y-%m-%d")
hrData$DateofTermination <- format(as.Date(hrData$DateofTermination, format = "%m/%d/%Y"), "%Y-%m-%d")


##2020-10-31 chosen as that was the last dataset update. 
hrData$YearsofService <- if_else(
  hrData$Termd == 0, 
  difftime('2020-10-31',hrData$DateofHire, units = "days"),
  difftime(hrData$DateofTermination, hrData$DateofHire, units = "days"))

hrData$YearsofService <- as.numeric(str_replace(hrData$YearsofService, "days",""))/365
view(hrData)

##Creation of Days since last Perf Review
hrData$LastPerformanceReview_Date <- format(as.Date(hrData$LastPerformanceReview_Date, format = "%m/%d/%Y"), "%Y-%m-%d")
hrData$YearsSinceLastPerfRev <- if_else(
  hrData$Termd == 0, 
  difftime(Sys.Date(),hrData$DateofHire, units = "days"),
  difftime(hrData$DateofTermination, hrData$LastPerformanceReview_Date, units = "days"))
hrData$YearsSinceLastPerfRev<- difftime('2020-10-31', hrData$LastPerformanceReview_Date, units = "days")
hrData$YearsSinceLastPerfRev <- as.numeric(str_replace(hrData$YearsSinceLastPerfRev, "days",""))/365

##creation of age
hrData$DOB <- format(as.Date(hrData$DOB, format = "%m/%d/%Y"), "%Y-%m-%d")
hrData$DOB <- as.Date(hrData$DOB)
hrData$Age <- as.numeric(str_replace(difftime(Sys.Date(), hrData$DOB, units = "days"),"days",""))/365

hrData$CostofAbsence <- (hrData$Salary/2080)*8*hrData$Absences

hrData$GenderID <- as.factor(hrData$GenderID)
hrData$MarriedID <- as.factor(hrData$MarriedID)
hrData<- hrData%>% mutate(PerfScoreID = case_when(
  PerformanceScore =='PIP'~'1',
  PerformanceScore =='Needs Improvement'~'2',
  PerformanceScore =='Fully Meets'~'3',
  PerformanceScore =='Exceeds'~'4'
)
)
hrData$PerfScoreID <- as.numeric(hrData$PerfScoreID)
view(hrData)
str(hrData)

##Summary Information--------------------------------------------------------------------------------------------------------
summary(hrData)
sum(hrData$CostofAbsence)
hrData%>%group_by(Termd)%>%summarise(n())
hrData%>% group_by(Department)%>% summarise(n())
hrData%>%group_by(Department)%>%summarise(sum(Termd))
hrData%>%group_by(Sex, Termd)%>%summarise(n())
hrData%>%summarise(sum(Absences))
hrData%>%group_by(Department)%>%summarise(sum(CostofAbsence))
hrData%>%group_by(Department)%>%summarise(mean(EmpSatisfaction))
abs_dep_sex_term<- hrData%>%group_by(Department, Sex, Termd )%>%summarise(Sum_abs = sum(Absences))
abs_dep_sex_term <- abs_dep_sex_term[order(abs_dep_sex_term$Sum_abs, decreasing = TRUE),]
abs_dep_sex_term

mean(hrData$YearsofService)
summary(hrData$YearsSinceLastPerfRev)
##ggplots---------------------------------------------------------------------------------------------------------------------
hrData%>%filter(Termd == 1)%>%group_by(YOS = YearsofService, Termd = as.factor(Termd))%>%summarise(n())%>% ggplot(aes(YOS)) + geom_histogram()+labs(title = 'Termination Counts by YOS')

hrData%>%filter(TermReason !='N/A-StillEmployed')%>% ggplot(aes(TermReason)) +geom_bar() +theme(axis.text.x=element_text(angle=90, hjust=1))

view(hrData %>% group_by(Position, Termd) %>% summarise(n(),sum(Absences), mean(Absences), mean(YearsofService)))

#col chart of cost of absences by department 
ggplot(hrData, aes(x=Department, y=CostofAbsence,fill = Department, label = CostofAbsence)) + geom_col() + scale_y_continuous(labels = scales::dollar_format()) +labs(title = 'Cost of Absence by Department') 

#col chart of cost of absences by position within the production department
hrDataProduction <- filter(hrData, hrData$Department == "Production")
ggplot(hrDataProduction) + geom_col(aes(x = Position, y=CostofAbsence)) + coord_flip()+scale_y_continuous(labels = scales::dollar_format()) +labs(title = 'Cost of Absence by Production Department Position')

#col chart of cost of absences by position within the production department by gender
Plotvar1<- hrData %>% filter(Department == "Production") %>% group_by(Position, Sex) %>% summarise(CostofAbsence = sum(CostofAbsence)) 
ggplot(Plotvar1, aes(x=Position, y = CostofAbsence, color = Sex, fill = Sex)) +geom_col(position = 'stack') + coord_flip()+scale_y_continuous(labels = scales::dollar_format())+ labs(title = 'Cost of Position absences in the Production Department by Gender')

#count plot of employment status
ggplot(hrData) + geom_count(aes(x = EmploymentStatus, y=Absences))

#boxplot of employee satisfaction absences
ggplot(hrData) + geom_count(aes(x = as.factor(EmpSatisfaction), y = Absences))
#boxplot of marital status absences
ggplot(hrData) + geom_count(aes(x = as.factor(MaritalDesc), y = Absences))
#are happier employees paid more? - Yes, they tend to be
ggplot(hrData) + geom_boxplot(aes(x = as.factor(EmpSatisfaction), y=Salary))


view(hrData%>%group_by(as.factor(GenderID))%>% summarise(IQR(Salary)))


hrData %>% group_by(Absences = as.factor(Absences)) %>% summarise(AVG_YOS = mean(YearsofService)) %>% ggplot(aes(Absences, AVG_YOS)) + geom_col()
  
#column chart of absences of gender of employees by manager name
ggplot(hrData, aes(x=ManagerName, y=Absences, fill=Sex)) + geom_col() + coord_flip()

#Mean absences by binned salary
ggplot(hrData,aes(x = Salary, y=Absences, color = Sex)) + geom_point(stat="summary", fun = 'mean')+scale_x_binned(n.breaks = 25,labels = scales::dollar_format())+theme(axis.text.x=element_text(angle=90, hjust=1)) +geom_smooth(method = 'lm')

#bar chart count of employee sex
ggplot(hrData, aes(as.factor(x=Sex))) + geom_bar()

#col chart of absences by sex
ggplot(hrData) + geom_col(aes(x=Sex, y=Absences)) + labs(title = 'Absences by Sex')

#looking at avg years of service to see if it was a recruiting issue - it wasnt
hrData %>% group_by(RecruitmentSource) %>% summarise(AVG_YOS = mean(YearsofService)) %>% ggplot(aes(RecruitmentSource, AVG_YOS)) +
geom_col()

#Looking to see if there were any departments with lower years of service avergaes - there werent
hrData %>% group_by(Department) %>% summarise(AVG_YOS = mean(YearsofService)) %>% ggplot(aes(Department, AVG_YOS)) +geom_col()

view(hrData%>%group_by(as.factor(GenderID))%>% summarise(IQR(Salary)))

ggplot(hrData, aes(x=YearsofService, y=Absences, fill = Department)) + geom_col() + scale_x_binned(n.breaks = 15)
ggplot(hrData, aes(x=YearsofService, y=Absences, fill = as.factor(Termd))) + geom_col() + scale_x_binned(n.breaks = 15)
ggplot(hrData, aes(x=YearsofService, y=Absences, fill = Sex)) + geom_col() + scale_x_binned(n.breaks = 15)


ggplot(hrData, aes(x=Department, y=SpecialProjectsCount))+geom_col()
hrData%>%group_by(Department)%>% summarise(sum(SpecialProjectsCount), sum(Absences))

##SVM----------------------------------------------------------------------------------------------------------------------------
set.seed(400)
HRDataModel <- data.frame(
  Termd = factor(hrData$Termd),
 Salary = hrData$Salary, 
 # Engagement_Survey = hrData$EngagementSurvey,
  #Emp_Satisfaction = hrData$EmpSatisfaction,
  #Special_Projects = hrData$SpecialProjectsCount,
  YearsSinceLastPerfRev = hrData$YearsSinceLastPerfRev
  #Absences = hrData$Absences,
  #Tardy30 = hrData$DaysLateLast30,
 
  )
trainTermd <- createDataPartition(y=hrData$Termd, p=.6, list = FALSE)
trainsetTermd <- HRDataModel[trainTermd,]
testsetTermd<- HRDataModel[-trainTermd,]


svm.model <- train(Termd~., data = trainsetTermd,
                   method = "svmLinear",
                   trcontrol=trainControl(method = 'none'),
                   preProcess=c("center","scale"))

svm.model$finalModel

svmPrediction <- predict(svm.model, testsetTermd, type = "raw")

x <- table(testsetTermd$Termd, svmPrediction)

confusionMatrix(x)

cbind(testsetTermd$Termd, svmPrediction)

varImp(svm.model)
svm.model$results


##Linear Regression--------------------------------------------------------------------------------------------------------------
HRDataModelLM1 <- data.frame(
  Salary = hrData$Salary, 
  Engagement_Survey = hrData$EngagementSurvey,
  Emp_Satisfaction = hrData$EmpSatisfaction,
  Special_Projects = hrData$SpecialProjectsCount,
  YOS = hrData$YearsofService,
  YearsSinceLastPerfRev = hrData$YearsSinceLastPerfRev,
  Absences = hrData$Absences,
  Tardy30 = hrData$DaysLateLast30,
  Gender = as.factor(hrData$GenderID),
  Marital_Status = as.factor(hrData$MarriedID),
  Performance_score = hrData$PerfScoreID
  )
lmYOS1 <- lm(YOS~., data = HRDataModelLM1)
summary(lmYOS1)

HRDataModelLM2 <- data.frame(
  Salary = hrData$Salary, 
  Engagement_Survey = hrData$EngagementSurvey,
  Emp_Satisfaction = hrData$EmpSatisfaction,
  Special_Projects = hrData$SpecialProjectsCount,
  YearsSinceLastPerfRev = hrData$YearsSinceLastPerfRev,
  Absences = hrData$Absences,
  Tardy30 = hrData$DaysLateLast30,
  YOS = hrData$YearsofService
  )
lmYOS2 <- lm(YOS~., data = HRDataModelLM2)
summary(lmYOS2)


HRDataModelLM3 <- data.frame(
  Special_Projects = hrData$SpecialProjectsCount,
  YearsSinceLastPerfRev = hrData$YearsSinceLastPerfRev,
  YOS = hrData$YearsofService
  )
lmYOS3 <- lm(YOS~., data = HRDataModelLM3)
summary(lmYOS3)




