##########################################################################################
# Code Flow
# 00 - Inititialize and load libraries
# 01 - Load and prepare data
#  01.1 - 01.7 - load all the csv's one by one and do basic EDA
#  01.8 - 01.9 - final data integrity cross check across datasets and data merge to get a master dataset
#  01.10 - WOE replacement for missing values
#  01.11 - Binning continous variables - this automatically takes care of outlier treatment
#  01.13 - Dummy variable creation for categorical variables
##########################################################################################

##########################################################################################
#
#00-Prep/Initiation
#
##########################################################################################

#Ensure you set correct working directory using setwd
#and install all required packages below before you run the code.

########################################
#setwd("C:/08_PGDDS/Predictive_Modelling-01/Graded_Project")
####################################

#loading libraries
library(MASS)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(dplyr)
library(ROCR)
library(stringr)
library(tidyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(robustbase)
library(magrittr)
library(InformationValue)
library(broom)
library(corrplot)

##########################################################################################
#
#01-Load & Prep Data
#
##########################################################################################

#1.1.loading data
employee_survey_data<-read.csv(file='employee_survey_data.csv',stringsAsFactors = F,check.names = F)
general_data<-read.csv(file='general_data.csv',stringsAsFactors = F,check.names = F)
in_time<-read.csv(file='in_time.csv',stringsAsFactors = F,check.names = F)
manager_survey_data<-read.csv(file='manager_survey_data.csv',stringsAsFactors = F,check.names = F)
out_time<-read.csv(file='out_time.csv',stringsAsFactors = F,check.names = F)

#1.2.employee_survey_data - data prep
str(employee_survey_data)
nrow(employee_survey_data)#4410
length(unique(employee_survey_data$EmployeeID))#4410 All values are unique so no de-duplication investigation is required here
sum(is.na(employee_survey_data))#83
sapply(employee_survey_data, function(x) sum(is.na(x)))
# EmployeeID EnvironmentSatisfaction         JobSatisfaction 
# 0                      25                      20 
# WorkLifeBalance 
# 38 

#we will use WOE to replace the NA values. Code for the same is from line number xxx to yyy

#1.3.in_time - data prep
str(in_time)
length(names(in_time))#262 columns
nrow(in_time)#4410
summary(in_time)
#Summary shows that there are some columns which have all NA values.
#The days which are holidays have NAs for all the employees. Since this is not adding any value to the dataset, we will remove holidays
empty_cols<-as.vector(names(which(map(in_time, ~sum(is.na(.)))==nrow(in_time))))
in_time[,empty_cols]<-NULL
length(names(in_time))#12 columns are removed
#First column has no name.View(in_time) shows that it is employee id, so let's rename the column correctly
colnames(in_time)[1]<-"EmployeeID"
length(unique(in_time$EmployeeID))#4410 all unique 

#1.4.out_time - data prep
str(out_time)
length(names(out_time))#262 columns
nrow(out_time)
summary(out_time)
#Summary shows that there are some columns which have all NA values.
#The days which are holidays have NAs for all the employees. Since this is not adding any value to the dataset, we will remove holidays
empty_cols<-as.vector(names(which(map(out_time, ~sum(is.na(.)))==nrow(out_time))))
out_time[,empty_cols]<-NULL
length(names(out_time))#12 columns are removed
#First column has no name.View(out_time) shows that it is employee id, so let's rename the column correctly
colnames(out_time)[1]<-"EmployeeID"
length(unique(out_time$EmployeeID))#4410 all unique 
#Check if there are any descrepencies in NA values.eg. in-time has a valid entry but out-time is NA or vice-versa
setdiff(which(is.na(in_time)),which(is.na(out_time)))
#There's no descrepency in NA records and the time entries which have NA are leaves for the employees.
#Check if there are any mismatches in dates for in/out data
setdiff(names(in_time),names(out_time))
#Both datasets have data for same dates



#1.5.manager_survey_data - data prep
summary(manager_survey_data)#No NA records.
length(names(manager_survey_data))#3 columns
nrow(manager_survey_data)#4410
length(unique(manager_survey_data$EmployeeID))#all unique

#1.6.general_data - data prep
summary(general_data)
length(names(general_data))#24 columns
nrow(general_data)#4410
length(unique(general_data$EmployeeID))#all unique
#check for na's in general_data 
sum(is.na(general_data))# 28 na values
#column level NA analysis
sapply(general_data, function(x) sum(is.na(x)))
#NumCompaniesWorked 19 and TotalWorkingYears 9 have na .lets check these rows

#NumCompaniesWorked
general_data[which(is.na(general_data$NumCompaniesWorked)==1),]
#for the folks who have same  TotalWorkingYears and YearsAtCompany, this is the first and only company
general_data[which(is.na(general_data$NumCompaniesWorked)==1 & general_data$TotalWorkingYears==general_data$YearsAtCompany),c("NumCompaniesWorked")]<-0

#for those TotalWorkingYears are not same as YearsAtCompany, this is not the first company, no other column is suggesting/representing any corelation
# we will use woe recomedation to fill these values as can be seen between lines xxx and yyy
#TotalWorkingYears
general_data[which(is.na(general_data$TotalWorkingYears)==1),]

#similarly if number of company works is 1 then TotalWorkingYears is equal to YearsAtCompany
general_data[which(is.na(general_data$TotalWorkingYears)==1 & general_data$NumCompaniesWorked==1 ),c("TotalWorkingYears","YearsAtCompany")]
general_data[which(is.na(general_data$TotalWorkingYears)==1 & general_data$NumCompaniesWorked==1 ),c("TotalWorkingYears")]<-general_data[which(is.na(general_data$TotalWorkingYears)==1 & general_data$NumCompaniesWorked==1 ),c("YearsAtCompany")]

#TotalWorking Years can be at equal to YearsAtCompany at best but NumCompaniesWorked values being greater than one makes it a bad assumtion
#we will use maximum of woe recomendation for YearsAtCompany as can be seen between lines xxx and yyy
#EmployeeCount, StandardHours and Over18 have only singe value and they won't help in prediction hence removing them
del_col<-c("EmployeeCount", "StandardHours" , "Over18")
general_data<-general_data[,!names(general_data) %in% del_col]

#1.7.Create new derived variables from time-entry raw data
#coverting the datatype to timestamp
in_time.mod<-as.data.frame(lapply(in_time[,-1], strptime, format="%Y-%m-%d %H:%M:%S"))

#coverting the datatype to timestamp
out_time.mod<-as.data.frame(lapply(out_time[,-1], strptime, format="%Y-%m-%d %H:%M:%S"))

#We can calculate time that each employee is in office every day and find out mean/median workhour values and leaves taken for each employee
in_offfice_time <- data.frame(matrix(nrow=nrow(in_time.mod), ncol=ncol(in_time.mod)))                                
for (i in 1:(ncol(in_time.mod))) {
  
  in_offfice_time[,i]<-as.duration(in_time.mod[,i]   %--% out_time.mod[,i]) / dhours(1)
  
}
summary(in_offfice_time)

#Lets find out the average time an employee spends at work per day. Lets also calculate the median in case long work hours are present as outliers
dm<-data.matrix(in_offfice_time, rownames.force = NA)
median_office_time<-as.vector(rowMedians(dm,na.rm = TRUE))
mean_office_time<-as.vector(rowMeans(dm,na.rm = TRUE))

#Lets see how many leaves each employee took
emp_leaves <- apply(is.na(in_offfice_time), 1, sum)
emp_office_presence<-cbind(out_time$EmployeeID,mean_office_time,median_office_time,emp_leaves)

#we calculated mean & median for in and out time as well, found them to be in significant and hence removed them from, the script
#Assigning correct name to the first column
colnames(emp_office_presence)[1]<-"EmployeeID"
#Convert to data frame for merge
emp_office_presence<-as.data.frame(emp_office_presence)

#1.8.Check if all empids match
setdiff(employee_survey_data$EmployeeID,general_data$EmployeeID)
setdiff(general_data$EmployeeID,in_time$EmployeeID)
setdiff(emp_office_presence$EmployeeID,general_data$EmployeeID)
setdiff(general_data$EmployeeID,manager_survey_data$EmployeeID)
#All employee ids in all data sets match.

#1.9 Merging all datasets into one 
master_data <- merge(employee_survey_data,emp_office_presence,by="EmployeeID", all = F)
master_data <- merge(master_data,general_data,by="EmployeeID", all = F)
master_data <- merge(master_data,manager_survey_data,by="EmployeeID", all = F)

#1.10 Using WOE to treat missing values for the below columns which have NA
#  1.JobSatisfaction 2.WorkLifeBalance 3.EnvironmentSatisfaction 4.NumCompaniesWorked 5.TotalWorkingYears

#Changing Attrition column to logical for woe analysis
master_data$Attrition<-factor(master_data$Attrition)
str(master_data$Attrition)
levels(master_data$Attrition)<-c(F,T)
str(master_data$Attrition)
master_data$Attrition<-as.logical(master_data$Attrition)

# 1.10.1.JobSatisfaction
WOETable(X=factor(ifelse(is.na(master_data$JobSatisfaction),5,master_data$JobSatisfaction)), Y=master_data$Attrition)
# 5(dummy for na) has very negative value of -1.29529362 (19 churns of 20)
# We will use CAT 4 with WOE of -0.40020037 for replacement since this is closest to CAT 5 woe above 
master_data$JobSatisfaction<-ifelse(is.na(master_data$JobSatisfaction),4,master_data$JobSatisfaction)
WOETable(X=factor(ifelse(is.na(master_data$JobSatisfaction),5,master_data$JobSatisfaction)), Y=master_data$Attrition)
#As we can see, 4's woe is unaffected by the addition at -0.40937828

# 1.10.2.WorkLifeBalance 
WOETable(X=factor(ifelse(is.na(master_data$WorkLifeBalance),5,master_data$WorkLifeBalance)), Y=master_data$Attrition)
#5(dummy for na) has very negative -0.49092080 (34 churns of 38)
#closest to 3 woe of -0.1426141 although higher
master_data$WorkLifeBalance<-ifelse(is.na(master_data$WorkLifeBalance),3,master_data$WorkLifeBalance)
WOETable(X=factor(ifelse(is.na(master_data$WorkLifeBalance),5,master_data$WorkLifeBalance)), Y=master_data$Attrition)
#3 woe is un affected at -0.14694499

# 1.10.3.EnvironmentSatisfaction 
WOETable(X=factor(ifelse(is.na(master_data$EnvironmentSatisfaction),5,master_data$EnvironmentSatisfaction)), Y=master_data$Attrition)
#5(dummy for na) has a positive 0.26285100 with (20 churns of 25)
#closet to cat 1 with woe 0.56154813 with dif of .3 
#.3 in teh neative direction reaches till .04 which is still .04 away from -.08 of cat 2
master_data$EnvironmentSatisfaction<-ifelse(is.na(master_data$EnvironmentSatisfaction),1,master_data$EnvironmentSatisfaction)
WOETable(X=factor(ifelse(is.na(master_data$EnvironmentSatisfaction),5,master_data$EnvironmentSatisfaction)), Y=master_data$Attrition)
#impact on 1 woe is marginal with it being 0.55359586

# 1.10.4.NumCompaniesWorked 
#NumCompaniesWorked as a factor for replacement calculation
WOETable(X=factor(ifelse(is.na(master_data$NumCompaniesWorked),11,master_data$NumCompaniesWorked)), Y=master_data$Attrition)
#11(dummy for na) has a positive 0.3498624 with (11 churns of 14)
#it is closet to cat 7 with woe of 0.4393074
master_data$NumCompaniesWorked<-ifelse(is.na(master_data$NumCompaniesWorked),7,master_data$NumCompaniesWorked)
WOETable(X=factor(ifelse(is.na(master_data$NumCompaniesWorked),11,master_data$NumCompaniesWorked)), Y=master_data$Attrition)
#minimal impact on WOE of 7 with  0.4341227

# 1.10.5.TotalWorkingYears
#TotalWorkingYears as a factor for replacement calculation
WOETable(X=factor(ifelse(is.na(master_data$TotalWorkingYears),41,master_data$TotalWorkingYears)), Y=master_data$Attrition)
#41(dummy for na) has a negative -0.32190746 with (6 churns of 7)
# it is exactly matching with 33 with woe -0.32190746 and 18 churns out of 21
#lets see if 33 as TotalWorkingYears makes sense for this data
master_data[which(is.na(master_data$TotalWorkingYears)==1),]
# preoplwe with 0 NumCompaniesWorked should get YearsAtCompany as TotalWorkingYears
master_data[which(is.na(master_data$TotalWorkingYears)==1 & master_data$NumCompaniesWorked==0),c("TotalWorkingYears")]<-master_data[which(is.na(master_data$TotalWorkingYears)==1 & master_data$NumCompaniesWorked==0),c("YearsAtCompany")]
#lets check woe after that change
WOETable(X=factor(ifelse(is.na(master_data$TotalWorkingYears),41,master_data$TotalWorkingYears)), Y=master_data$Attrition)
#41(dummy for na) has a positive 0.08355765 with (4 churns of 5)
#close and exact matches
#11    21   85   106 0.024647887 0.0229419703  0.07172319 1.223538e-04
#34     3   12    15 0.003521127 0.0032388664  0.08355765 2.358501e-05
#7    54  189   243 0.063380282 0.0510121457  0.21708904 2.684987e-03
#3    27   99   126 0.031690141 0.0267206478  0.17056903 8.476416e-04
#4    36  153   189 0.042253521 0.0412955466  0.02293303 2.196926e-05
#lets see which one macthes w.r.t YearsAtCompany , NumCompaniesWorked and other attributes
master_data[which(is.na(master_data$TotalWorkingYears)==1), c("JobRole","JobLevel","MonthlyIncome","NumCompaniesWorked","YearsAtCompany")]
#folks with 7 NumCompaniesWorked and high salary can be tagged to 34 years of total exp
master_data[which(is.na(master_data$TotalWorkingYears)==1 & master_data$MonthlyIncome>50000), c("TotalWorkingYears")]<-34
#two other with 4 company swicted and  5 and 8 years with current firm respectively can be tagged to 11
#the last one although with higer number of companies worked for same salary level can also be tagged to 11
master_data[which(is.na(master_data$TotalWorkingYears)==1), c("TotalWorkingYears")]<-11

#Creating derived variables based on industry experience
#Income to expr ratio
#If an employees salary is low despite a considerable work experience, one might expect a churn
master_data$incomeToExpRatio<-(master_data$MonthlyIncome/ifelse(master_data$TotalWorkingYears==0,1,master_data$TotalWorkingYears))/100

#In India, many women tend to leave jobs post marriage or after kids. Combining a gender and marital status
master_data$genderWiseMaritalStatus<-paste(master_data$MaritalStatus,master_data$Gender,sep="_")

########Write master_data in CSV for Tableau plots
write.csv(master_data,file="master_data.csv")

#Let's create correlation table and plots
correl_num_cols<-names(which(sapply(master_data,is.numeric)))
correl_num_cols[length(correl_num_cols)+1]<-"Attrition" 

#Check correlation of price with engineering parameters of automibile
cortable<-cor(master_data[,correl_num_cols])
View(cortable)
#From correlation table,EnvironmentSatisfaction,JobSatisfaction,Age,
#TotalWorkingYears,YearsAtCompany,YearsWithCurrManager have negative correlation to Attrition
#mean_office_time,incomeToExpRatio have positive correlationship with Attrition

#The same observation can be visualzed using corrplot & heatmap
#Intall corrplot package if not already installed
#install.packages("corrplot")
#library(corrplot) - initiated above

corrplot(cortable, type = "upper", order = "hclust", tl.col = "black", tl.srt = 90)
col<- colorRampPalette(c("blue", "white", "pink"))(40)
heatmap(x = cortable, col = col, symm = TRUE)
#From the correlation plot, it is visible that
#1. YearsAtCompany and YearsWithCurrentManager are highly correlated
#2. YearsSinceLastPromotion and YearsAtCompany  are highly correlated
#3. YearsAtCompany  and TotalWorkingYears are  highly   correlated 
#4. Age and TotalWorkingYears  have good correlation
#Based on this, it could be inferred that there are some old timers that are with the same managers sticking around in the company 
#and could have stagnated.This could be be a reason for higher attrition for Young/Single population.

#Let's check categorical variables against attrition
#Business Travel
master_data %>%
  mutate(BusinessTravel = as.factor(BusinessTravel),Attrition = as.factor(Attrition)) %>%
  group_by(BusinessTravel, Attrition) %>%
  summarise(count_class = n()) %>%
  group_by(BusinessTravel) %>%
  mutate(count_man = sum(count_class)) %>%
  mutate(percent = count_class / count_man * 100) %>%
  ungroup() %>%
  ggplot(aes(x = BusinessTravel,y = count_class,group = Attrition)) +
  geom_bar(aes(fill = Attrition),stat = "identity") +
  geom_text(aes(label = sprintf("%0.1f%%", percent)),position = position_stack(vjust = 0.5))+ 
  labs(x="Business Travel",y="Number Of Employees", title="Plot - Business Travel & Attrition")+
  scale_fill_manual(values = c("#00bfff", "#de3163"))
#Those who travel rarely or frequently have higher attrition.Frequent travel seems to impact 
#attrition more with ~25%

#Department
master_data %>%
  mutate(Department = as.factor(Department),Attrition = as.factor(Attrition)) %>%
  group_by(Department, Attrition) %>%
  summarise(count_class = n()) %>%
  group_by(Department) %>%
  mutate(count_man = sum(count_class)) %>%
  mutate(percent = count_class / count_man * 100) %>%
  ungroup() %>%
  ggplot(aes(x = Department,y = count_class,group = Attrition)) +
  geom_bar(aes(fill = Attrition),stat = "identity") +
  geom_text(aes(label = sprintf("%0.1f%%", percent)),position = position_stack(vjust = 0.5))+ 
  labs(x="Department",y="Number Of Employees", title="Plot - Departmentwise Attrition")+
  scale_fill_manual(values = c("#00bfff", "#de3163"))
#R&D has highest attrition followed by sales in numbers and HR leads attrition in percentage~30.2%.

#EducationField
master_data %>%
  mutate(EducationField = as.factor(EducationField),Attrition = as.factor(Attrition)) %>%
  group_by(EducationField, Attrition) %>%
  summarise(count_class = n()) %>%
  group_by(EducationField) %>%
  mutate(count_man = sum(count_class)) %>%
  mutate(percent = count_class / count_man * 100) %>%
  ungroup() %>%
  ggplot(aes(x = EducationField,y = count_class,group = Attrition)) +
  geom_bar(aes(fill = Attrition),stat = "identity") +
  geom_text(aes(label = sprintf("%0.1f%%", percent)),position = position_stack(vjust = 0.5))+ 
  labs(x="Education Field",y="Number Of Employees", title="Plot - Education Field wise Attrition")+
  scale_fill_manual(values = c("#00bfff", "#de3163"))
#Employees with life sciences or medical education have higher attrition numbers 
#apart from marketing.From percentage perspective employees with HR degree have highest~41% attrition.
#This could indicate an issue with HR practices in the company as departments with highest
#employees and HR have highest attrition percentages


#Gender
master_data %>%
  mutate(Gender = as.factor(Gender),Attrition = as.factor(Attrition)) %>%
  group_by(Gender, Attrition) %>%
  summarise(count_class = n()) %>%
  group_by(Gender) %>%
  mutate(count_man = sum(count_class)) %>%
  mutate(percent = count_class / count_man * 100) %>%
  ungroup() %>%
  ggplot(aes(x = Gender,y = count_class,group = Attrition)) +
  geom_bar(aes(fill = Attrition),stat = "identity") +
  geom_text(aes(label = sprintf("%0.1f%%", percent)),position = position_stack(vjust = 0.5))+ 
  labs(x="Gender",y="Number Of Employees", title="Plot - Genderwise Attrition")+
  scale_fill_manual(values = c("#00bfff", "#de3163"))
#Male employees have higher attrition compared to female numbers as well as percentage

#JobRole
master_data %>%
  mutate(JobRole = as.factor(JobRole),Attrition = as.factor(Attrition)) %>%
  group_by(JobRole, Attrition) %>%
  summarise(count_class = n()) %>%
  group_by(JobRole) %>%
  mutate(count_man = sum(count_class)) %>%
  mutate(percent = count_class / count_man * 100) %>%
  ungroup() %>%
  ggplot(aes(x = JobRole,y = count_class,group = Attrition)) +
  geom_bar(aes(fill = Attrition),stat = "identity") +
  geom_text(aes(label = sprintf("%0.1f%%", percent)),position = position_stack(vjust = 0.5))+ 
  labs(x="Job Role",y="Number Of Employees", title="Plot - Job Role wise Attrition")+
  scale_fill_manual(values = c("#00bfff", "#de3163"))
#Sales Executive, Research Scientist and Lab technicians have highest attrition in numbers
#Research Scientists(18.2%) and Research Directors (23.8%) and Sales Exec(16.9) have highest attrition %
#This is not good sign since company is losing it's core research talent.

#MaritalStatus
master_data %>%
  mutate(MaritalStatus = as.factor(MaritalStatus),Attrition = as.factor(Attrition)) %>%
  group_by(MaritalStatus, Attrition) %>%
  summarise(count_class = n()) %>%
  group_by(MaritalStatus) %>%
  mutate(count_man = sum(count_class)) %>%
  mutate(percent = count_class / count_man * 100) %>%
  ungroup() %>%
  ggplot(aes(x = MaritalStatus,y = count_class,group = Attrition)) +
  geom_bar(aes(fill = Attrition),stat = "identity") +
  geom_text(aes(label = sprintf("%0.1f%%", percent)),position = position_stack(vjust = 0.5))+ 
  labs(x="Marital Status",y="Number Of Employees", title="Plot - Marital Status wise Attrition")+
  scale_fill_manual(values = c("#00bfff", "#de3163"))
#Single employees clearly seem to have higher attrition  numbers/percentage followed by Married employees.

#genderWiseMaritalStatus
master_data %>%
  mutate(genderWiseMaritalStatus = as.factor(genderWiseMaritalStatus),Attrition = as.factor(Attrition)) %>%
  group_by(genderWiseMaritalStatus, Attrition) %>%
  summarise(count_class = n()) %>%
  group_by(genderWiseMaritalStatus) %>%
  mutate(count_man = sum(count_class)) %>%
  mutate(percent = count_class / count_man * 100) %>%
  ungroup() %>%
  ggplot(aes(x = genderWiseMaritalStatus,y = count_class,group = Attrition)) +
  geom_bar(aes(fill = Attrition),stat = "identity") +
  geom_text(aes(label = sprintf("%0.1f%%", percent)),position = position_stack(vjust = 0.5))+ 
  labs(x="Genderwise Marital Status",y="Number Of Employees", title="Plot - Genderwise Marital Status wise Attrition")+
  scale_fill_manual(values = c("#00bfff", "#de3163"))
#Single male have highest attrition followed by Married male and single female in numbers
#Althoug Single female certainly have higher attrition percentage(23.4%) compared to married male.
#Overall single male/female employees seem to have higher attrition





######################################################
#lets work on continous variables
QuartileNHist <- function(data) 
{
  print(quantile(unlist(data),seq(0, 1, 0.01)))
  print(ggplot(data.frame(data),aes(x=data))+geom_histogram() + scale_x_continuous(breaks = round(seq(min(data), max(data),
                by = (max(data)-min(data))/30 ),1)) + theme(axis.text=element_text(size=8)))
  
}

#The below function gives the binning statement clause based on the binning values passed in the partition variable
mutatecol<- function(colname,partition)
{
  clause<- paste(" ",colname ," = case_when( ")
  for (i in 1:length(partition))  
  { 
    if (i==1)
    { clause<-paste(clause , " ( ",colname ," <= ", partition[i]," )~ ",i," , ")
    
    } else if (i>1 
               # & i!=length(partition)
    )
    {
      clause<-paste(clause, " ( ", colname ," > ", partition[i-1]," & ",colname ," <= ",partition[i]," ) ~ ",i," , ")
    }
  }
  clause<-paste(clause , " ( ", colname ," > ", partition[length(partition)]," )~ ",length(partition)+1," ,T~",length(partition)+2," ) ")
  
  print(clause)  
}


#looking at the pvalue and distinguisins is difficul with naked eye
library(xtable)
pvalue.based.sort<-function(model){
  idx <- order(coef(summary(model))[,4],decreasing=T)  # sort out the p-values
  out <- coef(summary(model))[idx,]       # reorder coef, SE, etc. by increasing p
  print(out)
  
}

#1.11.We will do binning  for continous variables. It will take care of outlier and scaling together
master_data_copy <- master_data
#creating a copy of the master file and adding binned columns to it

QuartileNHist(master_data$mean_office_time)
#based on the histogram and quartile values, let's bin the data
partition<-c(6.1,6.3,6.6,6.8,7.1,8,9,9.7,10,10.9)
mutatecol('mean_office_time',partition)
master_data_copy %<>% mutate(   mean_office_time  = case_when(   (  mean_office_time  <=  6.1  )~  1  ,   (  mean_office_time  >  6.1  &  mean_office_time  <=  6.3  ) ~  2  ,   (  mean_office_time  >  6.3  &  mean_office_time  <=  6.6  ) ~  3  ,   (  mean_office_time  >  6.6  &  mean_office_time  <=  6.8  ) ~  4  ,   (  mean_office_time  >  6.8  &  mean_office_time  <=  7.1  ) ~  5  ,   (  mean_office_time  >  7.1  &  mean_office_time  <=  8  ) ~  6  ,   (  mean_office_time  >  8  &  mean_office_time  <=  9  ) ~  7  ,   (  mean_office_time  >  9  &  mean_office_time  <=  9.7  ) ~  8  ,   (  mean_office_time  >  9.7  &  mean_office_time  <=  10  ) ~  9  ,   (  mean_office_time  >  10  &  mean_office_time  <=  10.9  ) ~  10  ,   (  mean_office_time  >  10.9  )~  11  ,T~ 12  ) )

QuartileNHist(master_data$median_office_time)
#based on the histogram and quartile values, let's bin the data
#breaking into only two halfs -- peopel who spend exact time and the ones that overwork
partition<-c(8)
mutatecol(median_office_time,partition)
master_data_copy %<>% mutate(  median_office_time  = case_when(   (  median_office_time  <=  8  )~  1  ,   (  median_office_time  >  8  )~  2  ,T~ 3  ) )

#lets check its relation with attrition value
#median_office_time
master_data_copy %>%
  mutate(median_office_time = as.factor(median_office_time),Attrition = as.factor(Attrition)) %>%
  group_by(median_office_time, Attrition) %>%
  summarise(count_class = n()) %>%
  group_by(median_office_time) %>%
  mutate(count_man = sum(count_class)) %>%
  mutate(percent = count_class / count_man * 100) %>%
  ungroup() %>%
  ggplot(aes(x = median_office_time,y = count_class,group = Attrition)) +
  geom_bar(aes(fill = Attrition),stat = "identity") +
  geom_text(aes(label = sprintf("%0.1f%%", percent)),position = position_stack(vjust = 0.5))+ 
  labs(x="median_office_time",y="Number Of Employees", title="Plot - Meadian Time Vs Attrition")+
  scale_fill_manual(values = c("#00bfff", "#de3163"))
#Employees who spend long hours in office seem to have almost 3 times(29%) attrition percentage compared to those who don't(10.7%)


QuartileNHist(master_data$emp_leaves)
#based on the histogram and quartile values, let's bin the data
partition<-c(3,4,5,9,13,17,21,22)
mutatecol('emp_leaves',partition)
master_data_copy %<>% mutate(emp_leaves  = case_when(   (  emp_leaves  <=  3  )~  1  ,   (  emp_leaves  >  3  &  emp_leaves  <=  4  ) ~  2  ,   (  emp_leaves  >  4  &  emp_leaves  <=  5  ) ~  3  ,   (  emp_leaves  >  5  &  emp_leaves  <=  9  ) ~  4  ,   (  emp_leaves  >  9  &  emp_leaves  <=  13  ) ~  5  ,   (  emp_leaves  >  13  &  emp_leaves  <=  17  ) ~  6  ,   (  emp_leaves  >  17  &  emp_leaves  <=  21  ) ~  7  ,   (  emp_leaves  >  21  &  emp_leaves  <=  22  ) ~  8  ,   (  emp_leaves  >  22  )~  9  ,T~ 10  ))

QuartileNHist(master_data$Age)
#based on the histogram and quartile values, let's bin the data
partition<-c(19,24,27,30,32,34,38,39,42,47,54)
mutatecol('Age',partition)
master_data_copy %<>% mutate( Age  = case_when(   (  Age  <=  19  )~  1  ,   (  Age  >  19  &  Age  <=  24  ) ~  2  ,   (  Age  >  24  &  Age  <=  27  ) ~  3  ,   (  Age  >  27  &  Age  <=  30  ) ~  4  ,   (  Age  >  30  &  Age  <=  32  ) ~  5  ,   (  Age  >  32  &  Age  <=  34  ) ~  6  ,   (  Age  >  34  &  Age  <=  38  ) ~  7  ,   (  Age  >  38  &  Age  <=  39  ) ~  8  ,   (  Age  >  39  &  Age  <=  42  ) ~  9  ,   (  Age  >  42  &  Age  <=  47  ) ~  10  ,   (  Age  >  47  &  Age  <=  54  ) ~  11  ,   (  Age  >  54  )~  12  ,T~ 13  ))

QuartileNHist(master_data$DistanceFromHome)
#based on the histogram and quartile values, let's bin the data
partition<-c(2,3,6.23,10)
mutatecol('DistanceFromHome',partition)
master_data_copy %<>% mutate(DistanceFromHome  = case_when(   (  DistanceFromHome  <=  2  )~  1  ,   (  DistanceFromHome  >  2  &  DistanceFromHome  <=  3  ) ~  2  ,   (  DistanceFromHome  >  3  &  DistanceFromHome  <=  6.23  ) ~  3  ,   (  DistanceFromHome  >  6.23  &  DistanceFromHome  <=  10  ) ~  4  ,   (  DistanceFromHome  >  10  )~  5  ,T~ 6  ))

QuartileNHist(master_data$MonthlyIncome)
#based on the histogram and quartile values, let's bin the data
partition<-c(16420,22750,29080,41740,54400,67060,93620.0,107250.0,137756.0,164130.0,186437.6,199990.0)
mutatecol('MonthlyIncome',partition)
master_data_copy %<>% mutate(MonthlyIncome  = case_when(   (  MonthlyIncome  <=  16420  )~  1  ,   (  MonthlyIncome  >  16420  &  MonthlyIncome  <=  22750  ) ~  2  ,   (  MonthlyIncome  >  22750  &  MonthlyIncome  <=  29080  ) ~  3  ,   (  MonthlyIncome  >  29080  &  MonthlyIncome  <=  41740  ) ~  4  ,   (  MonthlyIncome  >  41740  &  MonthlyIncome  <=  54400  ) ~  5  ,   (  MonthlyIncome  >  54400  &  MonthlyIncome  <=  67060  ) ~  6  ,   (  MonthlyIncome  >  67060  &  MonthlyIncome  <=  93620  ) ~  7  ,   (  MonthlyIncome  >  93620  &  MonthlyIncome  <=  107250  ) ~  8  ,   (  MonthlyIncome  >  107250  &  MonthlyIncome  <=  137756  ) ~  9  ,   (  MonthlyIncome  >  137756  &  MonthlyIncome  <=  164130  ) ~  10  ,   (  MonthlyIncome  >  164130  &  MonthlyIncome  <=  186437.6  ) ~  11  ,   (  MonthlyIncome  >  186437.6  &  MonthlyIncome  <=  199990  ) ~  12  ,   (  MonthlyIncome  >  199990  )~  13  ,T~ 14  ))

QuartileNHist(master_data$NumCompaniesWorked)
#we will keep this column as is since it's already binned like a discrete variable

QuartileNHist(master_data$PercentSalaryHike)
#we will keep this column as is since it's already binned like a discrete variable

QuartileNHist(master_data$TotalWorkingYears)
#based on the histogram and quartile values, let's bin the data
partition<-c(1,2,5,6,8,9,10,12,13,17,21,24,27,33)
mutatecol('TotalWorkingYears',partition)
master_data_copy %<>% mutate( TotalWorkingYears  = case_when(   (  TotalWorkingYears  <=  1  )~  1  ,   (  TotalWorkingYears  >  1  &  TotalWorkingYears  <=  2  ) ~  2  ,   (  TotalWorkingYears  >  2  &  TotalWorkingYears  <=  5  ) ~  3  ,   (  TotalWorkingYears  >  5  &  TotalWorkingYears  <=  6  ) ~  4  ,   (  TotalWorkingYears  >  6  &  TotalWorkingYears  <=  8  ) ~  5  ,   (  TotalWorkingYears  >  8  &  TotalWorkingYears  <=  9  ) ~  6  ,   (  TotalWorkingYears  >  9  &  TotalWorkingYears  <=  10  ) ~  7  ,   (  TotalWorkingYears  >  10  &  TotalWorkingYears  <=  12  ) ~  8  ,   (  TotalWorkingYears  >  12  &  TotalWorkingYears  <=  13  ) ~  9  ,   (  TotalWorkingYears  >  13  &  TotalWorkingYears  <=  17  ) ~  10  ,   (  TotalWorkingYears  >  17  &  TotalWorkingYears  <=  21  ) ~  11  ,   (  TotalWorkingYears  >  21  &  TotalWorkingYears  <=  24  ) ~  12  ,   (  TotalWorkingYears  >  24  &  TotalWorkingYears  <=  27  ) ~  13  ,   (  TotalWorkingYears  >  27  &  TotalWorkingYears  <=  33  ) ~  14  ,   (  TotalWorkingYears  >  33  )~  15  ,T~ 16  ) )

QuartileNHist(master_data$TrainingTimesLastYear)
#we will keep this column as is since it's already binned like a discrete variable

QuartileNHist(master_data$YearsAtCompany)
#we will keep this column as is since it's already binned like a discrete variable

QuartileNHist(master_data$YearsSinceLastPromotion)
#we will keep this column as is since it's already binned like a discrete variable

QuartileNHist(master_data$YearsWithCurrManager)
#we will keep this column as is since it's already binned like a discrete variable

QuartileNHist(master_data$incomeToExpRatio)
#based on the histogram and quartile values, let's bin the data
partition<-c(3.26,123.2,174.7,197.4,211,354,600)
mutatecol('incomeToExpRatio',partition)
master_data_copy %<>% mutate( incomeToExpRatio  = case_when(   (  incomeToExpRatio  <=  3.26  )~  1  ,   (  incomeToExpRatio  >  3.26  &  incomeToExpRatio  <=  123.2  ) ~  2  ,   (  incomeToExpRatio  >  123.2  &  incomeToExpRatio  <=  174.7  ) ~  3  ,   (  incomeToExpRatio  >  174.7  &  incomeToExpRatio  <=  197.4  ) ~  4  ,   (  incomeToExpRatio  >  197.4  &  incomeToExpRatio  <=  211  ) ~  5  ,   (  incomeToExpRatio  >  211  &  incomeToExpRatio  <=  354  ) ~  6  ,   (  incomeToExpRatio  >  354  &  incomeToExpRatio  <=  600  ) ~  7  ,   (  incomeToExpRatio  >  600  )~  8  ,T~ 9  ) )

str(master_data)
# After binning, we have 24 numeric type variables and 7 character type variable

Cate.Var.Conv.2level<- function(colname)
{
  col.number<-which( colnames(master_data_copy)==colname )
  uni.values<-length(unique(master_data_copy[,col.number]))
  master_data_copy[,col.number]<-factor(master_data_copy[,col.number])
  print(str(master_data_copy[,col.number]))
  levels(master_data_copy[,col.number])<-c(0:(uni.values-1))
  print(str(master_data_copy[,col.number]))
  master_data_copy[,col.number]<- as.numeric(levels(master_data_copy[,col.number]))[master_data_copy[,col.number]]
  master_data_copy[,col.number]
  master_data_copy <<- master_data_copy  
}

#Gender
master_data_copy[,"Gender"]
Cate.Var.Conv.2level("Gender")
#PerformanceRating
#only two level are present in data; 
#3,4 meaning that other values of 1,2 not being used/assigned to any of the employess
#formal goal based performance management should help distinguish between performers and non performers.

master_data_copy[,"PerformanceRating"]
Cate.Var.Conv.2level("PerformanceRating")


#factor with more than 2 level
#numerical factors will remains as is
factor.var<-model.matrix(~EnvironmentSatisfaction+JobSatisfaction+
                           WorkLifeBalance+BusinessTravel+Department+
                           Education+EducationField+JobLevel+MaritalStatus+
                           StockOptionLevel+JobRole+
                           genderWiseMaritalStatus+JobInvolvement,data=master_data_copy)
factor.var<-factor.var[,-1]
str(factor.var)
#dropping the original categorical column
drops <- c('EnvironmentSatisfaction','JobSatisfaction','WorkLifeBalance','BusinessTravel','Department',
           'Education','EducationField','JobLevel','MaritalStatus','StockOptionLevel','JobRole','genderWiseMaritalStatus','JobInvolvement')
master_data_copy<-master_data_copy[ , !(names(master_data_copy) %in% drops)]
#combining the modified categorical columns set to the data
master_data_copy<-cbind(master_data_copy,factor.var)
summary(master_data_copy)
str(master_data_copy)

#dropping EmployeeID
master_data_copy<-master_data_copy[ , !(names(master_data_copy) %in% c("EmployeeID"))]
emp_master_data_final <- master_data_copy
master_data_copy[,"Attrition"]
colnames(emp_master_data_final)

########################################################################
# 02.Build model
########################################################################

# splitting the data between train and test
set.seed(100)

indices = sample.split(emp_master_data_final$Attrition, SplitRatio = 0.7)

train = emp_master_data_final[indices,]

test = emp_master_data_final[!(indices),]

#comapting spread of data in teh actaul data set and test data set
ggplot(emp_master_data_final,aes(x=Attrition))+geom_bar()+  geom_text(stat='count',aes(label=..count..),vjust=-1)
ggplot(test,aes(x=Attrition))+geom_bar()+  geom_text(stat='count',aes(label=..count..),vjust=-1)
#False is ard 5 times of true in both total data and test set
#represting that out test data represnt teh real world scenarios



# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = emp_master_data_final, family = "binomial")
summary(model_1) 
# Null deviance: 3895.7  on 4409  degrees of freedom
# Residual deviance: 2865.8  on 4365  degrees of freedom
# AIC: 2955.8
nrow(summary(model_1)$coefficients)#45 coefficients

# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")
summary(model_2) 
# Null deviance: 3895.7  on 4409  degrees of freedom
# Residual deviance: 2874.4  on 4378  degrees of freedom
# AIC: 2938.4
nrow(summary(model_2)$coefficients)#32 coefficients 
#13 varaible were removed

# The below section uses the broom library and creates a nice table with p values and vifs for every model variable
createCleanModelSummary <- function(modelName){
  model_summary <- tidy(modelName)[-1,c(1,5)]
  model_vif <- data.frame(vif(modelName))
  model_summary <- cbind(model_summary,model_vif[,1])
  colnames(model_summary) <- c("Input Variable Name","P Value","VIF")
  print(model_summary)
}

# lets see how well the model did
createCleanModelSummary(model_2)

# Many p values and vifs are high. We will start with the highest vif varaible `EducationFieldLife Sciences` 15
model_3 <- glm(formula = Attrition ~ median_office_time + Age + Gender + 
                 MonthlyIncome + NumCompaniesWorked + PercentSalaryHike + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsAtCompany + 
                 YearsSinceLastPromotion + YearsWithCurrManager + incomeToExpRatio + 
                 EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                 BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 `DepartmentResearch & Development` + DepartmentSales + Education + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + `EducationFieldTechnical Degree` + 
                 MaritalStatusSingle + `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                 `JobRoleSales Executive` + `JobRoleSales Representative` + 
                 genderWiseMaritalStatusMarried_Female, family = "binomial", 
               data = emp_master_data_final)

summary(model_3) 
# Null deviance: 3895.7  on 4409  degrees of freedom
# Residual deviance: 2879.6  on 4379  degrees of freedom
# AIC: 2941.6
#acceptable gain in AIC and Residual deviance

# lets see how well the model did
createCleanModelSummary(model_3)

#let's pick YearsAtCompany as next variabel 2nd highest vif and much higher insignificant  p value
model_4 <- glm(formula = Attrition ~ median_office_time + Age + Gender + 
                 MonthlyIncome + NumCompaniesWorked + PercentSalaryHike + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + incomeToExpRatio + 
                 EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                 BusinessTravelTravel_Frequently + BusinessTravelTravel_Rarely + 
                 `DepartmentResearch & Development` + DepartmentSales + Education + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + `EducationFieldTechnical Degree` + 
                 MaritalStatusSingle + `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                 `JobRoleSales Executive` + `JobRoleSales Representative` + 
                 genderWiseMaritalStatusMarried_Female, family = "binomial", 
               data = emp_master_data_final)

summary(model_4) 
# Null deviance: 3895.7  on 4409  degrees of freedom
# Residual deviance: 2881.8  on 4380  degrees of freedom
# AIC: 2941.8
#no significant chages to the model

# lets see how well the model did
createCleanModelSummary(model_4)

# next we remove BusinessTravelTravel_Rarely with high vif
model_5 <- glm(formula = Attrition ~ median_office_time + Age + Gender + 
                 MonthlyIncome + NumCompaniesWorked + PercentSalaryHike + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + incomeToExpRatio + 
                 EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                 BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales + Education + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + `EducationFieldTechnical Degree` + 
                 MaritalStatusSingle + `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                 `JobRoleSales Executive` + `JobRoleSales Representative` + 
                 genderWiseMaritalStatusMarried_Female, family = "binomial", 
               data = emp_master_data_final)

summary(model_5) 
# Null deviance: 3895.7  on 4409  degrees of freedom
# Residual deviance: 2895.8  on 4381  degrees of freedom
# AIC: 2953.8
# impact on the model is slightely higher with  AIC and Residual deviance increasing but still acceptable 

# lets see how well the model did
createCleanModelSummary(model_5)

#MaritalStatus.GenderMarried_Female is a next candidate with higher p value and 5th highest vif
model_6 <- glm(formula = Attrition ~ median_office_time + Age + Gender + 
                 MonthlyIncome + NumCompaniesWorked + PercentSalaryHike + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + incomeToExpRatio + 
                 EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                 BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales + Education + 
                 EducationFieldMarketing + EducationFieldMedical + EducationFieldOther + `EducationFieldTechnical Degree` + 
                 MaritalStatusSingle + `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                 `JobRoleSales Executive` + `JobRoleSales Representative`, family = "binomial", 
               data = emp_master_data_final)

summary(model_6) 
# Null deviance: 3895.7  on 4409  degrees of freedom
# Residual deviance: 2904.1  on 4382  degrees of freedom
# AIC: 2960.1
#acceptable increase in AIC and Residual deviance

# lets see how well the model did
createCleanModelSummary(model_6)

#EducationFieldMarketing with higher p-value is the next candidate
model_7 <- glm(formula = Attrition ~ median_office_time + Age + Gender + 
                 MonthlyIncome + NumCompaniesWorked + PercentSalaryHike + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + incomeToExpRatio + 
                 EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                 BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales + Education + 
                 EducationFieldMedical + EducationFieldOther + `EducationFieldTechnical Degree` + 
                 MaritalStatusSingle + `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                 `JobRoleSales Executive` + `JobRoleSales Representative`, family = "binomial", 
               data = emp_master_data_final)

summary(model_7) 
# Null deviance: 3895.7  on 4409  degrees of freedom
# Residual deviance: 2905.9  on 4383  degrees of freedom
# AIC: 2959.9
#decrease in AIC and slight increase in Residual deviance, very acceptable

# lets see how well the model did
createCleanModelSummary(model_7)

#EducationFieldMedical with very high p-value is the next candidate
model_8 <- glm(formula = Attrition ~ median_office_time + Age + Gender + 
                 MonthlyIncome + NumCompaniesWorked + PercentSalaryHike + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + incomeToExpRatio + 
                 EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                 BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales + Education + 
                 EducationFieldOther + `EducationFieldTechnical Degree` + 
                 MaritalStatusSingle + `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                 `JobRoleSales Executive` + `JobRoleSales Representative`, family = "binomial", 
               data = emp_master_data_final)

summary(model_8) 
# Null deviance: 3895.7  on 4409  degrees of freedom
# Residual deviance: 2906.0  on 4384  degrees of freedom
# AIC: 2958
#decrease in AIC and slight increase in Residual deviance, very acceptable

# lets see how well the model did
createCleanModelSummary(model_8)

#gender is the nect candidate for drop with very high p-value
model_9 <- glm(formula = Attrition ~ median_office_time + Age + MonthlyIncome + NumCompaniesWorked + PercentSalaryHike + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + incomeToExpRatio + 
                 EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                 BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales + Education + 
                 EducationFieldOther + `EducationFieldTechnical Degree` + 
                 MaritalStatusSingle + `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                 `JobRoleSales Executive` + `JobRoleSales Representative`, family = "binomial", 
               data = emp_master_data_final)

summary(model_9) 
# Null deviance: 3895.7  on 4409  degrees of freedom
# Residual deviance: 2906.2  on 4385  degrees of freedom
# AIC: 2956.2
#drop in aic and Residual deviance is a good sign

# lets see how well the model did
createCleanModelSummary(model_9)

#next candidate is PercentSalaryHike                   0.01581    0.01313   1.204 0.228577  
model_10 <- glm(formula = Attrition ~ median_office_time + Age + MonthlyIncome + NumCompaniesWorked +  
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + incomeToExpRatio + 
                  EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales + Education + 
                  EducationFieldOther + `EducationFieldTechnical Degree` + 
                  MaritalStatusSingle + `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                  `JobRoleSales Executive` + `JobRoleSales Representative`, family = "binomial", 
                data = emp_master_data_final)

summary(model_10) 
# Null deviance: 3895.7  on 4409  degrees of freedom
# Residual deviance: 2907.6  on 4386  degrees of freedom
# AIC: 2955.6
#slight incres in aic and Residual deviance

# lets see how well the model did
createCleanModelSummary(model_10)

#next candidate based on p-value is`JobRoleSales Representative`      -0.31430    0.21059  -1.492 0.135583
model_11 <- glm(formula = Attrition ~ median_office_time + Age + MonthlyIncome + NumCompaniesWorked +  
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + incomeToExpRatio + 
                  EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales + Education + 
                  EducationFieldOther + `EducationFieldTechnical Degree` + 
                  MaritalStatusSingle + `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                  `JobRoleSales Executive`, family = "binomial", 
                data = emp_master_data_final)

summary(model_11) 
# Null deviance: 3895.7  on 4409  degrees of freedom
# Residual deviance: 2910.0  on 4387  degrees of freedom
# AIC: 2956
# minute increase in aic

# lets see how well the model did
createCleanModelSummary(model_11)

#next candidate is Education                          -0.08367    0.04734  -1.767 0.077165
model_12 <- glm(formula = Attrition ~ median_office_time + Age + MonthlyIncome + NumCompaniesWorked +  
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + incomeToExpRatio + 
                  EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales + 
                  EducationFieldOther + `EducationFieldTechnical Degree` + 
                  MaritalStatusSingle + `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                  `JobRoleSales Executive`, family = "binomial", 
                data = emp_master_data_final)

summary(model_12) 
# Null deviance: 3895.7  on 4409  degrees of freedom
# Residual deviance: 2913.1  on 4388  degrees of freedom
# AIC: 2957.1
# minute chnage in AIC and  Residual deviance

# lets see how well the model did
createCleanModelSummary(model_12)

#next iteration is EducationFieldOther                -0.50778    0.22575  -2.249 0.024495 *
model_13 <- glm(formula = Attrition ~ median_office_time + Age + MonthlyIncome + NumCompaniesWorked +  
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + incomeToExpRatio + 
                  EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales + 
                  `EducationFieldTechnical Degree` + 
                  MaritalStatusSingle + `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                  `JobRoleSales Executive`, family = "binomial", 
                data = emp_master_data_final)

summary(model_13) 
# Null deviance: 3895.7  on 4409  degrees of freedom
# Residual deviance: 2918.5  on 4389  degrees of freedom
# AIC: 2960.5
#acceptable change in aic and Residual deviance

# lets see how well the model did
createCleanModelSummary(model_13)

#next iteration is`EducationFieldTechnical Degree`   -0.41961    0.18743  -2.239 0.025170 *  
model_14 <- glm(formula = Attrition ~ median_office_time + Age + MonthlyIncome + NumCompaniesWorked +  
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + incomeToExpRatio + 
                  EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales +
                  MaritalStatusSingle + `JobRoleManufacturing Director` + `JobRoleResearch Director` + 
                  `JobRoleSales Executive`, family = "binomial", 
                data = emp_master_data_final)

summary(model_14) 
# Null deviance: 3895.7  on 4409  degrees of freedom
# Residual deviance: 2923.9  on 4390  degrees of freedom
# AIC: 2963.9
#acceptable change in aic and Residual deviance

# lets see how well the model did
createCleanModelSummary(model_14)

#next candidate is`JobRoleSales Executive`            0.27849    0.11591   2.403 0.016276
model_15 <- glm(formula = Attrition ~ median_office_time + Age + MonthlyIncome + NumCompaniesWorked +  
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + incomeToExpRatio + 
                  EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales +
                  MaritalStatusSingle + `JobRoleManufacturing Director` + `JobRoleResearch Director`, family = "binomial", 
                data = emp_master_data_final)

summary(model_15) 
# Null deviance: 3895.7  on 4409  degrees of freedom
# Residual deviance: 2929.6  on 4391  degrees of freedom
# AIC: 2967.6
#acceptable increase in aic and Residual deviance

# lets see how well the model did
createCleanModelSummary(model_15)

#next candidate is`JobRoleResearch Director`          0.57738    0.18428   3.133 0.001730 **
model_16 <- glm(formula = Attrition ~ median_office_time + Age + MonthlyIncome + NumCompaniesWorked +  
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + incomeToExpRatio + 
                  EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales +
                  MaritalStatusSingle + `JobRoleManufacturing Director`, family = "binomial", 
                data = emp_master_data_final)

summary(model_16) 
# Null deviance: 3895.7  on 4409  degrees of freedom
# Residual deviance: 2938.9  on 4392  degrees of freedom
# AIC: 2974.9
#acceptable chnage in aic and Residual deviance

# lets see how well the model did
createCleanModelSummary(model_16)

#although all varaibles are very significant, lets try and remove the least among them
#MonthlyIncome                      -0.06970    0.02085  -3.344 0.000827 ***
model_17 <- glm(formula = Attrition ~ median_office_time + Age + NumCompaniesWorked +  
                  TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + incomeToExpRatio + 
                  EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales +
                  MaritalStatusSingle + `JobRoleManufacturing Director`, family = "binomial", 
                data = emp_master_data_final)

summary(model_17) 
# Null deviance: 3895.7  on 4409  degrees of freedom
# Residual deviance: 2950.2  on 4393  degrees of freedom
# AIC: 2984.2
# slightly bigger but still acceptable change in AIC and  Residual deviance

# lets see how well the model did
createCleanModelSummary(model_17)

#next candidate on the bases of comparably high p value
#TrainingTimesLastYear              -0.13978    0.03798  -3.680 0.000233 ***
model_18 <- glm(formula = Attrition ~ median_office_time + Age + NumCompaniesWorked +  
                  TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + incomeToExpRatio + 
                  EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales +
                  MaritalStatusSingle + `JobRoleManufacturing Director`, family = "binomial", 
                data = emp_master_data_final)

summary(model_18) 
# Null deviance: 3895.7  on 4409  degrees of freedom
# Residual deviance: 2964.1  on 4394  degrees of freedom
# AIC: 2996.1
# bigger but still acceptable change in AIC and  Residual deviance

# lets see how well the model did
createCleanModelSummary(model_18)

#`JobRoleManufacturing Director`    -0.76420    0.18092  -4.224 2.40e-05 ***
model_19 <- glm(formula = Attrition ~ median_office_time + Age + NumCompaniesWorked +  
                  TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + incomeToExpRatio + 
                  EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales +
                  MaritalStatusSingle, family = "binomial", 
                data = emp_master_data_final)

summary(model_19) 
# Null deviance: 3895.7  on 4409  degrees of freedom
# Residual deviance: 2984.2  on 4395  degrees of freedom
# AIC: 3014.2
# still acceptable change in AIC and  Residual deviance

# lets see how well the model did
createCleanModelSummary(model_19)

#Age                    -0.09666695 0.02167713 -4.4593974 8.219041e-06  
model_20 <- glm(formula = Attrition ~ median_office_time + NumCompaniesWorked +  
                  TotalWorkingYears + YearsSinceLastPromotion + YearsWithCurrManager + incomeToExpRatio + 
                  EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales +
                  MaritalStatusSingle, family = "binomial", data = emp_master_data_final)

summary(model_20) 
# Null deviance: 3895.7  on 4409  degrees of freedom
# Residual deviance: 3004.6  on 4396  degrees of freedom
# AIC: 3032.6
#increase in AIC & Residual deviance are getting higher

# lets see how well the model did
createCleanModelSummary(model_20)

#lets try for removing another variable and see its impact
#next candidate on the bases of comparably high p value
#YearsWithCurrManager               -0.09284715 0.02072463 -4.480040 7.462914e-06
model_21 <- glm(formula = Attrition ~ median_office_time + NumCompaniesWorked +  
                  TotalWorkingYears + YearsSinceLastPromotion + incomeToExpRatio + 
                  EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales +
                  MaritalStatusSingle, family = "binomial", data = emp_master_data_final)

summary(model_21) 
# Null deviance: 3895.7  on 4409  degrees of freedom
# Residual deviance: 3025.1  on 4397  degrees of freedom
# AIC: 3051.1
#interestingly Fisher Scoring iterations has dropped to 5 with this model
#signifiyng taht teh model is converging faster
#similar increase in AIC and Residual deviance:

# lets see how well the model did
createCleanModelSummary(model_21)

#lets try for removing another variable and see its impact
#WorkLifeBalance                    -0.3046446 0.06452733  -4.721172 2.344899e-06
model_22 <- glm(formula = Attrition ~ median_office_time + NumCompaniesWorked +  
                  TotalWorkingYears + YearsSinceLastPromotion + incomeToExpRatio + 
                  EnvironmentSatisfaction + JobSatisfaction +  
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + DepartmentSales +
                  MaritalStatusSingle, family = "binomial", data = emp_master_data_final)

summary(model_22) 
# Null deviance: 3895.7  on 4409  degrees of freedom
# Residual deviance: 3047.2  on 4398  degrees of freedom
# AIC: 3071.2
#similar increase in AIC and Residual deviance

# lets see how well the model did
createCleanModelSummary(model_22)


#lets try for removing another variable and see its impact
#next candidate on the bases of comparably high p value
#DepartmentSales                    -0.9725134 0.20252551  -4.801931 1.571432e-06
model_23 <- glm(formula = Attrition ~ median_office_time + NumCompaniesWorked +  
                  TotalWorkingYears + YearsSinceLastPromotion + incomeToExpRatio + 
                  EnvironmentSatisfaction + JobSatisfaction +  
                  BusinessTravelTravel_Frequently + `DepartmentResearch & Development` + 
                  MaritalStatusSingle, family = "binomial", data = emp_master_data_final)

summary(model_23) 
# Null deviance: 3895.7  on 4409  degrees of freedom
# Residual deviance: 3069.0  on 4399  degrees of freedom
# AIC: 3091
#slightly lower increase in AIC and Residual deviance compared to previous couple of changes

# lets see how well the model did
createCleanModelSummary(model_23)

#`DepartmentResearch & Development` -0.14040    0.09706  -1.446    0.148    
#`DepartmentResearch & Development` lost its significance value by dropping DepartmentSales
#lets try for removing it and see its impact
model_24 <- glm(formula = Attrition ~ median_office_time + NumCompaniesWorked +  
                  TotalWorkingYears + YearsSinceLastPromotion + incomeToExpRatio + 
                  EnvironmentSatisfaction + JobSatisfaction +  
                  BusinessTravelTravel_Frequently + MaritalStatusSingle, family = "binomial", data = emp_master_data_final)

summary(model_24) 
# Null deviance: 3895.7  on 4409  degrees of freedom
# Residual deviance: 3071.1  on 4400  degrees of freedom
# AIC: 3091.1
#minimal gain in aic and Residual deviance: 

# lets see how well the model did
createCleanModelSummary(model_24)

#lets try for removing another variable and see its impact
#next candidate on the bases of comparably high p value
#inc.exp.rt                       0.1665985 0.03000388   5.552563 2.815105e-08
model_25 <- glm(formula = Attrition ~ median_office_time + NumCompaniesWorked +  
                  TotalWorkingYears + YearsSinceLastPromotion + EnvironmentSatisfaction + JobSatisfaction +  
                  BusinessTravelTravel_Frequently + MaritalStatusSingle, family = "binomial", data = emp_master_data_final)

summary(model_25) 
# Null deviance: 3895.7  on 4409  degrees of freedom
# Residual deviance: 3101.8  on 4401  degrees of freedom
# AIC: 3119.8
#the gain aic and Residual deviance are comparablly higher 
# looks like we are reaching a boudary for variable removal

# lets see how well the model did
createCleanModelSummary(model_25)

#lets try to remove another varaible based on co-efficient from model 5.8
#YearsSinceLastPromotion          0.1143729 0.01773818   6.447836 1.134581e-10
model_26 <- glm(formula = Attrition ~ median_office_time + NumCompaniesWorked +  
                  TotalWorkingYears + EnvironmentSatisfaction + JobSatisfaction +  
                  BusinessTravelTravel_Frequently + MaritalStatusSingle, family = "binomial", data = emp_master_data_final)

summary(model_26) 
# Null deviance: 3895.7  on 4409  degrees of freedom
# Residual deviance: 3141.5  on 4402  degrees of freedom
# AIC: 3157.5
# Drop in AIC becomes bigger beyond this point, hence we will stop 

# lets see how well the model did
createCleanModelSummary(model_26)

# All the values show hig significance so lets stop here

#######################################################
#3.0.Model Validation
#######################################################
#below are the model we will consider for validation

Model_for_testing <- list(model_17,model_18,model_19,model_20,model_21,model_22,model_23,model_24,model_25,model_26)

no_of_model<-length(Model_for_testing)

result_matrix<-c()
for  (i in 1:no_of_model){
  test_pred = predict(Model_for_testing[[i]], type = "response", 
                      newdata = test[,-5])#ignoring Attrition
  
  test_pred_churn <- factor(ifelse(test_pred >= 0.5, "Yes", "No"))
  test_actual_churn <- factor(ifelse(test$Attrition, "Yes", "No"))
  test_conf <- caret::confusionMatrix(data=test_pred_churn, reference=test_actual_churn, positive = "Yes")
  result_matrix<-rbind(result_matrix,c(test_conf$overall["Accuracy"],
                                       test_conf$byClass["Sensitivity"],
                                       test_conf$byClass["Specificity"]))
  
}


result_matrix
View(result_matrix)
#model 9(model_25) have comparable sensitivity,accuracy and specifity  with minimal set of varaible
# [9,] 0.8722600   0.2957746   0.9828829

# let's check for varaious value for conversion what are accuracy, specifity 
# and sesnstivity of out choosen model
result_matrix<-data.frame()
str(result_matrix)
for  (i in seq(0.5,.1,-.01)){
  test_pred = predict(model_25, type = "response", 
                      newdata = test[,-5])
  
  test_pred_churn <- factor(ifelse(test_pred >= i, "Yes", "No"))
  test_actual_churn <- factor(ifelse(test$Attrition, "Yes", "No"))
  test_conf <- caret::confusionMatrix(data=test_pred_churn, reference=test_actual_churn, positive = "Yes")
  result_matrix<-rbind(result_matrix,c(i,test_conf$overall["Accuracy"],
                                       test_conf$byClass["Sensitivity"],
                                       test_conf$byClass["Specificity"]))
  
}
colnames(result_matrix)<-c("Check_value","Accuracy","Sensitivity","Specificity")
View(result_matrix)

#lets evalaute gains/loss at each level
result_matrix$chn_acc<-NA
result_matrix$chn_sens<-NA
result_matrix$chn_spec<-NA
for (i in 2:length(seq(0.5,.1,-.01)))
{
  result_matrix[i,c("chn_acc")]<-result_matrix[i,2]-result_matrix[i-1,2]
  result_matrix[i,c("chn_sens")]<-result_matrix[i,3]-result_matrix[i-1,3]
  result_matrix[i,c("chn_spec")]<-result_matrix[i,4]-result_matrix[i-1,4]
  
}

View(result_matrix)
#plotting the three graphs
s = seq(0.5,.1,-.01)
plot(s, result_matrix$Sensitivity,xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,result_matrix$Specificity,col="darkgreen",lwd=2)
lines(s,result_matrix$Accuracy,col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


#we will choose from:-
#chn_value  Accuracy Sensitivity Specificity  chn_acc       chn_sens    chn_spec
##  0.17 0.7702192   0.7323944   0.7774775 -0.0068027211 0.023474178 -0.0126126126
##  0.16 0.7596372   0.7464789   0.7621622 -0.0105820106 0.014084507 -0.0153153153
##  0.15 0.7369615   0.7699531   0.7306306 -0.0226757370 0.023474178 -0.0315315315
##at 0.16 the three curves are intersecting 
#after which the gain is Sensitivity is over shadowed by loss of Specificity
#we will use .16 as our conversion/change value


#########################################################################################
#########################################################################################
#Final model
summary(model_25)
# Null deviance: 3895.7  on 4409  degrees of freedom
# Residual deviance: 3101.8  on 4401  degrees of freedom
# AIC: 3119.8

#   median_office_time                  1.51703    0.09495  15.977  < 2e-16 ***
#people spending more time in office on a regualr bases are suspectible to churn
#the co-efficient is represeting that

#   NumCompaniesWorked               0.15545    0.01845   8.425  < 2e-16 ***
# more number of swicthes in the past indicates a habit to churn

#   TotalWorkingYears               -0.26100    0.01805 -14.461  < 2e-16 ***
# more experienced employees are less likely to churn
#indicatesd by teh negative co-effiecnet

#   YearsSinceLastPromotion          0.11437    0.01774   6.448 1.13e-10 ***
#no promostion is a motivation to leave although less than the previous swicthes
#as its coefficient is slightly lower

#   EnvironmentSatisfaction         -0.37716    0.04216  -8.946  < 2e-16 ***
# Being satisfied in teh work enviroment is more stroger motivation to stay stonger than TotalWorkingYears 

#   JobSatisfaction                 -0.34308    0.04166  -8.234  < 2e-16 ***
#Being satisfied in the job is equally important factor in emp staying

#   BusinessTravelTravel_Frequently  0.81371    0.10677   7.621 2.51e-14 ***
#Peaople being asked to travel frequenlty are more likely to churn

#   MaritalStatusSingle              1.00259    0.09331  10.745  < 2e-16 ***
#Single people tend to swicth more

test_pred = predict(model_25, type = "response", newdata = test[,-5])



test_pred_churn <- factor(ifelse(test_pred >= .16, "Yes", "No"))
test_actual_churn <- factor(ifelse(test$Attrition, "Yes", "No"))
test_conf <- caret::confusionMatrix(data=test_pred_churn, reference=test_actual_churn, positive = "Yes")
test_conf
# Accuracy    : 0.7596 
# Sensitivity : 0.7465          
# Specificity : 0.7622 

##################################################################################################
### KS -statistic - Test Data ######
##################################################################################################

test_cutoff_churn <- ifelse(test_pred_churn=="Yes",1,0)
test_actual_churn <- ifelse(test_actual_churn=="Yes",1,0)

#on testing  data
pred_object_test<- prediction(test_cutoff_churn, test_actual_churn)
pred_object_test
performance_measures_test<- performance(pred_object_test, "tpr", "fpr")
performance_measures_test



ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)#1] 0.508641

####################################################################
# Lift & Gain Chart 
####################################################################

# plotting the lift chart

lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Churn_decile = lift(test_actual_churn, test_pred, groups = 10)

Churn_decile
# 3      3   132       20.    155.  72.8    2.43
# 4      4   133       11.    166.  77.9    1.95
# 5      5   132       12.    178.  83.6    1.67

####################################################################
#lets use boot straping to test the stability of the model
####################################################################

set.seed(100)

#choosing a sample size of 35 
ratio_row<-35/nrow(test)
result_matrix<-data.frame()
for (c in 1:500){
  indices = sample.split(test$Attrition, SplitRatio = ratio_row)
  test_sample = test[!(indices),]
  
  
  test_pred = predict(model_21, type = "response", 
                      newdata = test_sample[,-5])
  
  test_pred_churn <- factor(ifelse(test_pred >= .16, "Yes", "No"))
  test_actual_churn <- factor(ifelse(test_sample$Attrition, "Yes", "No"))
  test_conf <- caret::confusionMatrix(data=test_pred_churn, reference=test_actual_churn, positive = "Yes")
  
  result_matrix<-rbind(result_matrix,c(c,test_conf$overall["Accuracy"],
                                       test_conf$byClass["Sensitivity"],
                                       test_conf$byClass["Specificity"]))
  
}


result_matrix
View(result_matrix)
colnames(result_matrix)<-c("Check_value","Accuracy","Sensitivity","Specificity")

#Accuracy
c(mean(result_matrix$Accuracy),max(result_matrix$Sensitivity),min(result_matrix$Specificity))
# 0.7785637 0.7971014 0.7733580
#Sensitivity
c(mean(result_matrix$Sensitivity),max(result_matrix$Sensitivity),min(result_matrix$Sensitivity))
# 0.7796715 0.7971014 0.7729469
#Specificity
c(mean(result_matrix$Specificity),max(result_matrix$Specificity),min(result_matrix$Specificity))
# 0.7783515 0.7853839 0.7733580

#As we can see over 500 iteration os sample size 35 the values are very stable for Accuracy,Sensitivity and Specificity
#We can safely give recomendation based on the model with these stable results