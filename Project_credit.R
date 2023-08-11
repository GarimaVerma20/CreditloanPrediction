### PROJECT Credit Loan Prediction
# loading packages
library(rpart)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(caret)
library(rpart.plot)
library(ROSE)

# loading and reading structure of data

loan_data = read.csv(file.choose(),stringsAsFactors=TRUE)  # choose file of data named as train 
str(loan_data)

# Summary of data

summary(loan_data)
# putting NA in place of missing values
loan_data[loan_data==""]=NA
# as there are some missing values and i have put NA there .

# Handling missing values
# Dataset is small and data is imbalance for target variables. So,for balance out data
# I am imputing mode vales for categorical data and the median for continous variables.

# Data imputation for categorical variables(imputing mode values)

loan_data$Gender[is.na(loan_data$Gender)]='Male'
loan_data$Married[is.na(loan_data$Married)]='Yes'
loan_data$Self_Employed[is.na(loan_data$Self_Employed)]='No'
loan_data$Credit_History <- as.factor(loan_data$Credit_History)
loan_data$Credit_History[is.na(loan_data$Credit_History)]='1'
loan_data$Loan_Amount_Term <- as.factor(loan_data$Loan_Amount_Term)
loan_data$Loan_Amount_Term[is.na(loan_data$Loan_Amount_Term)]='360'

#Data imputation for continuous variables(imputing median values)

loan_data$LoanAmount[is.na(loan_data$LoanAmount)]=median(loan_data$LoanAmount, na.rm=TRUE)
str(loan_data)

# Basic Exploratory Data Analysis
# Some questions raised
# 1) What is distribution of the applicant's income?
# 2) What is distribution of loans approved?
# 3) What is proportion of loan approved?
# 4) What is the average loan amount between males and females?
# 5) What is the average loan amount between married and non-married?
# 6) What is the average loan amount between employeed and self employeed?
# 7) What are the most important factors that determine whether an applicant gets the loan or not?

# Visualising the applicant's distribution
ggplot(loan_data, aes(ApplicantIncome,))+geom_histogram(bins=300,alpha=0.5)+coord_cartesian(xlim=c(0,25000),ylim=c(0,50))+labs(title="Distribution of Income for applicants",x="Income",y="Number of applicants")
# Hist_Distribution_of_income_for_applicants ###
# the histogram shows a fatter tail on the let side of the distribution, the median of the applicant's income is 3812 dollors and 
# only 25 percent of the applicants earns more than 5795 dollors.

ggplot(loan_data, aes(LoanAmount))+geom_histogram(bins=140,alpha=0.5)+coord_cartesian(xlim=c(0,200),ylim=c(0,60))+labs(title="Distribution of Loan amount",x="Amount",y="Number of loans")
# Hist_Distribution_of_Loan_Amount
# The histogram shows a more symmetrical distribution for the loan amount in comparision with the distribution of income for applicants.
# The average loan is of 145 dollor and only 25 percent of the loans approved are higher than 164.5 dollors.

ggplot(data=loan_data,aes(x=factor(Loan_Status),y=prop.table(after_stat(count)),fill=factor(Loan_Status),label=scales::percent(prop.table(after_stat(count)))))+geom_bar(position="dodge")+geom_text(stat="count",position=position_dodge(0.9),vjust=-0.5,size=3)+scale_x_discrete(labels=c("No Approved","Approved"))+scale_y_continuous(labels=scales::percent)+ggtitle("Proportion of loan aplications approved")+labs(x="",y="Proportion of Applications")
# Hist_Proportion_of_loan_applications_approved
# As we can , more people get their loan applications approved in comparision to those who are rejected.

library(tidyverse)
gender<- loan_data %>% group_by(Gender) %>% summarize(avg_loan=mean(LoanAmount))
ggplot(data=gender,aes(x=Gender,y=avg_loan))+geom_col(aes(fill=Gender))+ggtitle("Average loan amount by gender")+labs(x="")
# Hist_Average_loan_amount_by_gender
# As we can see males tend to get into debt more in comparision to females, for example the average loan amount for males is 
# 149.99 dollors and for females is 126.73 dollors, respectively.
 
married<-loan_data %>% group_by(Married) %>% summarize(avg_loan=mean(LoanAmount))
ggplot(data=married,aes(x=Married,y=avg_loan))+geom_col(aes(fill=Married))+labs(title="Average loan by married people")+labs(x="")
# Hist_Average_loan_by_married_people
# As we can see married people ask for higher loans, for example the average loan amount for married people is 
# 154.72 dollors in comparision to 128.85 dollors for those who are not married.

self_employed<-loan_data %>% group_by(Self_Employed) %>% summarize(avg_loan=mean(LoanAmount))
ggplot(data=self_employed,aes(x=Self_Employed,y=avg_loan))+geom_col(aes(fill=Self_Employed))+labs(title="Average loan by self_employed people")+labs(x="")
# Hist_Average_loan_by_self_employed_people
# We see that self employeed people ask for bigger loans, for example the average loan amount for self employeed people is
# 170.39 dollors which is 28.36 dollors greater in comparision to employees.

# Testing for relationships between variables for loan approval status

# Testing statistically significant dependence between gender and loan approval
chisq.test(loan_data$Gender,loan_data$Loan_Status,correct=FALSE)
# Since the p-value> 0.05 significant level, we can not reject the null hypothesis that the variables are independent and
# we conclude that there is no a significant relationship between these two variables.

# Testing statistically significant dependence between married and loan approval
chisq.test(loan_data$Married,loan_data$Loan_Status,correct=FALSE)
# Since the p-value<0.05 significant level , we reject the null hypothesis that the variables are independent and 
# we conclude that there is a significant relationship between these two variables.

# Testing statistically significant dependence between dependents and loan approval
chisq.test(loan_data$Dependents,loan_data$Loan_Status)
# Since the p-value>0.05 significant level, we can not reject the null hypothesis that the variables are independent and
# we conclude that there is no significant relationship between these two variables.

# Testing statistically significant dependence between education and loan approval
chisq.test(loan_data$Education,loan_data$Loan_Status,correct=FALSE)
# Since the p-value<0.05 significant level, we reject the null hypothesis that the variables are independent and
# we conclude that there is a significant relationship between these two variables.
 
# Testing statistically significant dependence between self employed and loan approval
chisq.test(loan_data$Self_Employed,loan_data$Loan_Status,correct=FALSE)
# Since the p-value>0.05 significant level, we can not reject the null hypothesis that the variables are independent and
# we conclude that there is no significant relationship between these two variables.

# Testing statistically significant dependence between credit history and loan approval
chisq.test(loan_data$Credit_History,loan_data$Loan_Status,correct=FALSE)
# Since the p-value<0.05 significant level, we reject the null hypothesis that the variables are independent and
# we conclude that there is a significant relationship between these two variables.

# Testing statistically significant dependence between property area and loan approval
chisq.test(loan_data$Property_Area,loan_data$Loan_Status,correct=FALSE)
# Since the p-value<0.05 significant level, we reject the null hypothesis that the variables are independent and
# we conclude that there is a significant relationship between these two variables.

# Testing statistically significant dependence between loan amount and loan approval
chisq.test(loan_data$Loan_Amount,loan_data$Loan_Status,simulate.p.value=TRUE)
# Since the p-value>0.05 significant level, we can not reject the null hypothesis that the variables are independent and
# we conclude that there is no significant relationship between these two variables.


#Exploring the relationship between applicant income and loan status
ggplot(loan_data,aes(x=Loan_Status,y=ApplicantIncome))+geom_boxplot()+coord_cartesian(ylim=c(0,10000))+ labs(title="Boxplot Applicant income by loan status")
# Boxplot_Applicant_income_by_loan_status
# Suprisingly the boxplot shows that there is not a clear relationship between the applicant's income and the loan status.

ggplot(loan_data,aes(x=Loan_Status,y=CoapplicantIncome))+geom_boxplot()+coord_cartesian(ylim=c(0,10000))+ labs(title="Boxplot Coapplicant income by loan status")
# Boxplot_coapplicant_income_by_loan_status
# The boxplot shows that there is not a clear relationship between the coapplicant's income and the loan status . 
# Moreover,comparing these two plots we see that coapplicants have a lower income .
 
# Data Preparation
# Splitting the data for training and testing

library(caret)
loan_data <- loan_data%>%select(-Loan_ID)
set.seed(1996)
split<- createDataPartition(y=loan_data$Loan_Status,time=1,p=.70,list=FALSE)
data_train<-loan_data[split,]
data_test<-loan_data[-split,]
table(data_train$Loan_Status)
table(data_test$Loan_Status)
# Because the dataset is small and the data is imbalanced the SMOTE technique is applied
# to test if there's a difference in comparision with the original data.

#SMOTE Sampling
#installing devtools
remote::install_github("cran/DMwR")
library(DMwR)
set.seed(1996)
smote_train<-SMOTE(Loan_Status~.,data=data_train)
table(smote_train$Loan_Status)

# DATA MODELING
set.seed(1996)
orig_fit<-rpart(Loan_Status~.,data=data_train)
pred_orig<-predict(orig_fit,newdata=data_test, method="class")
rpart.plot(orig_fit)
# Orig_fit_Tree
roc.curve(data_test$Loan_Status,pred_orig[,2],plotit=TRUE)
# Roc_curve
observ_orig<-as.factor(data_test$Loan_Status)
predic_orig<-as.factor(ifelse(test=pred_orig[,2]>0.50,yes='Y',no='N'))
matriz_orig<-confusionMatrix(predic_orig,observ_orig,positive='Y')
matriz_orig


set.seed(1996)
prune_control<-rpart.control(maxdepth=3,minsplit=1)
rose_fit<-rpart(Loan_Status~.,data=smote_train,control=prune_control)
pred_rose<-predict(rose_fit,newdata=data_test,method="class")
rpart.plot(rose_fit)
# Rose_fit_Tree
roc.curve(data_test$Loan_Status,pred_rose[,2],plotit=TRUE)
# Roc_curve_rose_fit
observ_rose<-as.factor(data_test$Loan_Status)
predic_rose<-as.factor(ifelse(test=pred_rose[,2]>0.70,yes='Y',no='N'))
matriz_rose<-confusionMatrix(predic_rose,observ_orig,positive='Y')
matriz_rose

plotcp(rose_fit)
# Plot_Tree
# CONCLUSION
# The two models created have a similar performance with the original data and 
# the balance data , we observe a precision around 80%, however, the models seems 
# to be pretty good at predicting positive outcomes. The balance accuracy for these 
# two models is around 70%. The kappa coefficient indicates that there's is a fair 
# agreement between classes,also the AUC indicates that the models has around 70% of 
# the probability of guessing correctly between these two classes.

# Suprisingly, the evidence shows that the income is not important to determine whether
# an applicant will get the loan, however, credit history is the most important variables
# that determines whether an applicant gets the loan approved or not.

# The variables Married, Education, Credit History,Property Area seem to be important to 
# determine whether an applicant gets the loan approved accordingly to the tests of dependence
# performed with this in mind another approaches like Logistic Regression could be explored.
 






