install.packages(c("dplyr","ggplot2"))
install.packages("lattice")
library(lattice)
library(dplyr)
library(ggplot2)
hr.data<-read.csv('HR_comma_sep.csv',stringsAsFactors = T)
str(hr.data)
summary(hr.data)
names(hr.data)[9]<-"department"
names(hr.data)[4]<-"average_monthly_hours"
levels(hr.data$department)
hr.data<-hr.data[c(7,1,2,3,4,5,6,8,9,10)]


#Exploratory Data Analysis 
table(hr.data$left,hr.data$department)
table(hr.data$last_evaluation,hr.data$department)
table(hr.data$satisfaction_level, hr.data$department)
summary(hr.data$department)

ggplot(hr.data, mapping = aes(satisfaction_level,department))+geom_col()
plot(hr.data[,1:9])
ggplot(hr.data, mapping = aes(average_monthly_hours,satisfaction_level))+geom_col()
ggplot(hr.data, mapping = aes(salary,satisfaction_level))+geom_boxplot()
ggplot(hr.data, mapping = aes(last_evaluation,department))+geom_boxplot()
ggplot(hr.data, mapping = aes(satisfaction_level,department))+geom_boxplot()
ggplot(hr.data, mapping=aes(time_spend_company, satisfaction_level))+ geom_col()

ggplot(hr.data, mapping = aes(department,salary))+ geom_col()
ggplot(hr.data, mapping=aes( salary,satisfaction_level))+geom_col()
ggplot(hr.data, mapping = aes(department, left)) +geom_col()
ggplot(hr.data, mapping=aes(department,average_monthly_hours)) +geom_col()
ggplot(hr.data,mapping = aes(department,number_project)) +geom_col()
ggplot(hr.data, mapping=aes(department, promotion_last_5years)) +geom_col()

table(hr.data$number_project, hr.data$department)
hist(hr.data$number_project)
install.packages("GGally")
library(GGally)
ggcorr(hr.data)


#Create dummies suitable for modelling and removing 'department' to aid test and train predictive
install.packages("dummies")
library(dummies)
hr.data<-cbind(hr.data,dummy(hr.data$department, sep = "_"))
hr.data<-hr.data[-c(9)]



# 4. Decision tree prediction
install.packages("ISLR")
install.packages("data.tree")
install.packages("rpart.plot")
install.packages("caret")
library(ISLR)
library(data.tree)
library(rpart)
library("rpart.plot")
library(caret)


# formula for predicting staff leaving
hr_formula = left ~ satisfaction_level + last_evaluation+ number_project + salary+ time_spend_company


# train a decision tree
n_rows <- nrow(hr.data)
training_idx <- sample(n_rows, n_rows * 0.7)
training <- hr.data[training_idx,]
test <- hr.data[-training_idx,]
dim(training)
dim(test)

tree_hr <- rpart(hr_formula, data = training, method= "anova",cp=0.1)
summary(tree_hr)

# plot the tree with rpartplot
par(mfrow= c(1,2),xpd=TRUE)
plot(tree_hr)
text(tree_hr, use.n = TRUE)
rpart.plot(tree_hr)

printcp(tree_hr)
plotcp(tree_hr)

#PrEDICT R PART
hr_predict<-predict(tree_hr,test)




