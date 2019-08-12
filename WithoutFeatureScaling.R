# Here I have done the project with and without feature scaling,
# This is without using Feature Scaling,
#Another R file has the project done using Feature Scaling 

#---------------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------------
rm(list=ls())
library(ggplot2)
library(corrgram)
library(caret)
library(rpart)
library(MASS)
#Reading the dataset
data=read.csv(file="BikeRental.csv",header=T)
#---------------------------------------------------------------------------------------------
#Creating Copy so that we can use it anywhere in our program
data_c=data
#Dropping two variables instant and dteday
data=data[,-c(1,2,4,6)]
#---------------------------------------------------------------------------------------------
head(data)
#checking the column names
colnames(data)
#checking the datatypes of each variable
str(data)
head(data,5)
#---------------------------------------------------------------------------------------------
#Here Above we have found that the first 7 variables are of integer datatype for 
#catagorical variable as it should be a factor for catogorical datatype

#converting the datatype 
for(i in c('season','mnth','weekday','workingday','weathersit')){
  
  data[,i]=as.factor(data[,i])
  
}

#checking the datatypes
str(data)
#---------------------------------------------------------------------------------------------
# Missing value analysis


missing_val=data.frame(apply(data,2,function(x){sum(is.na(x))}))
missing_val

#As we don't have any of the  missing values we can proceed

#---------------------------------------------------------------------------------------------

# Outlier analysis


#taking the numerical indexes
num_index=sapply(data,is.numeric)
#taking the numerical data based on numerical indexes taken before
num_data=data[,num_index]
#checking numerical data
num_data
#getting all the numerical data columns
cnames=colnames(num_data)
#---------------------------------------------------------------------------------------------
# Visualizing the Outliers using boxplots

boxplot(data['casual'], data=data, xlab="Values of casual variable",main="Outlier Analysis for Casual")
boxplot(data['windspeed'], data=data, xlab="Values of windspeed variable",main="Outlier Analysis for windspeed")
boxplot(data['hum'], data=data, xlab="Values of hum variable",main="Outlier Analysis for hum")
boxplot(data['registered'], data=data, xlab="Outlier analysis of registered variable",main="Outlier Analysis for registered")
#---------------------------------------------------------------------------------------------
#here we found that we are having outliers in variables like 'casual','windspeed','hum'
#Now performing outlier analysis on theses variables

for (i in cnames){
  print(i)
  val=data[,i][data[,i]%in%boxplot.stats(data[,i])$out]
  data[,i][data[,i]%in% val]=NA
}
#---------------------------------------------------------------------------------------------
#checking the number of NA values present

sum(is.na(data))

# Now Applying missing value analysis to replace NA values that are present

data$windspeed[is.na(data$windspeed)]=median(data$windspeed,na.rm=T)
data$casual[is.na(data$casual)]=median(data$casual,na.rm=T)
data$hum[is.na(data$hum)]=median(data$hum,na.rm=T)

#checking the no of outliers after outlier analysis,But let's visualize the variables to find the outlier's

sum(is.na(data))

#as it is 0 we proceed to Feature selection

#visualizing the variables again after outlier analysis

boxplot(data['casual'], data=data, xlab="Outlier analysis of Casual variable",main="Outlier Analysis for casual")
boxplot(data['windspeed'], data=data, xlab="Outlier analysis of windspeed variable",main="Outlier Analysis for windspeed")
boxplot(data['hum'], data=data, xlab="Outlier analysis of hum variable",main="Outlier Analysis for hum")

#---------------------------------------------------------------------------------------------

#Feature selection :

#correlation:
head(data)
num_data=data[,-c(1:5)]
corrgram(data,order=F,upper.panel=panel.pie,text.panel =panel.txt,main="Correlation plot")


#here windspeed and hum are not providing much information about the dependent variable cnt,
#so remove them

#Removing variables windspeed and hum
data=data[,-c(8,9)]

#Checking wheather is it removed or not
head(data)

#checking the columns that we have..
colnames(data)

#---------------------------------------------------------------------------------------------

#Sampling:

#Sampling: We are splitting data into train and test So, that we can use test dataset 
# for prediction or classification.

set.seed(321)
indexTrain = createDataPartition(data$cnt, p = .80, list = FALSE)
train = data[ indexTrain,]
test  = data[-indexTrain,]

#Checking dimensions of actual dataset, Train and test dataset.

dim(data)
dim(train)
dim(test)

#---------------------------------------------------------------------------------------------

#Now using Machine Learning algorithms to predict the values :


#Using decision tree for regression

f=rpart(cnt~ .,data=train,method="anova")
fpredictions=predict(f,test[,-10])

#calculating mape

mape=function(y,yhat){
  mean(abs((y-yhat)/y))
}
mape(as.integer(test[,10]),as.integer(fpredictions))

#mape = 52.63 %
#accuracy = 47.36 %

#---------------------------------------------------------------------------------------------

#using linear regression for regression

lm_model=lm(cnt~.,data = train)

#predicting

p_lm=predict(lm_model,test[,1:9])

#Calculating mape

mape(test[,10],p_lm)

#Error Percentage = 20 % 
#Accuracy = 80%

#---------------------------------------------------------------------------------------------

#Using randomforest for regression
head(test)
head(predicted)
x <- train
# Fitting model
fit <- randomForest(train$cnt ~ .,x,ntree=500)
summary(fit)
#Predict Output 
predicted= predict(fit,test)

#Calculating mape

mape(test[,10],predicted)

#error percentage 56%
#accuracy 44%
#---------------------------------------------------------------------------------------------

#So, when we compare All the Algorithms we found that linear regression is
#providing the best incase of accuracy and error percentage..


#comparing the results side by side, It is only 80% accurate.

original=test[,10]
original
#we are column binding
Combined=cbind(original,round(p_lm,0))
Combined_data=as.data.frame(Combined)
names(Combined_data)=c('Original','Predicted')
head(Combined_data,20)


# We found that linear regression is providing the best results.

# So, we can apply linear regression on the future data i.e bike Rental prediction Project.

#---------------------------------------------------------------------------------------------






