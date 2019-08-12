
# With feature Scaling  :

#Here we are having,The data which is i 0 to 1 range, MAPE(mean absolute percentage error)
# is not working so, we are using rmse to find the best suitable model.
#----------------------------------------------------------------------------------------------

rm(list=ls())

library(ggplot2)
library(corrgram)
library(randomForest)
library(caret)
library(rpart)
library(MASS)
install.packages("usdm")
library(usdm)
#Reading the dataset
data=read.csv(file="BikeRental.csv",header=T)

#Creating Copy so that we can use it anywhere in our program
data_c=data
#----------------------------------------------------------------------------------------------
#Dropping two variables instant and dteday and 
data=data[,-c(1,2,4,6)]
head(data)
#checking the column names
colnames(data)
#checking the datatypes of each variable
str(data)
head(data,5)

#here we have found that the first 7 variables are of integer datatype for 
#catagorical variable as it shouldbe a factor for catogorical datatype


for(i in c('season','mnth','weekday','workingday','weathersit')){
  
  data[,i]=as.factor(data[,i])
  
}

#checking the datatypes
str(data)
#----------------------------------------------------------------------------------------------

# Missing value analysis


missing_val=data.frame(apply(data,2,function(x){sum(is.na(x))}))
missing_val

#As we don't have any of the  missing values we can proceed

#----------------------------------------------------------------------------------------------

# Outlier analysis

#taking the numerical indexes
num_index=sapply(data,is.numeric)
#taking the numerical data based on numerical indexes taken before
num_data=data[,num_index]
#checking numerical data
num_data
#getting all the numerical data columns
cnames=colnames(num_data)
#----------------------------------------------------------------------------------------------

# Visualizing the Outliers using boxplots

boxplot(data['casual'], data=data, xlab="Values of casual variable",main="Outlier Analysis for Casual")
boxplot(data['windspeed'], data=data, xlab="Values of windspeed variable",main="Outlier Analysis for windspeed")
boxplot(data['hum'], data=data, xlab="Values of hum variable",main="Outlier Analysis for hum")

#----------------------------------------------------------------------------------------------

#here we found that we are having outliers in variables like 'casual','windspeed','hum',holiday
#Now performing outlier analysis on theses variables

for (i in cnames){
  print(i)
  val=data[,i][data[,i]%in%boxplot.stats(data[,i])$out]
  data[,i][data[,i]%in% val]=NA
}

#checking the number of NA values present

sum(is.na(data))
#----------------------------------------------------------------------------------------------

# Now Applying missing value analysis to replace NA values that are present

data$windspeed[is.na(data$windspeed)]=median(data$windspeed,na.rm=T)
data$casual[is.na(data$casual)]=median(data$casual,na.rm=T)
data$hum[is.na(data$hum)]=median(data$hum,na.rm=T)


#checking the no of outliers after outlier analysis, as it is 0 we proceed to Feature selection
sum(is.na(data))

#----------------------------------------------------------------------------------------------


#visualizing the variables again after outlier analysis

boxplot(data['casual'], data=data, xlab="Outlier analysis of Casual variable",main="Outlier Analysis for casual")
boxplot(data['windspeed'], data=data, xlab="Outlier analysis of windspeed variable",main="Outlier Analysis for windspeed")
boxplot(data['hum'], data=data, xlab="Outlier analysis of hum variable",main="Outlier Analysis for hum")#----------------------------------------------------------------------------------------------

#Feature selection :

#correlation:
head(data)
num_data=data[,-c(1:5)]
corrgram(num_data,order=F,upper.panel=panel.pie,text.panel =panel.txt,main="Correlation plot")

#----------------------------------------------------------------------------------------------
#here windspeed and hum are not providing much information about the dependent variable cnt,
#so remove them

#Removing variables windspeed and hum
data=data[,-c(8,9)]
head(data)

#checking the columns that we have..
colnames(data)

#Checking the values of the data on which feature Scaling to be applied
head(data,5)

#----------------------------------------------------------------------------------------------

#Feature Scaling

#Here as our data is of different ranges we are making the data to be in a particular range

#Checking the distribution
hist(data$casual)
hist(data$registered)
hist(data$cnt)

str(data)

#----------------------------------------------------------------------------------------------

#As we don't have normally distributed data we are going to use normalization
#Here we are using the normalization formula to set the data in a range
for(i in c(8,9,10)){
  data[,i]=formatC((data[,i]-min(data[,i]))/(max(data[,i])-min(data[,i])), digits = 4, format = "f")
}
#----------------------------------------------------------------------------------------------
#Checking the datatypes of variables
str(data)

#So we found that casual,registered,cnt are having character datatype 
#So, We are Changing the datatype from character to numeric 

for(i in c(8,9,10)){
  
  data[,i]=as.numeric(data[,i])
  
}

#Now again checking the datatypes of the variables 
str(data)

# We found that that they became numerical..So proceed to next step..

#----------------------------------------------------------------------------------------------

#Sampling

#Sampling: We are splitting data into train and test So, that we can use test dataset 
# for prediction or classification.

set.seed(321)
indexTrain = createDataPartition(data$cnt, p = .80, list = FALSE)
train = data[ indexTrain,]
test  = data[-indexTrain,]

#Checking actual dataset, Train and test datasets

dim(data)
dim(train)
dim(test)
head(train)

str(data)
#----------------------------------------------------------------------------------------------

#Now using Machine Learning algorithms to predict the values :

#using decision tree for regression

f=rpart(cnt~ .,data=train,method="anova")
rpredictions=predict(f,test[,-10])

#calculating rmse
#The least the rmse value is, the greater the model is.


rmse = function(m, o){
  print(sqrt(mean((m - o)^2)))
}
rmse(test[,10],rpredictions)
# Value we got is  = 0.07886405


num_data

#----------------------------------------------------------------------------------------------

# Using linear regression for regression


vifcor(num_data, th=0.9)



lm_model=lm(cnt~.,data = train)
summary(lm_model)
#predicting

p_lr=predict(lm_model,test[,1:9])

#calculating rmse

rmse = function(m, o){
  print(sqrt(mean((m - o)^2)))
}
rmse(test[,10],p_lr)
#rmse value is = 0.04293188

#----------------------------------------------------------------------------------------------

#Using randomforest for regression

x <- train
# Fitting model
fit <- randomForest(train$cnt ~ .,x,ntree=500)
summary(fit)

#Predict Output 

rfpredicted= predict(fit,test)

#calculating rmse


rmse = function(m, o){
  print(sqrt(mean((m - o)^2)))
}
rmse(test[,10],rfpredicted)

#rmse value is 0.03534393

#----------------------------------------------------------------------------------------------


#So, when we compare All the Algorithms we found that Random Forest
#is having the least value.So, we can apply on our future data.


#comparing the results side by side

original=test[,10]
original
#we are column binding
Combined=cbind(original,round(rfpredicted,4))
Combined_data=as.data.frame(Combined)
names(Combined_data)=c('Original','Predicted')
head(Combined_data,20)

# The reason why I have done the with and without feature scaling is because
# When done feature Scaling in python , MAPE Worked and Linear regression gave 81% accuracy But
# In R , MAPE did not work it gave the value "Inf" and gave  less accuracy 
# So, I have performed both the methods to show the diference.

# In both the cases i.e 1.With Feature Scaling and 2.Without Feature Scaling

#Random Forest worked well while predicting the Featured Scaled data,
#Linear regression worked well while predicting the Original data.
#Depending on the context we have to use the different models to predict the values in the data.



