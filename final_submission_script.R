library(tidyverse)
library(jsonlite)
library(MASS)
library(dplyr)

#install.packages("data.table")
library(data.table)
#install.packages("readr")
library(readr)
library(tidyr)
library(magrittr)
library(lubridate)
library(purrr)
library(ggplot2)
library(gridExtra)
#install.packages("countrycode")
library(countrycode)
#install.packages("highcharter")
library(highcharter)
#install.packages("ggExtra")
library(ggExtra)


setwd("~/Documents/Fallsem/SYS/kagglecomp4")

training_data <- read_csv(file = "train.csv", col_names=T)  %>% 
  mutate(DataSplit = "Training") 




testing_data <- read_csv(file = "test.csv", col_names=T)  %>% 
  mutate(DataSplit = "Testing") 

full_data <- bind_rows(testing_data, training_data)


#parsing JSON colums

ParseJSONColumn <- function(x)  {
  str_c("[ ", str_c(x, collapse = ",", sep=" "), " ]")  %>% 
    fromJSON(flatten = T) %>% 
    as.tibble()
}


JSONcolumn_data <- full_data  %>% 
  dplyr::select(trafficSource, totals, geoNetwork, device)  %>% 
  map_dfc(.f = ParseJSONColumn)


full_data_wJSON <- bind_cols(full_data,JSONcolumn_data)

full_data_wJSON  %>% 
  group_by_at(vars(starts_with("mobile"),
                   ends_with("titude"),
                   starts_with("screen"),
                   starts_with(regex("browser[A-Z]")),
                   "operatingSystemVersion",
                   "flashVersion",
                   "language",
  ))  %>% 
  tally()


full_data_wJSON <- full_data_wJSON  %>% 
  dplyr::select(-device,
         -geoNetwork,
         -totals,
         -trafficSource,
         -networkLocation,
         -operatingSystemVersion,
         -flashVersion,
         -language,
         -starts_with("mobile"),
         -ends_with("itude"),
         -starts_with("screen"),
         -browserVersion,
         -contains(".CriteriaParameters"),
         -cityId,
         -browserVersion,
         -browserSize
  )

head(full_data_wJSON)
save(full_data_wJSON,file = "full_data_wJSON.Rdata")


###### cleaning data

#changing browser apart from mentioned to "other"
full_data_wJSON[(full_data_wJSON$browser!= "Safari") &
        (full_data_wJSON$browser!="Internet Explorer") &
        (full_data_wJSON$browser != "Firefox"), "browser"] <- "Other"

#changing OS apart from mentioned to "other"
full_data_wJSON[(full_data_wJSON$operatingSystem!= "Android") &
        (full_data_wJSON$operatingSystem!="Chrome OS") &
        (full_data_wJSON$operatingSystem != "iOS") &
        (full_data_wJSON$operatingSystem != "Linux") &
        (full_data_wJSON$operatingSystem != "Macintosh") &
        (full_data_wJSON$operatingSystem != "Windows"), "operatingSystem"] <- "Other"

#changing countries apart from mentioned to "other"
full_data_wJSON[(full_data_wJSON$country!= "United States") &
        (full_data_wJSON$country!="Vietnam") &
        (full_data_wJSON$country != "United Kingdom") &
        (full_data_wJSON$country != "Brazil") &
        (full_data_wJSON$country != "Canada") &
        (full_data_wJSON$country != "France") &
        (full_data_wJSON$country != "Germany") &
        (full_data_wJSON$country != "India") &
        (full_data_wJSON$country != "Italy") &
        (full_data_wJSON$country != "Japan") &
        (full_data_wJSON$country != "Mexico") &
        (full_data_wJSON$country != "Netherlands") &
        (full_data_wJSON$country != "Russia") &
        (full_data_wJSON$country != "Spain") &
        (full_data_wJSON$country != "Taiwan") &
        (full_data_wJSON$country != "Turkey"), "country"] <- "Other"

#converting into factors
categorical <- c("channelGrouping", "browser", "operatingSystem", "deviceCategory", "continent", "country", "campaign")
full_data_wJSON[, categorical] <- lapply(full_data_wJSON[, categorical], as.factor)


#converting into numeric
numeric <- c("hits", "pageviews", "visitNumber", "transactionRevenue")
full_data_wJSON[, numeric] <- lapply(full_data_wJSON[, numeric], as.numeric)

# transactionRevenue has many Nas which in reality are equivalent to having 0
full_data_wJSON[is.na(full_data_wJSON$transactionRevenue), "transactionRevenue"] <- 0

library(lubridate)
#install.packages("gbm")
library(gbm)

# convert visit StartTime from Unix timestamp to POSIXct and get useful features from it
full_data_wJSON$visitStartTime <- as.POSIXct(as.numeric(full_data_wJSON$visitStartTime), origin = '1970-01-01', tz = 'GMT')
full_data_wJSON$Year <- year(full_data_wJSON$visitStartTime)
full_data_wJSON$Year <- as.factor(full_data_wJSON$Year)
full_data_wJSON$Month <- month(full_data_wJSON$visitStartTime)
full_data_wJSON$Month <- as.factor(full_data_wJSON$Month)
full_data_wJSON$Hour <- hour(full_data_wJSON$visitStartTime)
full_data_wJSON$Hour <- as.factor(full_data_wJSON$Hour)

#we now have featues such as visitStartTime, Year, Month, Hour 



full_data_wJSON[,colSums(!is.na(full_data_wJSON))>0.1*nrow(full_data_wJSON)]


#removing columns with only 1 unique value
unique<-sapply(full_data_wJSON, function(x) {length(unique(x))})
one_val <- names(unique[unique = 1])
full_data_wJSON<-full_data_wJSON %>% dplyr::select(-one_val)

sum((train$transactionRevenue==0))


#converting date column to date format 
full_data_wJSON$date<- as.Date(as.character(full_data_wJSON$date), format ='%Y%m%d')
full_data_wJSON$visitNumber<-as.datetime(as.character(full_data_wJSON$visitNumber))


train<- full_data_wJSON %>% filter(DataSplit=="Training")
test<- full_data_wJSON %>% filter(DataSplit=="Testing")

save(train,file = "train.Rdata")
load("train.Rdata")
save(test,file = "test.Rdata")
load("test.Rdata")

combined<-bind_rows(train,test)



combined$start_Year<-format(combined$date,"%Y") # Get only the years

combined$start_Month<-format(combined$date,'%B') 

unique(combined$start_Year)

#extracting day od the week
#Weekday 0-6 , Sunday is 0
combined$weekday<-format(combined$date,'%w') 
unique(combined$weekday)
######### replacing with NAS#######

na_vals <- c("unknown.unknown", "(not set)", "not available in demo dataset", 
             "(not provided)", "(none)", "<NA>")

for(col in 1:ncol(combined)){
  combined[which(combined[,col] %in% na_vals), col]= NA
}


###Dropping cols with 90% NAs
combined <- combined[,colSums(!is.na(combined))>0.9*nrow(combined)]



###conversiting char to factors before modelling ######
combined$sessionId<-as.factor(combined$sessionId)
combined$socialEngagementType<-as.factor(combined$socialEngagementType)
combined$visitId<-as.factor(combined$visitId)
combined$source<-as.factor(combined$source)
combined$visits<-as.factor(combined$visits)
combined$hits<-as.factor(combined$hits)
combined$pageviews<-as.factor(combined$pageviews)
combined$continent<-as.factor(combined$continent)
combined$subContinent<-as.factor(combined$subContinent)
combined$country<-as.factor(combined$country)
combined$browser<-as.factor(combined$browser)
combined$isMobile<-as.factor(combined$isMobile)
combined$start_Year<-as.factor(combined$start_Year)
combined$start_Month<-as.factor(combined$start_Month)
combined$weekday<-as.factor(combined$weekday)



install.packages("chron")
library(chron)



save(combined,file = "combined.Rdata")
load("combined.Rdata")

combined$weekend <- combined$date %>% is.weekend()

train<- combined[combined$DataSplit == 'Training',]
test<- combined[combined$DataSplit == 'Testing',]


save(train,file = "train.Rdata")
load("train.Rdata")
save(test,file = "test.Rdata")
load("test.Rdata")



#######DATA EXPLORATION 

time_range <- range(train$date)
print(time_range)

#plotting distribution of log(transaction revenue)

train %>% 
  ggplot(aes(x=log(transactionRevenue), y=..density..)) + 
  geom_histogram(fill='steelblue', na.rm=TRUE, bins=40) + 
  geom_density(aes(x=log(transactionRevenue)), fill='orange', color='orange', alpha=0.3, na.rm=TRUE) + 
  labs(
    title = 'Distribution of transaction revenue',
    x = 'Natural log of transaction revenue'
  )


###Determinig difference through mobile and non mobile devices 

train %>%
  ggplot(aes(x=log(transactionRevenue), y=..density.., fill=isMobile)) +
  geom_density(alpha=0.5) + 
  scale_fill_manual(values = c('steelblue', 'orange')) + 
  labs(title='Distribution of log revenue by mobile and non-mobile devices')


#more data exploration plots of response var vs regressors (helps us identify knot positions used later in splines)

ggplot(data =train
       , aes(x = weekday, y = transactionRevenue)) + geom_bar(stat = "identity")

ggplot(data =train
       , aes(x = start_Month, y = transactionRevenue)) + geom_bar(stat = "identity")


ggplot(data =train
       , aes(x = continent, y = transactionRevenue)) + geom_bar(stat = "identity")


ggplot(data =train
       , aes(x = country, y = transactionRevenue)) + geom_bar(stat = "identity")


ggplot(data =train
       , aes(x = browser, y = transactionRevenue)) + geom_bar(stat = "identity")



ggplot(data =train
       , aes(x = operatingSystem, y = transactionRevenue)) + geom_point()

ggplot(data =train
       , aes(x = operatingSystem, y = transactionRevenue)) + geom_bar(stat = "identity")


ggplot(data =train
       , aes(x = deviceCategory, y = transactionRevenue)) + geom_point()


ggplot(data =train
       , aes(x = Hour, y = transactionRevenue)) + geom_point()



#filtering top 20 revs and excluding those points from plots
top_rev<-
  train %>% arrange(desc(transactionRevenue)) %>% head(20) 


ggplot(data = filter(train, !(transactionRevenue %in% top_rev$transactionRevenue)), aes(x=weekday, y=transactionRevenue)) + geom_point()

ggplot(data = filter(train, !(transactionRevenue %in% top_rev$transactionRevenue)), aes(x=start_Month, y=transactionRevenue)) + geom_point()



######converting transaction revenue to log(transactionRevenue) 

train$log.transactionRevenue<- log(train$transactionRevenue)

train[(train$log.transactionRevenue<0), "log.transactionRevenue"] <- 0

sum(is.na(train$log.transactionRevenue))

############

#sampling training data into train and test for performing crossvalidation
samplesize = 0.60 * nrow(train)
set.seed(120)
index = sample( seq_len ( nrow ( train ) ), size = samplesize )

# Create training and test set
datatrain = train[ index, ]
datatest = train[ -index, ]

save(datatrain,file = "datatrain.Rdata")
load("datatrain.Rdata")
save(datatest,file = "datatest.Rdata")
load("datatest.Rdata")





model_lm <- lm(log.transactionRevenue~country+operatingSystem+deviceCategory+Month+Hour+weekday+weekend, data=datatrain)
summary(model_lm)

#dropping variables with insignificant p value 

model_lm2 <- lm(log.transactionRevenue~+deviceCategory+Month+Hour+weekday+weekend, data=datatrain)
summary(model_lm)

#cross validation
pred=predict(model_lm,newdata=datatest,se=T)
head(pred)
pred$fit<-ifelse(pred$fit<0,0,pred$fit)

mean((pred$fit-datatest$log.transactionRevenue)^2)
#3.980305


pred=predict(model_lm2,newdata=datatest,se=T)
head(pred)
pred$fit<-ifelse(pred$fit<0,0,pred$fit)

mean((pred$fit-datatest$log.transactionRevenue)^2)
#4.049296


##building first lm model on whole train data 

model_lm <- lm(log.transactionRevenue~country+operatingSystem+deviceCategory+Month+Hour+weekday+weekend, data=train)

#predicting on test set 
pred=predict(model_lm,newdata=test,se=T)
pred$fit<-ifelse(pred$fit<0,0,pred$fit)

prediction_test = data.frame(matrix(0,nrow = nrow(test), ncol = 2))
prediction_test$fullVisitorId = test$fullVisitorId
prediction_test$log.transactionRevenue = pred$fit


prediction<-data.frame('fullVisitorId' = prediction_test$fullVisitorId, 'log.transactionRevenue' = prediction_test$log.transactionRevenue)

#converting log TR back to TR before grouping by and summarizing 
prediction$transactionRevenue<-exp(prediction_test$log.transactionRevenue)
prediction <- prediction %>% group_by(fullVisitorId) %>%  summarise(sum(transactionRevenue)) 
colnames(prediction)<-c("fullVisitorId","sumtransactionRevenue")
prediction$log.transactionRevenue<-log(prediction$sumtransactionRevenue)

prediction$log.transactionRevenue<-ifelse(prediction$log.transactionRevenue<0,0,prediction$log.transactionRevenue)


prediction<-subset(prediction,select = -c(sumtransactionRevenue))
head(prediction)
range(prediction$log.transactionRevenue)
######
######################
###RANDOM FOREST 

library(randomForest)


RF1<- randomForest(log.transactionRevenue~weekday+weekend+Month+Hour, data=datatrain, mtyr=4 , ntree=20)
yhat.bag = predict(RF1,newdata=datatest)
mean((yhat.bag-datatest$log.transactionRevenue)^2)

#checking variable importance
varImpPlot(RF1)
importance(RF1)

#crossvalidating 
RF2<- randomForest(log.transactionRevenue~operatingSystem+deviceCategory+weekday+weekend+Month+Hour, data=datatrain, mtyr=4 , ntree=40)
yhat.bag = predict(RF2,newdata=datatest)
mean((yhat.bag-datatest$log.transactionRevenue)^2)

RF3<- randomForest(log.transactionRevenue~deviceCategory+weekday+weekend+Month+Hour, data=datatrain, mtyr=4 , ntree=40)
yhat.bag = predict(RF3,newdata=datatest)
mean((yhat.bag-datatest$log.transactionRevenue)^2)


#building model on whole train data 
RFmodel<- randomForest(log.transactionRevenue~deviceCategory+weekday+weekend+Month+Hour, data=train, mtyr=4 , ntree=40)



pred=predict(RFmodel,newdata=test,se=T)


prediction_test = data.frame(matrix(0,nrow = nrow(test), ncol = 2))
prediction_test$fullVisitorId = test$fullVisitorId
prediction_test$log.transactionRevenue = pred


prediction<-data.frame('fullVisitorId' = prediction_test$fullVisitorId, 'log.transactionRevenue' = prediction_test$log.transactionRevenue)

#converting log TR back to TR before grouping by and summarizing 
prediction$transactionRevenue<-exp(prediction_test$log.transactionRevenue)
prediction <- prediction %>% group_by(fullVisitorId) %>%  summarise(sum(transactionRevenue)) 
colnames(prediction)<-c("fullVisitorId","sumtransactionRevenue")
#converting back to log 
prediction$log.transactionRevenue<-log(prediction$sumtransactionRevenue)

prediction$log.transactionRevenue<-ifelse(prediction$log.transactionRevenue<0,0,prediction$log.transactionRevenue)


prediction<-subset(prediction,select = -c(sumtransactionRevenue))
head(prediction)
range(prediction$log.transactionRevenue)


write.csv(prediction, file="prediction_rf.csv", row.names=FALSE)

############################ SPLINES 

library(splines)
hourlims=range(datatrain$Hour)
#1 to 24

wlims=range(datatrain$weekday)
# 1 to 7

datatrain$Hour<-as.numeric(datatrain$Hour)
datatrain$weekday<-as.numeric(datatrain$weekday)


datatest$Hour<-as.numeric(datatest$Hour)
datatest$weekday<-as.numeric(datatest$weekday)

#cross-validation
fit=lm(log.transactionRevenue~bs(Hour,knots=c(7,14,20))+bs(weekday,knots=c(4)),data=datatrain)

fit2=lm(log.transactionRevenue~bs(Hour,knots=c(8,14,20))+bs(weekday,knots=c(4)),+ bs(Month,knots=c(6)),data=datatrain)

fit3=lm(log.transactionRevenue~bs(Hour,knots=quantile(Hour))+bs(weekday,knots=quantile(weekday)),+ bs(Month,knots=quantile(Month)),data=datatrain)

#picking knots based on plots in Data exploration 
fit4=lm(log.transactionRevenue~bs(Hour,knots=c(8,14,20))+bs(weekday,knots=c(2,5)),+ bs(Month,knots=c(7,10,12)),data=datatrain)

pred=predict(fit4,newdata=datatest,se=T)
pred$fit<-ifelse(pred$fit<0,0,pred$fit)
mean((pred$fit-datatest$log.transactionRevenue)^2)

#picking fit4 based on CV

########### building model on whole data and predicting on test 

train$Hour<-as.numeric(train$Hour)
train$weekday<-as.numeric(train$weekday)
train$Month<-as.numeric(train$Month)


test$Hour<-as.numeric(test$Hour)
test$weekday<-as.numeric(test$weekday)
test$Month<-as.numeric(test$Month)

fit4=lm(log.transactionRevenue~bs(Hour,knots=c(8,14,20))+bs(weekday,knots=c(2,5)),+ bs(Month,knots=c(7,10,12)),data=train)


pred=predict(fit,newdata=test,se=T)
pred$fit<-ifelse(pred$fit<0,0,pred$fit)

prediction_test = data.frame(matrix(0,nrow = nrow(test), ncol = 2))
prediction_test$fullVisitorId = test$fullVisitorId
prediction_test$log.transactionRevenue = pred$fit


prediction<-data.frame('fullVisitorId' = prediction_test$fullVisitorId, 'log.transactionRevenue' = prediction_test$log.transactionRevenue)

prediction$transactionRevenue<-exp(prediction_test$log.transactionRevenue)
prediction <- prediction %>% group_by(fullVisitorId) %>%  summarise(sum(transactionRevenue)) 
colnames(prediction)<-c("fullVisitorId","sumtransactionRevenue")
prediction$log.transactionRevenue<-log(prediction$sumtransactionRevenue)

prediction$log.transactionRevenue<-ifelse(prediction$log.transactionRevenue<0,0,prediction$log.transactionRevenue)


prediction<-subset(prediction,select = -c(sumtransactionRevenue))
head(prediction)
range(prediction$log.transactionRevenue)


write.csv(prediction, file="prediction_spline1.csv", row.names=FALSE)


