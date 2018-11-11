# install.packages('MASS')
# install.packages('jsonlite')
# install.packages('tidyverse')
# install.packages('dplyr')
# install.packages("gbm")
# install.packages("lubridate")
# install.packages('datetime')
# install.packages('randomForest')
library(tidyverse)
library(jsonlite)
library(MASS)
library(dplyr)
library(lubridate)
library(gbm)
library(datetime)
library(randomForest)

setwd("~/Desktop/SYS6018/kaggle_competition/04_customer_revenue")

# use the following commands to generate train_small.csv
# unzip train_v2.csv.zip
# unzip test_v2.csv.zip
# pip install subsample
# subsample -n 400000 train_v2.csv -r > train_small.csv

training_data <- read_csv(file = "train_small.csv", col_names=T)  %>% 
  mutate(DataSplit = "Training") 

testing_data <- read_csv(file = "test_v2.csv", col_names=T)  %>% 
  mutate(DataSplit = "Testing") 

full_data <- bind_rows(testing_data, training_data)

# free memory
rm(training_data)
rm(testing_data)
gc() # garbage collect to free memory

#j<- full_data  %>% dplyr::select(trafficSource, totals, geoNetwork, device) 

ParseJSONColumn <- function(x)  {
  str_c("[ ", str_c(x, collapse = ",", sep=" "), " ]")  %>% 
    fromJSON(flatten = T) %>% 
    as.tibble()
}

JSONcolumn_data <- full_data  %>% 
  dplyr::select(trafficSource, totals, geoNetwork, device)  %>% 
  map_dfc(.f = ParseJSONColumn)

full_data_wJSON <- bind_cols(full_data[,-which(names(full_data) == 'hits')],JSONcolumn_data)

# free memory
rm(full_data)
rm(JSONcolumn_data)
gc()

full_data_wJSON  %>% 
  group_by_at(vars(starts_with("mobile"),
                   ends_with("titude"),
                   starts_with("screen"),
                   starts_with(regex("browser[A-Z]")),
                   "operatingSystemVersion",
                   "flashVersion",
                   "language"
  ))  %>% 
  tally()

full_data_wJSON <- full_data_wJSON  %>% 
  dplyr::select(-device,
                -geoNetwork,
                -customDimensions,
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

# load saved data
#load("full_data_wJSON.Rdata")


###### cleaning data

full_data_wJSON[full_data_wJSON$browser == "Safari (in-app)", "browser"] <- "Safari"
full_data_wJSON[(full_data_wJSON$browser == "Opera Mini") &
                  (full_data_wJSON$browser == "SAMSUNG-GT-C3322 Opera") &
                  (full_data_wJSON$browser == "SAMSUNG-SM-B355E Opera") &
                  (full_data_wJSON$browser == "SAMSUNG-SM-B350E Opera")  , "browser"] <- "Opera"

full_data_wJSON[(full_data_wJSON$browser!= "Safari") &
                  (full_data_wJSON$browser!= "Chrome") &
                  (full_data_wJSON$browser!= "Edge") &
                  (full_data_wJSON$browser!= "Opera") &
                  (full_data_wJSON$browser!="Internet Explorer") &
                  (full_data_wJSON$browser != "Firefox"), "browser"] <- "Other"

full_data_wJSON[(full_data_wJSON$operatingSystem!= "Android") &
                  (full_data_wJSON$operatingSystem!="Chrome OS") &
                  (full_data_wJSON$operatingSystem != "iOS") &
                  (full_data_wJSON$operatingSystem != "Linux") &
                  (full_data_wJSON$operatingSystem != "Macintosh") &
                  (full_data_wJSON$operatingSystem != "Windows"), "operatingSystem"] <- "Other"


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

# reduce factor levels to 50 if more than that
for (i in 1:ncol(full_data_wJSON)){
  full_data_wJSON[,i] <- unlist(full_data_wJSON[,i])
}


max_level <- 50
factor_col <- c()
for (an_index in 1:ncol(full_data_wJSON)) {
  if (is.factor(unlist(full_data_wJSON[1,an_index]))) {
    count_level <- length(unlist(unique(full_data_wJSON[,an_index])))
    if (count_level > max_level) {
      factor_col <- c(factor_col, an_index)
    }
  }
}

for (an_index in factor_col) {
  count_level <- summary(full_data_wJSON[,an_index], maxsum=max_level)
  top_count <- c()
  for (a in count_level){
    top_count <- c(top_count, trimws(unlist(strsplit(a,':'))[1]))
  }
  few_rows <- !(unlist(full_data_wJSON[,an_index]) %in% top_count)
  print(sum(few_rows))
  print(names(full_data_wJSON[few_rows,an_index]))
  full_data_wJSON[,an_index] <- as.character(unlist(full_data_wJSON[,an_index]))
  full_data_wJSON[few_rows,an_index] <- 'Other'
  full_data_wJSON[,an_index] <- as.factor(unlist(full_data_wJSON[,an_index]))
}

# transactionRevenue has many Nas which in reality are equivalent to having 0
full_data_wJSON[is.na(full_data_wJSON$transactionRevenue), "transactionRevenue"] <- 0

# convert visit StartTime from Unix timestamp to POSIXct and get useful features from it:
full_data_wJSON$visitStartTime <- as.POSIXct(as.numeric(full_data_wJSON$visitStartTime), origin = '1970-01-01', tz = 'GMT')
full_data_wJSON$Year <- year(full_data_wJSON$visitStartTime)
full_data_wJSON$Year <- as.factor(full_data_wJSON$Year)
full_data_wJSON$Month <- month(full_data_wJSON$visitStartTime)
full_data_wJSON$Month <- as.factor(full_data_wJSON$Month)
full_data_wJSON$Hour <- hour(full_data_wJSON$visitStartTime)
full_data_wJSON$Hour <- as.factor(full_data_wJSON$Hour)

#full_data_wJSON$visitStartTime <- NULL

# train$transactionRevenue[is.na(train$transactionRevenue)] = 0
# 
# sapply(train, class)


# full_data_wJSON[is.na(full_data_wJSON$keyword), "keyword"] <- "not provided"
# full_data_wJSON[is.na(full_data_wJSON$isTrueDirect), "isTrueDirect"] <- "not provided"
# full_data_wJSON[is.na(full_data_wJSON$referralPath), "referralPath"] <- "not provided"
# full_data_wJSON[is.na(full_data_wJSON$adContent), "adContent"] <- "not provided"
# full_data_wJSON[is.na(full_data_wJSON$campaignCode), "campaignCode"] <- "not provided"
# full_data_wJSON[is.na(full_data_wJSON$adwordsClickInfo.page), "adwordsClickInfo.page"] <- "not provided"
# full_data_wJSON[is.na(full_data_wJSON$adwordsClickInfo.slot), "adwordsClickInfo.slot"] <- "not provided"
# full_data_wJSON[is.na(full_data_wJSON$adwordsClickInfo.gclId), "adwordsClickInfo.gclId"] <- "not provided"
# full_data_wJSON[is.na(full_data_wJSON$adwordsClickInfo.adNetworkType), "adwordsClickInfo.adNetworkType"] <- "not provided"
# full_data_wJSON[is.na(full_data_wJSON$adwordsClickInfo.isVideoAd), "adwordsClickInfo.isVideoAd"] <- "not provided"
# full_data_wJSON[is.na(full_data_wJSON$pageviews), "adwordsClickInfo.isVideoAd"] <- 0
# full_data_wJSON[is.na(full_data_wJSON$newVisits), "newVisits"] <- 0
# full_data_wJSON[is.na(full_data_wJSON$bounces), "bounces"] <- "not provided"

# removing columns with too many NAs
full_data_wJSON <- full_data_wJSON[,colSums(!is.na(full_data_wJSON))>0.85*nrow(full_data_wJSON)]

# removing columns with only 1 unique value
unique<-sapply(full_data_wJSON, function(x) {length(unique(x))})
one_val <- names(unique[unique = 1])
full_data_wJSON<-full_data_wJSON %>% dplyr::select(-one_val)

for (i in 1:ncol(full_data_wJSON)){
  full_data_wJSON[,i] <- unlist(full_data_wJSON[,i])
}

#converting into factors
categorical <- c("browser", "operatingSystem", "deviceCategory", "continent", "country", "campaign")
full_data_wJSON[, categorical] <- lapply(full_data_wJSON[, categorical], as.factor)


#converting into numeric
numeric <- c("hits", "pageviews", "visitNumber", "transactionRevenue", "visits", 'fullVisitorId')
full_data_wJSON[, numeric] <- lapply(full_data_wJSON[, numeric], as.numeric)

# impute data
na_col_index <- which(colSums(is.na(full_data_wJSON)) > 0)
for (an_index in na_col_index) {
  full_data_wJSON[is.na(full_data_wJSON[,an_index]),an_index] <- median(unlist(full_data_wJSON[,an_index]), na.rm=T)
}


# convert char columns to factor
char_col <- c('socialEngagementType', 'source', 'medium', 'subContinent', 'region', 
              'metro', 'city', 'networkDomain', 'isMobile')
col_index <- which(names(full_data_wJSON) %in% char_col)
for (an_index in col_index) {
  full_data_wJSON[,an_index] <- as.factor(unlist(full_data_wJSON[,an_index]))
}

#converting date column to date format 

#full_data_wJSON$date<- as.Date(as.character(full_data_wJSON$date), format ='%Y%m%d')
#full_data_wJSON$visitNumber<-as.datetime(as.character(full_data_wJSON$visitNumber))


train<- full_data_wJSON %>% filter(DataSplit=="Training")
test<- full_data_wJSON %>% filter(DataSplit=="Testing")

save(train,file = "train.Rdata")
save(test,file = "test.Rdata")
#
#load("train.Rdata")
#load("test.Rdata") 




