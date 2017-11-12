# Input load. Please do not change #
`dataset` = read.csv('C:/Users/lpchen/AppData/Local/Radio/REditorWrapper_885ef548-708c-4f55-8c3c-9c5cd0e3da9d/input_df_a02d3b59-a4dc-40b7-8676-e888f33e08a6.csv', check.names = FALSE, encoding = "UTF-8", blank.lines.skip = FALSE);
# Original Script. Please update your script content here and once completed copy below section back to the original editing window #

library(forecast)
library(plyr)
library(tidyverse)
library(gbm)
# data input
#data <- dataset # class: data.frame
data<-dataset %>%
  group_by(Year,Month) %>%
  summarise(
    Sales = sum(Sales)
  ) %>%
  mutate(month_num = match(Month,month.name))%>%
  arrange(Year,month_num)
#print(data)
test.length <- 0
seasonality <- 12
observation.freq <- "month"
timeformat <- "%m/%d/%Y"

get.pred.period<-function(year,month,horizon){
    year<-as.numeric(year)
   month<-as.numeric(month)
   horizon<-as.numeric(horizon)
   #print(year)
   #print(month)
   #print(horizon)
    if(horizon<1){
        return
    }
    mth<-month
    yr<-year
    Month<-c()
    Year<-c()
    for(i in 1:horizon){
        if(mth+1>12){
        yr<-yr+1
        mth<-mth-11
        Month<-c(Month,mth)
        Year<-c(Year,yr)
        }
        else{
        mth<-mth+1
        Month<-c(Month,mth)
        Year<-c(Year,yr)
        }
    
    }
#print(length(Month))
return(data.frame(Year,Month,stringsAsFactors=FALSE))

}

# Forecasting set-up
# Forecasting Function


  arima.single.id <- function(data){
  method.name <- "STL_ARIMA"
  pred_horizon<-15

  #print(colnames(data))
  # Train and test split
  data.length <- nrow(data)
  train.length <- data.length-test.length
  train <- data[1:train.length, ]
  
  
  yr<-data[train.length,"Year"]
  mth<-data[train.length,"month_num"]
  print(yr)
  print(mth)
  # Missing data: replace na with average
  train$Sales[is.na(train$Sales)] <- mean(train$Sales, na.rm = TRUE)

  # Build forecasting models
  train.ts <- ts(train$Sales, frequency = seasonality,start=c(2015,1))
  # train.model <- stlf(train.ts, h =pred_horizon, method = "arima", s.window = "periodic")
  train.stl <- stl(train.ts, s.window="periodic")
  train.model <- forecast(train.stl, h = pred_horizon, method = 'ets', ic = 'bic', opt.crit='mae')
 
  forecast.value <- train.model$mean
  forecast.lo95 <- train.model$lower[,1]
  forecast.hi95 <- train.model$upper[,1]
  #print(length(forecast.value))

  fct <-get.pred.period(yr,mth,pred_horizon)
  output <- data.frame( cbind(fct$Year,fct$Month,forecast.value))
  #colnames(output) <- paste(c("Year","Month","Forecast"), method.name, sep = ".") 
  colnames(output) <- c("Year","Month","Forecast")
  #print(fct$Month)
  #plot(fct$Month,forecast.value)
  #ggplot(data=output)+geom_line(mapping=aes(x=Month,y=Forecast,color=Year))

  return(output)
}

output <- arima.single.id(data)

ggplot(data=output)+geom_line(mapping=aes(x=Month,y=Forecast,color=Year))


our_model<-function(data,period){
  for(i in 1:period){
    #data<-mutate(data,newcol=lag(month_num,i))
    data<-cbind(data,newcol=lag(data$Sales,i))
    colnames(data)<-replace(colnames(data),length(colnames(data)),paste("lag",i,sep=""))
  }
  data<-na.omit(data)
  #print(data)
  return(data)
}
our_model_data<-our_model(data,6)

gbmModel = gbm(formula = Sales ~ Year + month_num+lag1+lag2+lag3+lag4+lag5+lag6 ,
               distribution = "gaussian",
               data = our_model_data,
               n.trees = 100,
               shrinkage = .2,
               bag.fraction = 1,
               n.minobsinnode = 10)

newdata<-our_model_data[nrow(our_model_data),c(1,4,3,5,6,7,8,9)]
newdata$month_num<-newdata$month_num+1
#newdata<-our_model_data[nrow(our_model_data),]
colnames(newdata)<-c('Year','month_num','lag1','lag2','lag3','lag4','lag5','lag6')
predict.gbm(gbmModel,newdata,n.trees = 100)
summary(gbmModel)


us_housing<-read.csv("C:/Users/lpchen/Documents/projects/demand_forecast/macro_eco/US_housing.csv")
us_housing <- us_housing %>%
  mutate(scale_housing = ( min(data$Sales, na.rm = TRUE) +
                             (us_housing$sale-min(us_housing$sale, na.rm = TRUE))/(max(us_housing$sale, na.rm = TRUE) - min(us_housing$sale, na.rm = TRUE)) *
                             (max(data$Sales, na.rm = TRUE) - min(data$Sales, na.rm = TRUE)) ))

data %>%
ggplot(aes(1:nrow(data),Sales))+geom_line()+
  geom_line(data=us_housing,aes(index+1,scale_housing),color='blue')+
  ggtitle("Total sales (black) with US housing price (blue)")






