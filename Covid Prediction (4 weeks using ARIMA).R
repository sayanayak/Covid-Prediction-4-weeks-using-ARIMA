# In this Analysis, you will be predicting the cumulative number 
# of confirmed COVID19 cases in various locations across the world,
# as well as the number of resulting fatalities, for future dates.


# Importing packages
library(tidyverse)
library(timeSeries)
library(dplyr) 
library(caret)
library(ggplot2)
library(forecast)
library(lubridate)
library(ModelMetrics)
set.seed(123) 
## Reading in files
covid_data_train = read.csv('C:/Users/SAYAN/Desktop/train.csv')
covid_data_test = read.csv('C:/Users/SAYAN/Desktop/test.csv')
covid_data_submission = read.csv('C:/Users/SAYAN/Desktop/submission.csv')
str(covid_data_train)
str(covid_data_test)
str(covid_data_submission)
# Converting factor datatypes to character
covid_data_train[["Province_State"]] <- as.character(covid_data_train[["Province_State"]] )
covid_data_train[["Country_Region"]] <- as.character(covid_data_train[["Country_Region"]] )
str(covid_data_train)

covid_data_test[["Province_State"]] <- as.character(covid_data_test[["Province_State"]] )
covid_data_test[["Country_Region"]] <- as.character(covid_data_test[["Country_Region"]] )
str(covid_data_test)

covid_data_train[["Date"]] <- as.Date(covid_data_train[["Date"]], format = "%Y-%m-%d")
covid_data_test[["Date"]] <- as.Date(covid_data_test[["Date"]], format = "%Y-%m-%d")
str(covid_data_train)

# Some of the Province State are missing, to replace the missing Province States with their Country.
covid_data_train[["Province_State"]] <- ifelse(covid_data_train[["Province_State"]] == "", covid_data_train[["Country_Region"]], covid_data_train[["Province_State"]])
covid_data_test[["Province_State"]] <- ifelse(covid_data_test[["Province_State"]] == "", covid_data_test[["Country_Region"]], covid_data_test[["Province_State"]])

head(covid_data_train)
tail(covid_data_train)

# show variables with missing values
colSums(is.na(covid_data_train)) 
colSums(is.na(covid_data_test)) 
sum(is.na(covid_data_train))

# cummulative the Train data by Date
covidCummulativeData <- covid_data_train %>% 
  group_by(Date) %>%
  summarise(ConfirmedCases = sum(ConfirmedCases), Fatalities = sum(Fatalities))


ggplot(covidCummulativeData, aes(Date)) + 
  geom_line(aes(y = ConfirmedCases/1000000, colour = "Confirmed Cases")) + 
  geom_line(aes(y = Fatalities/1000000, colour = "Fatalities")) + 
  labs(x = 'Date since 22-01-2020', y = 'Count Per Million') +
  ggtitle("Covid-19 Confirmed & Fatalities Cases with Date")


# Group the Train data by Country and Date
covidFilterData <- covid_data_train %>%
  dplyr::filter(Country_Region %in% c("China","Italy","US","Germany","Iran","Spain","France","Korea, South","Switzerland","United Kingdom","India")) %>%
  group_by(Date, Country_Region) %>%
  summarise(ConfirmedCases = sum(ConfirmedCases), Fatalities = sum(Fatalities))

# visualize the confirmed cases of few countries
ggplot(data = covidFilterData, aes(x = Date, y = ConfirmedCases, group = Country_Region)) +
  geom_line(aes(color = Country_Region)) + 
  labs(x = 'Date since 22-01-2020', y = 'Count') + 
  geom_point(aes(color=Country_Region))+
  ggtitle("Covid-19 Confirmed Cases with Date") 

# visualize the Fatalities cases of few countries
ggplot(data = covidFilterData, aes(x = Date, y = Fatalities, group = Country_Region)) +
  geom_line(aes(color = Country_Region)) + 
  labs(x = 'Date since 22-01-2020', y = 'Fatalities Count') + 
  geom_point(aes(color=Country_Region))+
  ggtitle("Covid-19 Fatalities with Date") 

#Create long format
longFrmtData <- gather(covidFilterData, event, total, ConfirmedCases:Fatalities) 
head(longFrmtData)

ggplot(longFrmtData, aes(Country_Region, total/1000, fill=event)) + 
  geom_bar(stat = "identity", position = 'dodge') + 
  labs(x="Country",y="Count per Day per Thousand") + 
  ggtitle("Confirmed vs Fatalities")

unique_state_train <- covid_data_train %>% distinct(Province_State)
head(unique_state_train)

state_len <- length(unique_state_train$Province_State)
cat("Count of unique Province States:", state_len, "\n")


## Foresting

for(states in 1:state_len){
  
  cat(states, "/", state_len, "Province:", unique_state_train[["Province_State"]][states], "\n")
  
  train <- covid_data_train %>% dplyr::filter(covid_data_train[["Province_State"]] == unique_state_train[["Province_State"]][states] ) %>%
    arrange(Date) 
  
  test <- covid_data_test %>% dplyr::filter(covid_data_test[["Province_State"]] == unique_state_train[["Province_State"]][states]) %>%
    arrange(Date)
  
  
  ## Confirm Case Forecasting
  
  if (all(train$ConfirmedCases == 0)) {
    
    covid_data_submission[["ConfirmedCases"]][covid_data_submission[["ForecastId"]] %in% test[["ForecastId"]]] <- 0
    covid_data_submission[["Fatalities"]][covid_data_submission[["ForecastId"]] %in% test[["ForecastId"]]] <- 0
    
    next()
    
  } else {
    
    ts.cc.train <- ts(train$ConfirmedCases, start = decimal_date(as.Date("2020-01-22")), frequency = 365.25)
    
    fit.cc <-  Arima(ts.cc.train, order = c(2,2,2), seasonal = list(order = c(1,1,0), period = 12), method = "ML",
                     optim.method = "BFGS")
    
    
    forecast.cc <- forecast(fit.cc, h=43, level=c(99.5))
    
    for(i in 1:43){
      test$ConfirmedCases[i] <- ifelse(forecast.cc[["upper"]][i] > 0, as.numeric(round(forecast.cc[["upper"]][i])), 0)
    }
    
    covid_data_submission[["ConfirmedCases"]][covid_data_submission[["ForecastId"]] %in% test[["ForecastId"]]] <-
      ifelse(forecast.cc[["upper"]] > 0, round(forecast.cc[["upper"]]), 0)
    
    rm(forecast.cc)
  }
  
  ### Fatalities Forecasting
  
  if (all(train$Fatalities == 0)) {
    
    covid_data_submission[["Fatalities"]][covid_data_submission[["ForecastId"]] %in% test[["ForecastId"]]] <- 0
    next()
    
  } else {
    
    fit.fat <- train(form=as.formula("Fatalities ~ ConfirmedCases"),
                     data = train,
                     method = "bayesglm",
                     trControl=trainControl(method="repeatedcv", number=8, repeats=5))
    
    preds <- predict(fit.fat, test)
    test$Fatalities <- ifelse(preds > 0, as.numeric(round(preds)), 0)
    covid_data_submission[["Fatalities"]][covid_data_submission[["ForecastId"]] %in% test[["ForecastId"]]] <- ifelse(preds > 0, round(preds), 0)
    
    preds2 <- predict(fit.fat, train)
    preds2[preds2 < 0] <- 0
    cat("RMSLE :-  ", round(rmsle(train$Fatalities, preds2), 4), "\n\n")
    
    rm(preds)
    rm(preds2)
    
  }
  
}

covid_data_submission[["ConfirmedCases"]] <- as.integer(covid_data_submission[["ConfirmedCases"]])
covid_data_submission[["Fatalities"]] <- as.integer(covid_data_submission[["Fatalities"]])
str(covid_data_submission)

head(covid_data_submission)
tail(covid_data_submission)

write.csv(covid_data_submission, file="C:/Users/SAYAN/Desktop/submissions.csv",row.names = FALSE)

# Predicted Result