#These code will give a csv with Percentage of 1 year profit 

library(quantmod)
library(ggplot2)
library(Rcpp)
library(rlang)
library(prophet)
library(dplyr)


CompanyDetails <- read.csv("CompanyDetailsLatest.csv")
head(CompanyDetails)

#Declare the array for currentprice and the price a week after

CurrentPrice <- c()
PredictionWeek <- c()
PredictionMonth <- c()
PredictionYear <- c()

for (i in 1:32){
  CompanyCode <- CompanyDetails$Symbol[i]
  CompanyAbbr <- CompanyDetails$Abbr[i]
  #Placeholder of the dataframe 
  y <- getSymbols(as.character(CompanyCode),rc = "yahoo", auto.assign = F)
  CompanyCodePlusDot <- paste(CompanyCode, ".", sep = "")
  colnames(y) <- gsub(CompanyCodePlusDot,"",colnames(y))
  assign(as.character(CompanyAbbr), y)
  #Fortify is to eliminate the time as index and join the df as a column
  y<- na.approx(y)
  y <- fortify(y)
  
  colnames(y)[1] <- "ds"
  colnames(y)[5] <- "y"
  plot(y$ds,y$y, main = as.character(CompanyAbbr), ylab = "StockPrice", xlab = "Time")
  Add <- y %>% filter(ds == "2019-05-27") %>% pull(y)
  CurrentPrice <- c(CurrentPrice, Add)
  
  #The prophet part of the code starts here
  D <- prophet(y)
  future <- make_future_dataframe(D, freq = "day" ,periods = 365)
  forecast <- predict(D, future)
  forecast2 <- forecast
  forecast2$ds <- as.Date(forecast2$ds)
  Add2 <- forecast2 %>% filter(ds == "2019-06-03") %>% pull(yhat)
  PredictionWeek <- c(PredictionWeek, Add2)
  Add3 <- forecast2 %>% filter(ds == "2019-06-27") %>% pull(yhat)
  PredictionMonth <- c(PredictionMonth, Add3)
  Add4 <- forecast2 %>% filter(ds == "2020-05-26") %>% pull(yhat)
  PredictionYear <- c(PredictionYear, Add4)
  plot(D,forecast,ylab = "StockPrice", xlab = "time")
}

 #Make new data frame with new predictions and the company name
CompanyName <- CompanyDetails$Abbr
head(CompanyName)
StockPricePrediction <- data_frame("Company" = CompanyName, "Current" = CurrentPrice, 
                                   "Week" = PredictionWeek, "Month" = PredictionMonth, 
                                   "Year" = PredictionYear)
head(StockPricePrediction)
tail(StockPricePrediction)

StockPricePrediction$WeekGainPercentage <- (StockPricePrediction$Week - StockPricePrediction$Current)/StockPricePrediction$Current*100
StockPricePrediction$MonthGainPercentage <- (StockPricePrediction$Month - StockPricePrediction$Current)/StockPricePrediction$Current*100
StockPricePrediction$YearGainPercentage <- (StockPricePrediction$Year - StockPricePrediction$Current)/StockPricePrediction$Current*100
StockPricePrediction %>% arrange(desc(WeekGainPercentage))
StockPricePrediction %>% arrange(desc(MonthGainPercentage))
A <- StockPricePrediction %>% arrange(desc(YearGainPercentage))
write.csv(A, "PredictionTop10-OneYearRanking2.csv")
