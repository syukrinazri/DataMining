#UMW get symbol
library(quantmod)
library(ggplot2)
library(Rcpp)
library(rlang)
library(prophet)
library(dplyr)

#UMW Company. Attempt to check whether we can make covariance between these indexes and
#this particular stocks
CompanyCode = "4588.KL"
getSymbols(CompanyCode,rc = "yahoo")
#Fortify is to eliminate the time as index and join the df as a column
x <- `4588.KL`

#make filler for the dates that does not exist and fill with last row observation
start_date <- start(x)
end_date <- end(x)

regular_index <- seq(from = start_date, to = end_date, by = "day")
regular_xts <- xts(order.by = regular_index)

merged_xts <- merge(regular_xts, x)
merged_xts
#first data is at 3rd of january 2007
head(merged_xts)
#last data is at 31st of May 2019
tail(merged_xts,150)
#Probably need to experiment with na.spline too
filled_xts <- na.approx(merged_xts)
tail(filled_xts,150)
plot(filled_xts)

#Delete the column that is unnessesary
filled_xts$X4588.KL.Open <- NULL
filled_xts$X4588.KL.High <- NULL
filled_xts$X4588.KL.Low <- NULL
filled_xts$X4588.KL.Adjusted <- NULL
plot(filled_xts)

UMW <- fortify(filled_xts)

#Calculate for Petronas Chemical


CompanyCode = "5183.KL"
getSymbols(CompanyCode,rc = "yahoo")
#Fortify is to eliminate the time as index and join the df as a column
x <- `5183.KL`

#make filler for the dates that does not exist and fill with last row observation
start_date <- start(x)
end_date <- end(x)

regular_index <- seq(from = start_date, to = end_date, by = "day")
regular_xts <- xts(order.by = regular_index)

merged_xts <- merge(regular_xts, x)
merged_xts
#first data is at 3rd of january 2007
head(merged_xts)
#last data is at 31st of May 2019
tail(merged_xts,150)
#Probably need to experiment with na.spline too
filled_xts <- na.approx(merged_xts)

#Delete the column that is unnessesary
filled_xts$X5183.KL.Open <- NULL
filled_xts$X5183.KL.High <- NULL
filled_xts$X5183.KL.Low <- NULL
filled_xts$X5183.KL.Adjusted <- NULL
plot(filled_xts)
y <- filled_xts
PetronasChemical <- filled_xts
PetronasChemical <- fortify(PetronasChemical)

