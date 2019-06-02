#Other stocks untuk focus kt satu je stocks
library(quantmod)
library(ggplot2)
library(Rcpp)
library(rlang)
library(prophet)
library(dplyr)
library(ggplot2)

#Crude Oil
CompanyCode = "CLN19.NYM"
getSymbols(CompanyCode,rc = "yahoo")
#Fortify is to eliminate the time as index and join the df as a column
x <- `CLN19.NYM`
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
filled_xts$CLN19.NYM.Open <- NULL
filled_xts$CLN19.NYM.High <- NULL
filled_xts$CLN19.NYM.Low <- NULL
filled_xts$CLN19.NYM.Adjusted <- NULL
filled_xts$CLN19.NYM.Volume <- NULL
plot(filled_xts)

colnames(filled_xts)

#Use fortify to get the time as a column

filled_xts <- fortify(filled_xts)
Oil <- filled_xts

#Gold
CompanyCode = "XAU"
getSymbols(CompanyCode,rc = "yahoo")
#Fortify is to eliminate the time as index and join the df as a column
x <- `XAU`

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

#Delete the column that is unnessesary
filled_xts$XAU.Open <- NULL
filled_xts$XAU.High <- NULL
filled_xts$XAU.Low <- NULL
filled_xts$XAU.Volume <- NULL
filled_xts$XAU.Adjusted <- NULL
plot(filled_xts)
filled_xts <- fortify(filled_xts)

#Put in variable to use with the cleaned stocks data
Gold <- filled_xts

#MYRtoDollar
CompanyCode = "MYRUSD=X"
getSymbols(CompanyCode,rc = "yahoo")
#Fortify is to eliminate the time as index and join the df as a column
x <- `MYRUSD=X`

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

#Delete the column that is unnessesary
filled_xts$MYRUSD.X.Open <- NULL
filled_xts$MYRUSD.X.High <- NULL
filled_xts$MYRUSD.X.Low <- NULL
filled_xts$MYRUSD.X.Volume <- NULL
filled_xts$MYRUSD.X.Adjusted <- NULL
plot(filled_xts)
filled_xts <- fortify(filled_xts)

USD <- filled_xts




#KLSE Index

CompanyCode = "^KLSE"
getSymbols(CompanyCode,rc = "yahoo")
#Fortify is to eliminate the time as index and join the df as a column
x <- KLSE
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

#Delete the column that is unnessesary
filled_xts$KLSE.Open <- NULL
filled_xts$KLSE.High <- NULL
filled_xts$KLSE.Low <- NULL
filled_xts$KLSE.Volume <- NULL
filled_xts$KLSE.Adjusted <- NULL
plot(filled_xts)
filled_xts <- fortify(filled_xts)

KLSE <- filled_xts

#Trim all 4 to unite to the same Index date value
A <- inner_join(USD,Gold, by = "Index")
B <- inner_join(A, Oil, by = "Index")
C <- inner_join(B, KLSE, by = "Index")
#Check NA whether it exist or not
table(is.na(C))

head(USD)
head(Gold)
head(Oil)
head(KLSE)
head(C)


#Combine the other data with UMW stocks 
D <- inner_join(C, UMW, by = "Index")
head(D)
#Get rid of the index to calculate the relation between variable

E <- D[-1]
head(E)
#Renaming column for easier understanding
E <- E %>% rename(Gold = XAU.Close, 
                  Oil = CLN19.NYM.Close, 
                  KLSE = KLSE.Close, 
                  USD = MYRUSD.X.Close, 
                  UMWStocks = X4588.KL.Close,
                  StockVolumeTraded = X4588.KL.Volume)
CompleteUMW <- E
pairs(CompleteUMW)
cor(CompleteUMW, method = "spearman")
cov(CompleteUMW, method = "spearman")

ggplot(CompleteUMW, aes(UMWStocks, USD)) + geom_point()
ggplot(CompleteUMW, aes(UMWStocks, Oil)) + geom_point()


#Calculate for Petronas Chemical instead
D <- inner_join(C, PetronasChemical, by = "Index")

plot(D)
#Get rid of the index to calculate the relation between variable

E <- D[-1]

#Renaming column for easier understanding
E <- E %>% rename(Gold = XAU.Close, 
                  Oil = CLN19.NYM.Close, 
                  KLSE = KLSE.Close, 
                  USD = MYRUSD.X.Close, 
                  PetronasChemical = X5183.KL.Close)
CompletePetronasChemical <- E
pairs(CompletePetronasChemical)
cor(CompletePetronasChemical, method = "spearman")
cov(CompletePetronasChemical, method = "spearman")

ggplot(CompletePetronasChemical, aes(PetronasChemical, USD)) + geom_point()
ggplot(CompletePetronasChemical, aes(PetronasChemical, KLSE)) + geom_point()

