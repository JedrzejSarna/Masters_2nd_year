setwd('/Users/jedrzejsarna/Desktop/II stopien 3 semestr/Time Series Analysis')

bitcoin_dataset <- read.csv('Lab1_Bitcoin_Historical_Price.csv')

#simple way without ts() object as the data is imported correctly

bitcoin_dataset$Date <- as.Date(bitcoin_dataset$Date, format = "%d-%m-%Y")
typeof(bitcoin_dataset$Date)
plot(bitcoin_dataset$Date, bitcoin_dataset$Close, xlab = "Date", ylab = "Close Price")


#ts() object try
bitcoin_TS<-ts(bitcoin_dataset$Close,start = c(2013,4), frequency = 365)
plot(bitcoin_TS)
