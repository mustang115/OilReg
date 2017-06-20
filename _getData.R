library(xts)
oildata <- read.csv("OiLRegData.csv")
oildata$Date <-  as.Date(oildata$Date, format = '%m/%d/%Y')
oilxts <- xts(oildata[,-1], order.by = oildata$Date)
