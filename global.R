library(lubridate)
library(caret)
library(forecast)
library(AppliedPredictiveModeling)
library(e1071)
library(TTR)
library(DMwR)
library(neuralnet)

getSymbols<- function()
{
  fileUrl<-"http://finviz.com/export.ashx?v=152&f=idx_sp500&ft=1&ta=1&p=d&r=1&c=1"
  temp <- tempfile()
  download.file(fileUrl,temp, method="curl")
  symbols <- read.csv(temp)
}

getPrices<- function(symbol)
{
  #symbol<-'AAPL'
  today<-Sys.Date()
  fileUrl<-paste("http://ichart.yahoo.com/table.csv?s=",symbol,sep="")
  temp <- tempfile()
  download.file(fileUrl,temp, method="curl")
  prices <- read.csv(temp)
  prices$Date<-as.Date(prices$Date)
  prices <- prices[prices$Date >= today-180,]
  prices <- prices[order(prices$Date),] 
  prevrows <- function(data,n) {sapply(1:n,function(x) c(rep(NA,x),head(data,-x)))}
  
  prices$Day <- as.numeric(as.POSIXlt(prices$Date)$wday)
  
  prices$PrevClose <- prevrows(prices$Close,1)
  prices$Change <- prices$Close - prices$PrevClose
  prices$Movement<-ifelse(prices$Change > 0 ,1 , -1)
  prices$SMA50<-SMA(prices$Close, n=50)
  prices$SMA10<-SMA(prices$Close, n=10)
  prices$SMA5<-SMA(prices$Close, n=5)
  prices$SMA3<-SMA(prices$Close, n=3)
  prices$EMA50<-EMA(prices$Close, n=50)
  prices$EMA10<-EMA(prices$Close, n=10)
  prices$EMA5<-EMA(prices$Close, n=5)
  prices$EMA3<-EMA(prices$Close, n=3)
  prices$RSI50<-RSI(prices$Close, n=50)
  prices$RSI10<-RSI(prices$Close, n=10)
  prices$RSI5<-RSI(prices$Close, n=5)
  prices$RSI3<-RSI(prices$Close, n=3)
  prices$VOLEMA50<-EMA(prices$Volume, n=50)
  prices$VOLEMA10<-EMA(prices$Volume, n=10)
  prices$VOLEMA5<-EMA(prices$Volume, n=5)
  prices$VOLEMA3<-EMA(prices$Volume, n=3)
  prices$OBV<-OBV(prices$Close, prices$Volume)
  
  HLC<-as.matrix(cbind(prices$High, prices$Low, prices$Close))
  prices$ATR<-ATR(HLC, n = 14)[,1]
  
  prices$MACD<-as.data.frame((MACD(prices$Close)))$macd
  
  prices<-prices[ which(complete.cases(prices)==TRUE),]
  
  today<-Sys.Date()
  training = prices[prices$Date < as.Date(today-30),]
  trainNN<-as.data.frame(cbind(training$Close,training$PrevClose,training$Day,training$SMA50,training$SMA10,training$SMA5,training$SMA3,training$EMA50,training$EMA10,training$EMA5,training$EMA3,training$RSI50,training$MACD,training$OBV,training$Change
  ))
  
  trainNN<-trainNN[ which(complete.cases(trainNN)==TRUE),]
  trainNN<-cbind(trainNN$V1, trainNN$V2
                 , prevrows(trainNN$V3,1)
                 , prevrows(trainNN$V4,1)
                 , prevrows(trainNN$V5,1)
                 , prevrows(trainNN$V6,1)
                 , prevrows(trainNN$V7,1)
                 , prevrows(trainNN$V8,1)
                 , prevrows(trainNN$V9,1)
                 , prevrows(trainNN$V10,1)
                 , prevrows(trainNN$V11,1)
                 , prevrows(trainNN$V12,1)
                 , prevrows(trainNN$V13,1)
                 , prevrows(trainNN$V14,1)
                 , prevrows(trainNN$V15,1)
  )
  trainNN<-trainNN[ which(complete.cases(trainNN)==TRUE),]
  trainNNScaled<-as.data.frame(scale(lag(as.matrix(trainNN),1)))
  
  n <- names(trainNNScaled)
  f <- as.formula(paste("V1 ~", paste(n[!n %in% "V1"], collapse = " + ")))
  set.seed(12345)
  net <- neuralnet(f,data=trainNNScaled, hidden=4, threshold=.01, rep=1)
  
  testing = prices[prices$Date >= as.Date(today-30),]
  lastTrainingDay<-as.data.frame(tail(trainNN,1))
  testNN<-rbind(lastTrainingDay,as.data.frame(cbind(testing$Close,testing$PrevClose,testing$Day,testing$SMA50,testing$SMA10,testing$SMA5,testing$SMA3,testing$EMA50,testing$EMA10,testing$EMA5,testing$EMA3,testing$RSI50,testing$MACD,testing$OBV,testing$Change
  )))
  
  nextwday <- function(dy) {
    ifelse(dy==5, 1, dy<-dy+1)
  }
  
  testNN<-cbind(testNN$V1, testNN$V2
                , prevrows(testNN$V3,1)
                , prevrows(testNN$V4,1)
                , prevrows(testNN$V5,1)
                , prevrows(testNN$V6,1)
                , prevrows(testNN$V7,1)
                , prevrows(testNN$V8,1)
                , prevrows(testNN$V9,1)
                , prevrows(testNN$V10,1)
                , prevrows(testNN$V11,1)
                , prevrows(testNN$V12,1)
                , prevrows(testNN$V13,1)
                , prevrows(testNN$V14,1)
                , prevrows(testNN$V15,1)
  )
  testNN<-testNN[ which(complete.cases(testNN)==TRUE),]
  lastDay<-as.data.frame(tail(testing,1))
  nextDay<-as.data.frame(cbind(0,lastDay$Close,nextwday(as.data.frame(tail(testNN,1))$V3),lastDay$SMA50,lastDay$SMA10,lastDay$SMA5,lastDay$SMA3,lastDay$EMA50,lastDay$EMA10,lastDay$EMA5,lastDay$EMA3,lastDay$RSI50,lastDay$MACD,lastDay$OBV,lastDay$Change
  ))
  testNN<-rbind(testNN,nextDay)
  
  for (i in 1:5){
    testNNScale <- scale(testNN, center = attr(scale(trainNN), 'scaled:center'), scale = attr(scale(trainNN), 'scaled:scale'))
    
    net.results <- compute(net, as.data.frame(testNNScale)[-1])
    results<-unscale(as.matrix(net.results$net.result)[,1], testNNScale)
    
    if(i==1){
      cleanoutput <- cbind(testNN$V1,as.data.frame(results), abs(testNN$V1-as.data.frame(results))/testNN$V1*100)
      colnames(cleanoutput) <- c("Expected Output","Neural Net Output", "Error %")
      print(cleanoutput)
      
      print(sum(cleanoutput[which(cleanoutput$"Error %" < 100),]$"Error %")/(nrow(cleanoutput)-1))
    }
    
    testNN[nrow(testNN),1]<-tail(as.data.frame(results),1)
    
    p<-rbind(trainNN,testNN)
    SMA50<-tail(SMA(p$V1, n=50),1)
    SMA10<-tail(SMA(p$V1, n=10),1)
    SMA5<-tail(SMA(p$V1, n=5),1)
    SMA3<-tail(SMA(p$V1, n=3),1)
    EMA50<-tail(EMA(p$V1, n=50),1)
    EMA10<-tail(EMA(p$V1, n=10),1)
    EMA5<-tail(EMA(p$V1, n=5),1)
    EMA3<-tail(EMA(p$V1, n=3),1)
    RSI50<-tail(RSI(p$V1, n=50),1)
    MACD<-tail(as.data.frame((MACD(p$V1)))$macd,1)
    OBV<-tail(p$V14,1)
    Change<-tail(as.data.frame(results),1)-head(tail(as.data.frame(results),2),1)
    
    nextDay<-as.data.frame(cbind(0,tail(as.data.frame(results),1),nextwday(as.data.frame(tail(testNN,1))$V3),SMA50,SMA10,SMA5,SMA3,EMA50,EMA10,EMA5,EMA3,RSI50,MACD,OBV,Change
    ))
    
    names(nextDay) <- names(testNN)
    testNN<-rbind(testNN,nextDay)
  }
  testNN<-testNN[-nrow(testNN),] 
  
  tail(prices,1)$Date
  
  predicts <- as.data.frame(cbind(as.Date(as.data.frame(rbind(as.Date(tail(prices,1)$Date)+1, 
                                                              as.Date(tail(prices,1)$Date)+2,
                                                              as.Date(tail(prices,1)$Date)+3,
                                                              as.Date(tail(prices,1)$Date)+4,
                                                              as.Date(tail(prices,1)$Date)+5))$V1),as.data.frame(tail(testNN,5)$V1)
  ))
  colnames(predicts) <- c("Date", "Close")
  actuals <- prices[, c("Date", "Close")]
  prices<-rbind(actuals,predicts)
}
