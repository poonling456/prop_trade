library(Rblpapi)
library(quantmod)
library(tseries)
# benchmark=read.csv('benchmark.csv')
# benchmark=xts(benchmark[,-1],as.Date(as.character(benchmark[,1]),format = '%m/%d/%Y'))

load("C:/Users/Anthony/Desktop/ongoing/hsi oversold strat/hsi data.RData")

blpConnect()

start.date=as.Date(paste(2000:2017,"-01-01",sep=""))
end.date=as.Date(paste(2000:2017,"-12-31",sep=""))

constituent=lapply(start.date,FUN = function(x){levels(beqs("HSI",screenType = "PRIVATE",date = x)$Ticker)})

rawdata=mapply(function(constituent,start.date,end.date){bdh(constituent,c("PX_OPEN","PX_HIGH","PX_LOW","PX_LAST","VOLUME"),start.date = start.date,end.date =end.date )},constituent,start.date,end.date)
rawdata=lapply(rawdata, function(x)
{
  temp=lapply(x,function(y){`names<-`(y,c("Date","Open","High","Low","Close","Volume"))})
  `names<-`(lapply(temp, function(y){xts(OHLCV(y),as.Date(y$Date))}),names(temp))
})

price=rawdata[[1]]
for(i in 2:length(rawdata))
{
  name12=intersect(names(price),names(rawdata[[i]]))
  name1=setdiff(names(price),names(rawdata[[i]]))
  name2=setdiff(names(rawdata[[i]]),names(price))
  
  temp=`names<-`(lapply(name12, function(x){rbind.xts(price[[x]],rawdata[[i]][[x]])}),name12)
  temp=append(temp,price[name1])
  temp=append(temp,rawdata[[i]][name2])
  
  price=temp
}

name=names(price)

atr=cbind.n(sapply(price,function(x){ATR(HLC(x))$atr}))
rsi9=cbind.n(sapply(price,function(x){RSI(Cl(x),9)}))
cl=cbind.n(sapply(price,function(x){Cl(x)}))
clcl=cbind.n(sapply(price,function(x){ClCl(x)}))
sma40=cbind.n(sapply(price,function(x){SMA(Cl(x),40)}))
sma80=cbind.n(sapply(price,function(x){SMA(Cl(x),80)}))
pctB=cbind.n(sapply(price,function(x){BBands(HLC(x))$pctB}))
pctB_15low=cbind.n(sapply(price,function(x){DonchianChannel(BBands(HLC(x))$pctB,15)$low}))


date=time(cl)
benchmark_cl=Cl(benchmark)
benchmark_sma40=SMA(benchmark_cl,40)
benchmark_sma80=SMA(benchmark_cl,80)

benchmark_ret=na.omit(ROC(Cl(benchmark)))
benchmark_ret_cu=cumsum(benchmark_ret)

comm=0
