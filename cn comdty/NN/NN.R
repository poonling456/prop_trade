# install.packages('neuralnet')
# install.packages("Rblpapi")
# install.packages("quantmod")
# install.packages("candlesticks", repos="http://R-Forge.R-project.org")

library(Rblpapi)
library(quantmod)
library(candlesticks)
library(tseries)
library(ggplot2)
library(reshape2)
library(foreach)
library(doParallel)
library(plyr)
library(lubridate)
library(neuralnet)

minmax_scale_rolling=function(x,n)
{
  rollapply(x,n,function(x) (last(x,1)-min(x))/(max(x)-min(x)))
}

#data
security=to.period(dce_px_vo,'seconds')

macd=MACD(Cl(security),20,40,15)
macd_scaled=macd$macd/macd$signal-1

sar=SAR(cbind(Hi(security),Lo(security)))
sar_scaled=log(Cl(security)/sar)

#fast = 10 seconds
#slow = 2 minutes
#atr uses max-min scaling of rolling 1 day (4 hour) time frame 

dataframe=data.frame(
 `adx fast`=ADX(HLC(security),10)$ADX/100,
 `adx slow`=ADX(HLC(security),120)$ADX/100,
 `aroon`=aroon(cbind(Hi(security),Lo(security)),600)$oscillator/100,
 `atr fast`=rollapply(ATR(HLC(security),10)$atr,4*60*60,function(x) (last(x,1)-min(x))/(max(x)-min(x))),
 `atr slow`=rollapply(ATR(HLC(security),120)$atr,4*60*60,function(x) (last(x,1)-min(x))/(max(x)-min(x))),
 `cmf fast`=CMF(HLC(security),Vo(security),10),
 `cmf slow`=CMF(HLC(security),Vo(security),120),
 `cmo fast`=CMO(Cl(security),10),
 `cmo slow`=CMO(Cl(security),120),
 `macd`=macd_scaled,
 `sar`=log(Cl(security)/SAR(cbind(Hi(security),Lo(security)))),
 `roc second`=ROC(Cl(security)),
 `roc fast`=ROC(Cl(security),10),
 `roc slow`=ROC(Cl(security),120),
 `return`=lag(ROC(Cl(security),60*60),-60*60)
  )

names(dataframe)=c(
 'adx_fast',
 'adx_slow',
 'aroon',
 'atr_fast',
 'atr_slow',
 'cmf_fast',
 'cmf_slow',
 'cmo_fast',
 'cmo_slow',
 'macd',
 'sar',
 'roc_second',
 'roc_fast',
 'roc_slow',
 'return'
)

dataframe=xts(dataframe,time(security))

training='2017/2018-01'
testing='2018-02/'

set.seed(1155077345)
NN=neuralnet(formula = return ~
adx_fast+
adx_slow+
adx_fast+
adx_slow+
aroon+
atr_fast+
atr_slow+
cmf_fast+
cmf_slow+
cmo_fast+
cmo_slow+
macd+
sar+
roc_second+
roc_fast+
roc_slow,
data = na.omit(dataframe[training]),
hidden = 4,
linear.output = T)

plot(NN)

NN_result=compute(NN,na.omit(dataframe[testing])[,-ncol(dataframe)])

plot(as.numeric(na.omit(dataframe[testing])$return),NN_result$net.result,ylim = c(-0.04,0.04))

#see if sign is correct
table(sign(as.numeric(na.omit(dataframe[testing])$return))*sign(NN_result$net.result))

#absolute shit


# try to use only return, volume and time! 3 dimension



#data
security=to.period(dce_px_vo,'seconds')

roc=ROC(Cl(security))
vo=Vo(security)

past_return=cbind(lag(roc,1:4),
lag(ROC(Cl(security),5),c(5,10,15)),
lag(ROC(Cl(security),10),c(20,30)),
lag(ROC(Cl(security),20),40))
colnames(past_return)=paste('ROC',c(1:4,5,10,15,20,30,40))

past_volume=cbind(minmax_scale_rolling(lag(Vo(security),1:4),4*60*60*10),
minmax_scale_rolling(rollapply(lag(Vo(security),5),5,sum),4*60*60*10),
minmax_scale_rolling(rollapply(lag(Vo(security),10),5,sum),4*60*60*10),
minmax_scale_rolling(rollapply(lag(Vo(security),15),5,sum),4*60*60*10),
minmax_scale_rolling(rollapply(lag(Vo(security),20),10,sum),4*60*60*10),
minmax_scale_rolling(rollapply(lag(Vo(security),30),20,sum),4*60*60*10),
minmax_scale_rolling(rollapply(lag(Vo(security),40),20,sum),4*60*60*10))
colnames(past_volume)=paste('Vo',c(1:4,5,10,15,20,30,40))

past_volatility=cbind(rollapply(lag(roc),60,sd),
rollapply(lag(roc),60*10,sd),
rollapply(lag(roc),60*60,sd),
rollapply(lag(roc),60*60*4,sd),
rollapply(lag(roc),60*60*4*5,sd))
colnames(past_volatility)=c(paste('vol',c('1m','10m','1h','1d','1w')))

#volume uses max-min scaling of rolling 10 days time frame 

dataframe=data.frame(
  past_return,
  past_volume,
  past_volatility,
  ROC(lag(SMA(Cl(security),4*60*60*20))),
  log(Cl(security)/(lag(SMA(Cl(security),4*60*60*20)))),
  roc
)

dataframe=xts(dataframe,time(security))


NN_result=compute(NN,na.omit(dataframe[testing])[,-ncol(dataframe)])

plot(as.numeric(na.omit(dataframe[testing])$return),NN_result$net.result,xlim=c(-.04,0.04),ylim=c(-.04,0.04))

#see if sign is correct
table(sign(as.numeric(na.omit(dataframe[testing])$return))*sign(NN_result$net.result))

View(as.numeric(na.omit(dataframe[testing])$return)[sign(NN_result$net.result)==1])

signal_change=(rle(as.numeric(sign(NN_result$net.result))))




#learn exit time instead of return
pnl=diff(Cl(security))
hist(na.omit(pnl),breaks = min(pnl,na.rm = T):max(pnl,na.rm = T))
hist(na.omit(pnl),breaks = min(pnl,na.rm = T):max(pnl,na.rm = T),xlim = c(-10,10))

commission=0.000165
b_a_spread=1
target=2*(commission+b_a_spread/mean(dce_px_vo$px))
deadline=60*10

#price have to move by target within deadline (1 hour)
#+ve move = 1
#-ve move = -1
#otherwise = 0

period=paste(time(security)+1,time(security)+deadline,sep='/')

price_base=Cl(security)

px_label=mapply(
  function(x,y)
  {
    px=dce_px_vo$px[x]
    cross=(px>(as.numeric(y)*exp(target))) - (px<(as.numeric(y)*exp(-target)))
    output=head(cross[cross!=0],1)
    ifelse(length(output)==0,0,output)
  },
  period,
  price_base,
  SIMPLIFY = T
)

cl=Cl(security)
roc=ROC(cl)
vo=Vo(security)
vo_sma_2w=SMA(vo,4*60*60*10)

past_return=cbind(lag(roc,1:9),
                  lag(ROC(cl,5),seq(from = 10,to = 110,by = 5)),
                  lag(ROC(cl,10),seq(from = 120,to = 290,by = 10)),
                  lag(ROC(cl,20),seq(from = 300,to = 460,by = 20)),
                  lag(ROC(cl,30),seq(from = 480,to = 600,by = 30)))
colnames(past_return)=paste('ROC',seq(1,ncol((past_return))))


past_volume=cbind(minmax_scale_rolling(lag(Vo(security),1:9),4*60*60*10),
                  minmax_scale_rolling(rollapply(lag(Vo(security),seq(10,110,5)),5,sum),4*60*60*10),
                  minmax_scale_rolling(rollapply(lag(Vo(security),seq(120,290,10)),10,sum),4*60*60*10),
                  minmax_scale_rolling(rollapply(lag(Vo(security),seq(480,600,30)),20,sum),4*60*60*10))
colnames(past_volume)=paste('Vo',c(1:4,5,10,15,20,30,40))


past_volume=cbind(lag(log(vo/vo_sma_2w),1:9),
                  rollapply(lag(log(vo/vo_sma_2w),seq(10,110,5)),5,mean),
                  rollapply(lag(log(vo/vo_sma_2w),seq(120,290,10)),10,mean),
                  rollapply(lag(log(vo/vo_sma_2w),seq(300,460,20)),20,mean),
                  rollapply(lag(log(vo/vo_sma_2w),seq(480,600,30)),30,mean))
colnames(past_volume)=paste('Vo',seq(1,ncol((past_volume))))

past_volatility=cbind(rollapply(lag(roc),60,sd),
                      rollapply(lag(roc),60*10,sd),
                      rollapply(lag(roc),60*60,sd),
                      rollapply(lag(roc),60*60*4,sd),
                      rollapply(lag(roc),60*60*4*5,sd))
colnames(past_volatility)=c(paste('vol',c('1m','10m','1h','1d','1w')))

dataframe=data.frame(
  past_return,
  past_volume,
  past_volatility,
  px_label
)
dataframe=xts(dataframe,time(security))

input_name=names(dataframe)
form=as.formula(paste("px_label ~", paste(input_name[!input_name %in% "px_label"], collapse = " + ")))

training='2017/2018-01'
testing='2018-02/'

set.seed(1155077345)
stopwatch=proc.time()
NN=neuralnet(
  formula = form,
  data = na.omit(dataframe[training,]),
  hidden = 3,
  linear.output = T,
  threshold = .05)
proc.time()-stopwatch

plot(NN)

# 
# first_price_greater=mapply(
#   function(x,y) 
#   {
#     px=dce_px_vo$px[x]
#     first_greater=Position(function(z) z > y*exp(target),px)
#     first_smaller=Position(function(z) z < y*exp(-target),px)
#     if(is.na(first_greater))
#     {
#       if(is.na(first_smaller)) return(0)
#       else return(-1)
#     }
#     else
#     {
#       if(is.na(first_smaller)) return(1)
#       else return(ifelse(first_greater>first_smaller,-1,1))
#     }
#   },
#   period[1:5],
#   price_base[1:5])
# 
# first_price_smaller=mapply(
#   function(x,y) 
#   {
#     Position(function(z) z < y,dce_px_vo$px[x])
#   },
#   period,
#   price_base*exp(-target))

# 
# stopwatch=proc.time()
# temp=dce_px_vo$px[period[1261]]
# first_greater=Position(function(z) z > price_base[1]*exp(target),temp)
# first_smaller=Position(function(z) z < price_base[1]*exp(-target),temp)
# if(is.na(first_greater))
# {
#   if(is.na(first_smaller)) 0
#   else -1
# }else
# {
#   if(is.na(first_smaller)) 1
#   else ifelse(first_greater>first_smaller,-1,1)
# }
# proc.time()-stopwatch