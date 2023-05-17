dce_roc=ROC(Cl(dce_1m))
plot(x=as.numeric(dce_roc),y=as.numeric(lag(dce_roc)),xlim = c(-0.01,0.01),ylim = c(-0.01,0.01))
#no auto correlation


dce_gapup=time(dce_1m)[CSPGap(dce_1m)$GapUp & as.POSIXlt(time(dce_1m))$hour!=8]
graph=sapply(dce_gapup,function(x) 
{
  tryCatch(
  {
    chartSeries(dce_1m,subset = paste(as.character(x-120),paste(as.character(as.Date(x)),' 15:00:00'),sep = '/'))
  },error = function (e)
  {
    print(paste(as.character(x),paste(as.character(as.Date(x)),' 15:00:00'),sep = '/'))
  })
})
#gap useless

dce_5sma20=time(dce_1m)[which(Vo(dce_1m) > 5*SMA(Vo(dce_1m),20))]
graph=sapply(dce_5sma20,function(x) 
{
  tryCatch(
    {
      chartSeries(dce_1m,TA = 'addTA(Vo(dce_1m));addTA(SMA(Vo(dce_1m),20)*5,on=2)',subset = paste(as.character(x-90),paste(as.character(as.Date(x)),' 15:00:00'),sep = '/'))
    },error = function (e)
    {
      print(paste(as.character(x),paste(as.character(as.Date(x)),' 15:00:00'),sep = '/'))
    })
})
#abnormal vo

pt10=time(dce_1m)[which(diff(Cl(dce_1m)) > 10)]
graph=sapply(pt10,function(x) 
{
  tryCatch(
    {
      chartSeries(dce_1m,TA = 'addTA(Vo(dce_1m));addTA(SMA(Vo(dce_1m),20),on=2)',subset = paste(as.character(x-90),as.character(x+300),sep = '/'))
    },error = function (e)
    {
      print(paste(as.character(x),paste(as.character(as.Date(x)),' 15:00:00'),sep = '/'))
    })
})

before=diff(Cl(dce_1m))[pt10]
after=lag(Cl(dce_1m)-Op(dce_1m),-1)[pt10]
table(before>0,after>0)
plot(density(after))

drawdown=lag(Hi(dce_1m)-lag(Cl(dce_1m)),-1)[pt10]
cutloss_10=ifelse(drawdown>10,10,after)
plot(density(cutloss_10))
sum(cutloss_10)
mean(cutloss_10)

cutloss_8=ifelse(drawdown>8,8,after)
plot(density(cutloss_8))
sum(cutloss_8)
mean(cutloss_8)

cutloss_12=ifelse(drawdown>12,12,after)
plot(density(cutloss_12))
sum(cutloss_12)
mean(cutloss_12)

cutloss_15=ifelse(drawdown>15,15,after)
plot(density(cutloss_15))
sum(cutloss_15)
mean(cutloss_15)
#occ 137
#most of the time win, but lose big
#cutloss at 12 seems effective



pt8=time(dce_1m)[which(diff(Cl(dce_1m)) > 8)]
graph=sapply(pt8,function(x) 
{
  tryCatch(
    {
      chartSeries(dce_1m,TA = 'addTA(Vo(dce_1m));addTA(SMA(Vo(dce_1m),20),on=2)',subset = paste(as.character(x-90),as.character(x+300),sep = '/'))
    },error = function (e)
    {
      print(paste(as.character(x),paste(as.character(as.Date(x)),' 15:00:00'),sep = '/'))
    })
})

before=diff(Cl(dce_1m))[pt8]
after=lag(Cl(dce_1m)-Op(dce_1m),-1)[pt8]
table(before>0,after>0)
plot(density(after))

drawdown=lag(Hi(dce_1m)-lag(Cl(dce_1m)),-1)[pt8]

cutloss_8=ifelse(drawdown>8,8,after)
plot(density(cutloss_8))
sum(cutloss_8)
mean(cutloss_8)

cutloss_10=ifelse(drawdown>10,10,after)
plot(density(cutloss_10))
sum(cutloss_10)
mean(cutloss_10)

cutloss_12=ifelse(drawdown>12,12,after)
plot(density(cutloss_12))
sum(cutloss_12)
mean(cutloss_12)

cutloss_15=ifelse(drawdown>15,15,after)
plot(density(cutloss_15))
sum(cutloss_15)
mean(cutloss_15)
#occ 243
#most of the time win, but lose big
#cutloss at 12 seems effective



pt6=time(dce_1m)[which(diff(Cl(dce_1m)) > 6)]
graph=sapply(pt6,function(x) 
{
  tryCatch(
    {
      chartSeries(dce_1m,TA = 'addTA(Vo(dce_1m));addTA(SMA(Vo(dce_1m),20),on=2)',subset = paste(as.character(x-90),as.character(x+300),sep = '/'))
    },error = function (e)
    {
      print(paste(as.character(x),paste(as.character(as.Date(x)),' 15:00:00'),sep = '/'))
    })
})

before=diff(Cl(dce_1m))[pt6]
after=lag(Cl(dce_1m)-Op(dce_1m),-1)[pt6]
table(before>0,after>0)
plot(density(after))

drawdown=lag(Hi(dce_1m)-lag(Cl(dce_1m)),-1)[pt6]

cutloss_8=ifelse(drawdown>8,8,after)
plot(density(cutloss_8))
sum(cutloss_8)
mean(cutloss_8)

cutloss_10=ifelse(drawdown>10,10,after)
plot(density(cutloss_10))
sum(cutloss_10)
mean(cutloss_10)

cutloss_12=ifelse(drawdown>12,12,after)
plot(density(cutloss_12))
sum(cutloss_12)
mean(cutloss_12)

cutloss_15=ifelse(drawdown>15,15,after)
plot(density(cutloss_15))
sum(cutloss_15)
mean(cutloss_15)
#occ 435
#most of the time win, but lose big
#cutloss at 12 seems effective


# wont cover average 1-2 pt of bid ask spread and 0.25pt of transaction fee


atr20=ATR(HLC(dce_1m),20)
tr20=atr20$tr
atr20=atr20$atr

signal=tr20/lag(atr20)>2 & tr20/lag(atr20)<3
graph=sapply(na.omit(unique(as.Date(time(dce_1m)[signal]))),function(x) 
{
  tryCatch(
    {
      chartSeries(dce_1m,TA = 'addTA(signal);addVo();addSMA(60);addSMA(120,col="blue")',subset = as.character(x))
    },error = function (e)
    {
      print(paste(as.character(x),paste(as.character(as.Date(x)),' 15:00:00'),sep = '/'))
    })
})

#not working


channel=DonchianChannel(Cl(dce_1m),5)
rng=channel$high-channel$low
breakout=seriesIncr(channel$high) & (Vo(dce_1m) > 3*SMA(Vo(dce_1m),10))

signal_n=lag(rng<5) & breakout
signal_x=seriesDecr(channel$low)

graph=sapply(na.omit(unique(as.Date(time(dce_1m)[signal_n]))),function(x) 
{
  tryCatch(
    {
      chartSeries(dce_1m,TA = 'addTA(signal_n,on=1,col=rgb(0,0,0.8,0.5));addTA(signal_x,on=1,col=rgb(0.5,0,0,0.5));addVo();addSMA(60);addSMA(120,col="blue")',subset = as.character(x))
    },error = function (e)
    {
      print(paste(as.character(x),paste(as.character(as.Date(x)),' 15:00:00'),sep = '/'))
    })
})
#seems to be working