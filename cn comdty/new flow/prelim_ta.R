dce_1dhigh=DonchianChannel(Cl(dce_1m),n = 240)$high
dce_break_1dhigh=time(dce_1m)[which(seriesIncr(dce_1dhigh))]
graph=sapply(dce_break_1dhigh,function(x) 
{
  tryCatch(
    {
      chartSeries(dce_1m,TA='addVo();addTA(dce_1dhigh,on=1)',subset = paste(as.character(x-300),paste(as.character(as.Date(x)),' 15:00:00'),sep = '/'))
    },error = function (e)
    {
      print(paste(as.character(x),paste(as.character(as.Date(x)),' 15:00:00'),sep = '/'))
    })
})

#seems good, can be even more sensitive

dce_2hhigh=DonchianChannel(Cl(dce_1m),n = 120)$high
dce_break_2hhigh=time(dce_1m)[which(seriesIncr(dce_2hhigh))]
graph=sapply(dce_break_2hhigh,function(x) 
{
  tryCatch(
    {
      chartSeries(dce_1m,TA='addVo();addTA(dce_2hhigh,on=1)',subset = paste(as.character(x-7200),as.character(x+7200),sep = '/'))
    },error = function (e)
    {
      print(paste(as.character(x),paste(as.character(as.Date(x)),' 15:00:00'),sep = '/'))
    })
})

#breakthrough in low volume low volatility
#more sensitive!

atr=ATR(HLC(dce_1m))$atr
tr=ATR(HLC(dce_1m))$tr
vo=Vo(dce_1m)

low_rng=apply(lag(tr<10,0:9),1,all)
low_rng=xts(low_rng,order.by = as.POSIXct(names(low_rng)))

low_vo=SMA(vo,10)<SMA(vo,30)

dce_high=DonchianChannel(Cl(dce_1m),n = 30)$high

dce_break=seriesIncr(dce_high)&lag(low_rng)&lag(low_vo)

graph=sapply(unique(as.Date(time(dce_1m)[which(dce_break)])),function(x) 
{
  tryCatch(
    {
      chartSeries(dce_1m,TA='addTA(SMA(vo,30));addTA(SMA(vo,10),on=2,col="blue");addTA(vo,on=2,col="green");addTA(DonchianChannel(Cl(dce_1m),n = 30),on=1);addTA(tr);addTA(dce_break)',subset = as.character(x))
    },error = function (e)
    {
      print(x)
    })
})


graph=sapply(unique(as.Date(time(dce_1m))),function(x) 
{
  tryCatch(
    {
      chartSeries(dce_1m,TA='addTA(DonchianChannel(Cl(dce_1m)),on=1);addSMA(60);addVo()',subset = as.character(x))
    },error = function (e)
    {
      print(x)
    })
})

dce_1d=to.period(dce_px_vo,
                 period = 'days')
