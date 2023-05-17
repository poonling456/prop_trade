#analysis

dce_10m=to.period(dce_px_vo,
                  period = 'minute',
                  k=10,
                  OHLC = TRUE)
names(dce_10m)=c('Open','High','Low','Close','Volume')


security=dce_10m
time_plot=time(security)
subset=paste(unique(round_date(as.Date(time_plot),unit = 'week')),unique(round_date(as.Date(time_plot),unit = 'week'))+4,sep = '/')

signal_n=seriesIncr(SMA(Cl(security),24))
signal_x=!signal_n

graph=sapply(subset,function(x) 
{
  tryCatch(
    {
      chartSeries(dce_10m,TA='addSMA(24);addTA(signal_n,col=rgb(0,0,1,0.2))',subset = x,yrange = c(min(Lo(dce_10m[x]))*0.98,max(Lo(dce_10m[x]))*1.02))
    },error = function (e)
    {
      print(x)
    })
})



###macd


macd=MACD(Cl(security),nFast = 9,nSlow = 14,nSig = 5)
signal_n=macd$macd > macd$signal



time_plot=time(security)


subset=paste(unique(floor_date(as.Date(time_plot),unit = 'month')),unique(ceiling_date(as.Date(time_plot),unit = 'month'))+4,sep = '/')


graph=sapply(subset,function(x) 
{
  tryCatch(
    {
      chartSeries(dce_10m,TA='addMACD(9,14,5);addTA(signal_n,on=1,col=rgb(0,0,1,0.2))',subset = x,yrange = c(min(Lo(dce_10m[x]))*0.98,max(Lo(dce_10m[x]))*1.02))
    },error = function (e)
    {
      print(x)
    })
})
#macd seems to work quite well


#try try with optimatized params 12 26 4
macd=MACD(Cl(security),nFast = 12,nSlow = 26,nSig = 4)
signal_n=macd$macd > macd$signal


subset=paste(unique(floor_date(as.Date(time_plot),unit = 'month')),unique(ceiling_date(as.Date(time_plot),unit = 'month'))+4,sep = '/')


graph=sapply(subset,function(x) 
{
  tryCatch(
    {
      chartSeries(dce_10m,TA='addMACD(12,26,4);addTA(signal_n,on=1,col=rgb(0,0,1,0.2))',subset = x,yrange = c(min(Lo(dce_10m[x]))*0.98,max(Lo(dce_10m[x]))*1.02))
    },error = function (e)
    {
      print(x)
    })
})
#need more decisive cut loss





#try macd hist
macd=MACD(Cl(security),nFast = 24,nSlow = 82,nSig = 30)
signal_n=seriesIncr(macd$macd - macd$signal)


time_plot=time(security)
subset=paste(unique(floor_date(as.Date(time_plot),unit = 'month')),unique(ceiling_date(as.Date(time_plot),unit = 'month'))+4,sep = '/')


graph=sapply(subset,function(x) 
{
  tryCatch(
    {
      chartSeries(dce_10m,TA='addMACD(24,82,30);addTA(signal_n,on=1,col=rgb(0,0,1,0.2))',subset = x,yrange = c(min(Lo(dce_10m[x]))*0.98,max(Lo(dce_10m[x]))*1.02))
    },error = function (e)
    {
      print(x)
    })
})




trade=bt_basic(sig_n = signal_n)
metrics=performance(trade)





#buy only when macd hist is negative

macd_hist=macd$macd - macd$signal

signal_n=seriesIncr(macd_hist) & macd_hist<0
signal_x=seriesDecr(macd_hist)

trade2=bt_basic(sig_n = signal_n ,sig_x = signal_x )
metrics2=performance(trade2)


trade3=bt_stoploss(sig_n = signal_n ,sig_x = signal_x,stoploss = trailing_stop_pct,  tick_price=dce_px_vo$px )
metrics2=performance(trade2)

time_plot=time(security)
subset=paste(trade2$`entry date`-240,trade2$`exit date`+240,sep = '/')


graph=sapply(subset,function(x) 
{
  tryCatch(
    {
      chartSeries(dce_10m,TA='addMACD(24,82,30)',subset = x)
    },error = function (e)
    {
      print(x)
    })
})
#double the sr, but still can use a prompted exit and stop loss
