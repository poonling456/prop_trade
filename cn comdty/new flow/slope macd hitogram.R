security=to.period(dce_px_vo_ex_opcl,period = 'minute',k = 10)

sample_period='2017/2018-01'

iter=list(
  seq(from = 6, to = 30, by = 4),
  seq(from = 70, to = 90, by = 4),
  seq(from = 100, to = 120, by = 4),
  seq(from = .985, to = .995, by = .003)
  
)

bid_ask_spread = 1
commission = 0.000165
commission_type = '*'

start_timer=proc.time()

cl=makeCluster(3, outfile='')
registerDoParallel(cl)

rpt=foreach(nFast = iter[[1]], .packages = c("quantmod", "doParallel","tseries"))%:%
  foreach(nSlow = iter[[2]], .packages = c("quantmod", "doParallel","tseries"))%:%
  foreach(nScreen = iter[[3]], .packages = c("quantmod", "doParallel","tseries"))%:%
  foreach(threshold = iter[[4]], .packages = c("quantmod", "doParallel","tseries"))%dopar%
  { 
    cat('nFast', nFast, 
        'nSlow',nSlow,
        'nSig',nSig,
        'nScreen',nScreen,
        'current time',proc.time(), 
        
        '\n', file = 'progress.txt', append = T)
    
    
    if(nFast<nSlow)
    {
      nSig=nFast
      
      MA_screen=SMA(Cl(security),n = nScreen)
      
      macd=MACD(Cl(security),nFast = nFast,nSlow = nSlow,nSig = nSig)
      
      sig_n=seriesIncr(macd$macd-macd$signal) & macd$macd<macd$signal & seriesIncr(MA_screen)
      sig_x=seriesDecr(macd$macd-macd$signal) & macd$macd>macd$signal
      
      df=bt_stoploss(security = security[sample_period],
                     sig_n = sig_n[sample_period],
                     sig_x = sig_x[sample_period],
                     stoploss = trailing_stop_pct,  
                     second_price=to.period(dce_px_vo,period = 'seconds')[sample_period],
                     threshold=threshold)
      
      performance(df,second_price=to.period(dce_px_vo,period = 'seconds')[sample_period],bid_ask_spread = bid_ask_spread,commission = commission,commission_type = commission_type)
    }
    else
    {
      list(`ledger`=NA,
           `bar return`=NA,
           `mean bar return`=NA,
           `trade`=NA,
           `win rate`=NA,
           `up day`=NA,
           `down day`=NA,
           `holding period`=NA,
           `sd`=NA,
           `sr`=NA,
           `mdd`=NA,
           `mdd period`=NA)
    }
  }
stopCluster(cl)

proc.time()-start_timer

ret_total=n_laply(rpt,FUN = 'sum(x1$`bar return`)',4)
ret_mean=n_laply(rpt,FUN = 'x1$`mean bar return`',4)
count=n_laply(rpt,FUN = 'x1$trade',4)
win_rate=n_laply(rpt, 'x1$`win rate`',4)
up_day=n_laply(rpt, 'x1$`up day`',4)
dn_day=n_laply(rpt, 'x1$`down day`',4)
dur_mean=n_laply(rpt, 'x1$`holding period`',4)
std=n_laply(rpt, 'x1$sd',4)
sr=n_laply(rpt, 'x1$sr',4)
mdd_pct=n_laply(rpt, 'x1$mdd',4)
mdd_dur=n_laply(rpt, 'x1$`mdd period`',4)

max(ret_total)
max(sr)
max(win_rate)

head(ndarray_which(ret_total,max))
head(ndarray_which(sr,max))
head(ndarray_which(win_rate,max))




### check check the chart
#one time bt
security=to.period(dce_px_vo_ex_opcl,period = 'minute',k = 10)

macd=MACD(Cl(security),nFast = 14,nSlow = 90,nSig = 14)
ema=EMA(Cl(security),112)
threshold=.985

sig_n=seriesIncr(macd$macd-macd$signal) & seriesIncr(ema) & macd$macd<macd$signal
sig_x=seriesDecr(macd$macd-macd$signal) & macd$macd>macd$signal

bid_ask_spread = 1
commission = 0.000165
commission_type = '*'

df=bt_stoploss(security = security ,
               sig_n = sig_n ,
               sig_x = sig_x,
               stoploss = no_stop,  
               second_price=to.period(dce_px_vo,period = 'seconds'))

perf=performance(df,security,bid_ask_spread = bid_ask_spread,commission = commission,commission_type = commission_type)

#annualize sr
perf$sr#*sqrt(6*5*250)
perf$trade
perf$`win rate`
perf$`holding period`

#graph
overlay_n=xts(rep(F,nrow(security)),time(security))
overlay_n[df$`entry time`]=T

overlay_x=xts(rep(F,nrow(security)),time(security))
overlay_x[nx_time(df$`exit time`,0,time_object = time(security))]=T

subset=unlist(unique(mapply(FUN = function(x,y) paste(x,y,sep='-'),as.POSIXlt(df$`entry time`)$year+1900,as.POSIXlt(df$`entry time`)$mon+1,SIMPLIFY = F)))
#subset=c('2017','2018')

pdf('price movement.pdf')
graph=sapply(subset,function(x) 
{
  tryCatch(
    {
      chartSeries(security,
                  TA = 'addTA(overlay_n,on=1,col=rgb(0,0,1,0.5));addTA(overlay_x,on=1,col=rgb(1,0,0,0.5));addMACD(14,90,14);addEMA(112)',
                  subset = x)
    },error = function (e)
    {
      print(x)
    }
  )
})
dev.off()

pdf('return.pdf')
plot(cumsum(perf$`bar return`))
dev.off()

write.csv(x = sapply(perf[-(1:2)], function(x) x), 'metrics.csv')
