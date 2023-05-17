#best sr 0.75, win rate 33, arround slow 82 fast 30 sig 30 (before transaction cost)
#buy only when macd hist -ve double the sr! (before transaction cost)


#one time bt
security=to.period(dce_px_vo_ex_opcl,period = 'minute',k = 10)

macd=MACD(Cl(security),nFast = 30,nSlow = 80,nSig = 30)
macd_hist=macd$macd-macd$signal

sig_n=seriesIncr(macd_hist) & macd_hist<0
sig_x=lag(macd_hist>0) & macd_hist<0

bid_ask_spread = 1
commission = 0.000165
commission_type = '*'

df=bt_stoploss(security = security ,
               sig_n = sig_n ,
               sig_x = sig_x,
               stoploss = trailing_stop_pct,  
               second_price=to.period(dce_px_vo,period = 'seconds'),
               threshold=.995)

perf=performance(df,security,bid_ask_spread = bid_ask_spread,commission = commission,commission_type = commission_type)

#annualize sr
perf$sr*sqrt(6*5*250)
perf$trade
perf$`win rate`

#graph
overlay_n=xts(rep(F,nrow(security)),time(security))
overlay_n[df$`entry time`]=T

overlay_x_sig=xts(rep(F,nrow(security)),time(security))
overlay_x_sig[nx_time(df$`exit time`,0,time_object = time(security))]=T

subset=unlist(unique(mapply(FUN = function(x,y) paste(x,y,sep='-'),a$year+1900,a$mon+1,SIMPLIFY = F)))
#subset=c('2017','2018')

graph=sapply(subset,function(x) 
{
  tryCatch(
    {
      chartSeries(security,
                  TA = 'addTA(overlay_n,on=1,col=rgb(0,0,1,0.5));addTA(overlay_x,on=1,col=rgb(1,0,0,0.5));addMACD(30,80,30)',
                  subset = x)
    },error = function (e)
    {
      print(x)
    }
  )
})
  


#no stop loss
security=to.period(dce_px_vo_ex_opcl,period = 'minute',k = 10)

nFast=30
nSlow=80
nSig=30

macd=MACD(Cl(security),nFast = nFast,nSlow = nSlow,nSig = nSig)
macd_hist=macd$macd-macd$signal

macd_signal_entry=cbind(seriesIncr(macd_hist), seriesIncr(macd_hist) & macd_hist<0)
macd_signal_exit=cbind(seriesDecr(macd_hist), seriesDecr(macd_hist) & macd_hist>0)

iter1=seq(from = 1, to = 2, by = 1)
iter2=seq(from = 1, to = 2, by = 1)

bid_ask_spread = 1
commission = 0.000165
commission_type = '*'

start_timer=proc.time()

cl=makeCluster(3, outfile='')
registerDoParallel(cl)

rpt=foreach(sig_n_index = iter1, .packages = c("quantmod", "doParallel","tseries"))%:%
  foreach(sig_x_index = iter2, .packages = c("quantmod", "doParallel","tseries"))%dopar%
  { 
    cat('sig_n_index', sig_n_index, 'sig_x_index',sig_x_index, '\n',file = 'progress.txt',append = T)
    
    if(T)
    {
      sig_n=macd_signal_entry[,sig_n_index]
      sig_x=macd_signal_exit[,sig_x_index]
      
      df=bt_stoploss(security = security ,
                     sig_n = sig_n ,
                     sig_x = sig_x,
                     stoploss = no_stop,  
                     second_price=to.period(dce_px_vo,period = 'seconds'))
      
      performance(df,security,bid_ask_spread = bid_ask_spread,commission = commission,commission_type = commission_type)
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


ret_mean=laply(rpt, function(x) laply(x, function(y) y$`mean bar return`))
count=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['trade'])))
win_rate=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z$`win rate`)))
up_day=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['up day'])))
dn_day=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['down day'])))
dur_mean=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['holding period'])))
std=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['sd'])))
sr=laply(rpt, function(x) laply(x, function(y) y$sr))
mdd_pct=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['mdd'])))
mdd_dur=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['mdd period'])))



# 100% trailing stop loss
security=to.period(dce_px_vo_ex_opcl,period = 'minute',k = 10)

nFast=30
nSlow=80
nSig=30

macd=MACD(Cl(security),nFast = nFast,nSlow = nSlow,nSig = nSig)
macd_hist=macd$macd-macd$signal

macd_signal_entry=cbind(seriesIncr(macd_hist), seriesIncr(macd_hist) & macd_hist<0)
macd_signal_exit=cbind(seriesDecr(macd_hist), seriesDecr(macd_hist) & macd_hist>0)

iter1=seq(from = 1, to = 2, by = 1)
iter2=seq(from = 1, to = 2, by = 1)

bid_ask_spread = 1
commission = 0.000165
commission_type = '*'

start_timer=proc.time()

cl=makeCluster(3, outfile='')
registerDoParallel(cl)

rpt=foreach(sig_n_index = iter1, .packages = c("quantmod", "doParallel","tseries"))%:%
  foreach(sig_x_index = iter2, .packages = c("quantmod", "doParallel","tseries"))%dopar%
  { 
    cat('sig_n_index', sig_n_index, 'sig_x_index',sig_x_index, '\n',file = 'progress.txt',append = T)
    
    if(T)
    {
      sig_n=macd_signal_entry[,sig_n_index]
      sig_x=macd_signal_exit[,sig_x_index]
      
      df=bt_stoploss(security = security ,
                     sig_n = sig_n ,
                     sig_x = sig_x,
                     stoploss = trailing_stop_pct,  
                     second_price=to.period(dce_px_vo,period = 'seconds'),
                     threshold=1)
      
      performance(df,security,bid_ask_spread = bid_ask_spread,commission = commission,commission_type = commission_type)
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


ret_mean=laply(rpt, function(x) laply(x, function(y) y$`mean bar return`))
count=laply(rpt, function(x) laply(x, function(y) y$trade))
win_rate=laply(rpt, function(x) laply(x, function(y) y$`win rate`))
up_day=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['up day'])))
dn_day=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['down day'])))
dur_mean=laply(rpt, function(x) laply(x, function(y) y$`holding period`))
std=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['sd'])))
sr=laply(rpt, function(x) laply(x, function(y) y$sr))
mdd_pct=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['mdd'])))
mdd_dur=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['mdd period'])))





# 99.5% trailing stop loss
security=to.period(dce_px_vo_ex_opcl,period = 'minute',k = 10)

nFast=30
nSlow=80
nSig=30

macd=MACD(Cl(security),nFast = nFast,nSlow = nSlow,nSig = nSig)
macd_hist=macd$macd-macd$signal

macd_signal_entry=cbind(seriesIncr(macd_hist), seriesIncr(macd_hist) & macd_hist<0)
macd_signal_exit=cbind(seriesDecr(macd_hist), seriesDecr(macd_hist) & macd_hist>0)

iter1=seq(from = 1, to = 2, by = 1)
iter2=seq(from = 1, to = 2, by = 1)

bid_ask_spread = 1
commission = 0.000165
commission_type = '*'

start_timer=proc.time()

cl=makeCluster(3, outfile='')
registerDoParallel(cl)

rpt=foreach(sig_n_index = iter1, .packages = c("quantmod", "doParallel","tseries"))%:%
  foreach(sig_x_index = iter2, .packages = c("quantmod", "doParallel","tseries"))%dopar%
  { 
    cat('sig_n_index', sig_n_index, 'sig_x_index',sig_x_index, '\n',file = 'progress.txt',append = T)
    
    if(T)
    {
      sig_n=macd_signal_entry[,sig_n_index]
      sig_x=macd_signal_exit[,sig_x_index]
      
      df=bt_stoploss(security = security ,
                     sig_n = sig_n ,
                     sig_x = sig_x,
                     stoploss = trailing_stop_pct,  
                     second_price=to.period(dce_px_vo,period = 'seconds'),
                     threshold=.995)
      
      performance(df,security,bid_ask_spread = bid_ask_spread,commission = commission,commission_type = commission_type)
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


ret_mean=laply(rpt, function(x) laply(x, function(y) y$`mean bar return`))
count=laply(rpt, function(x) laply(x, function(y) y$trade))
win_rate=laply(rpt, function(x) laply(x, function(y) y$`win rate`))
up_day=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['up day'])))
dn_day=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['down day'])))
dur_mean=laply(rpt, function(x) laply(x, function(y) y$`holding period`))
std=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['sd'])))
sr=laply(rpt, function(x) laply(x, function(y) y$sr))
mdd_pct=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['mdd'])))
mdd_dur=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['mdd period'])))



###optmizaiton

#no stop loss
security=to.period(dce_px_vo_ex_opcl,period = 'minute',k = 10)

nFast=30
nSlow=80
nSig=30

iter1=seq(from = 20, to = 40, by = 10)
iter2=seq(from = 60, to = 100, by = 10)
iter3=seq(from = 20, to = 40, by = 10)

bid_ask_spread = 1
commission = 0.000165
commission_type = '*'

start_timer=proc.time()

cl=makeCluster(3, outfile='')
registerDoParallel(cl)

rpt=foreach(nFast = iter1, .packages = c("quantmod", "doParallel","tseries"))%:%
  foreach(nSlow = iter2, .packages = c("quantmod", "doParallel","tseries"))%:%
  foreach(nSig = iter3, .packages = c("quantmod", "doParallel","tseries"))%dopar%
  { 
    cat('nFast', nFast, 'nSlow',nSlow,'nSig',nSig, '\n',file = 'progress.txt',append = T)
    
    if(T)
    {
      macd=MACD(Cl(security),nFast = nFast,nSlow = nSlow,nSig = nSig)
      macd_hist=macd$macd-macd$signal
      
      sig_n=seriesIncr(macd_hist) & macd_hist<0
      sig_x=seriesDecr(macd_hist) & macd_hist>0
      
      df=bt_stoploss(security = security ,
                     sig_n = sig_n ,
                     sig_x = sig_x,
                     stoploss = no_stop,  
                     second_price=to.period(dce_px_vo,period = 'seconds'))
      
      performance(df,security,bid_ask_spread = bid_ask_spread,commission = commission,commission_type = commission_type)
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


ret_mean=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z$`mean bar return`)))
count=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['trade'])))
win_rate=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z$`win rate`)))
up_day=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['up day'])))
dn_day=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['down day'])))
dur_mean=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['holding period'])))
std=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['sd'])))
sr=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z$sr)))
mdd_pct=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['mdd'])))
mdd_dur=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['mdd period'])))





lapply(1:17, function(sig)
{
  ggplot(melt(ret_mean[,,sig]), aes(factor(Var1),factor(Var2), fill=value)) + 
    geom_raster() +
    ggtitle(paste('ret_mean',sig))+
    scale_x_discrete(labels=seq(from = 36, to = 100, by = 2))+
    scale_y_discrete(labels=seq(from = 18, to = 80, by = 2))+
    lims(colour=c(min(ret_mean,na.rm = T),max(ret_mean,na.rm = T)))
})



lapply(1:17, function(sig)
{
  ggplot(melt(win_rate[,,sig]), aes(factor(Var1),factor(Var2), fill=value)) + 
    geom_raster() +
    ggtitle(paste('win_rate',sig))+
    scale_x_discrete(labels=seq(from = 18, to = 32, by = 2))+
    scale_y_discrete(labels=seq(from = 4, to = 20, by = 2))+
    lims(colour=c(min(win_rate,na.rm = T),max(win_rate,na.rm = T)))
})

lapply(1:11, function(sig)
{
  ggplot(melt(count[,,sig]), aes(factor(Var1),factor(Var2), fill=value)) + 
    geom_raster() +
    ggtitle(paste('count',sig))+
    scale_x_discrete(labels=seq(from = 18, to = 32, by = 2))+
    scale_y_discrete(labels=seq(from = 4, to = 20, by = 2))+
    lims(colour=c(min(count,na.rm = T),max(count,na.rm = T)))
})

lapply(1:11, function(sig)
{
  ggplot(melt(up_day[,,sig]), aes(factor(Var1),factor(Var2), fill=value)) + 
    geom_raster() +
    ggtitle(paste('up_day',sig))+
    scale_x_discrete(labels=seq(from = 18, to = 32, by = 2))+
    scale_y_discrete(labels=seq(from = 4, to = 20, by = 2))+
    lims(colour=c(min(up_day,na.rm = T),max(up_day,na.rm = T)))
})

lapply(1:11, function(sig)
{
  ggplot(melt(dn_day[,,sig]), aes(factor(Var1),factor(Var2), fill=value)) + 
    geom_raster() +
    ggtitle(paste('dn_day',sig))+
    scale_x_discrete(labels=seq(from = 18, to = 32, by = 2))+
    scale_y_discrete(labels=seq(from = 4, to = 20, by = 2))+
    lims(colour=c(min(dn_day,na.rm = T),max(dn_day,na.rm = T)))
})

lapply(1:11, function(sig)
{
  ggplot(melt(dur_mean[,,sig]), aes(factor(Var1),factor(Var2), fill=value)) + 
    geom_raster() +
    ggtitle(paste('dur_mean',sig))+
    scale_x_discrete(labels=seq(from = 18, to = 32, by = 2))+
    scale_y_discrete(labels=seq(from = 4, to = 20, by = 2))+
    lims(colour=c(min(dur_mean,na.rm = T),max(dur_mean,na.rm = T)))
})

lapply(1:11, function(sig)
{
  ggplot(melt(std[,,sig]), aes(factor(Var1),factor(Var2), fill=value)) + 
    geom_raster() +
    ggtitle(paste('std',sig))+
    scale_x_discrete(labels=seq(from = 18, to = 32, by = 2))+
    scale_y_discrete(labels=seq(from = 4, to = 20, by = 2))+
    lims(colour=c(min(std,na.rm = T),max(std,na.rm = T)))
})

lapply(1:17, function(sig)
{
  ggplot(melt(sr[,,sig]), aes(factor(Var1),factor(Var2), fill=value)) + 
    geom_raster() +
    ggtitle(paste('sr',sig))+
    scale_x_discrete(labels=seq(from = 36, to = 150, by = 2))+
    scale_y_discrete(labels=seq(from = 18, to = 70, by = 2))+
    lims(colour=c(min(sr,na.rm = T),max(sr,na.rm = T)))
})

lapply(1:11, function(sig)
{
  ggplot(melt(mdd_pct[,,sig]), aes(factor(Var1),factor(Var2), fill=value)) + 
    geom_raster() +
    ggtitle(paste('mdd_pct',sig))+
    scale_x_discrete(labels=seq(from = 18, to = 32, by = 2))+
    scale_y_discrete(labels=seq(from = 4, to = 20, by = 2))+
    lims(colour=c(min(mdd_pct,na.rm = T),max(mdd_pct,na.rm = T)))
})

lapply(1:11, function(sig)
{
  ggplot(melt(mdd_dur[,,sig]), aes(factor(Var1),factor(Var2), fill=value)) + 
    geom_raster() +
    ggtitle(paste('mdd_dur',sig))+
    scale_x_discrete(labels=seq(from = 18, to = 32, by = 2))+
    scale_y_discrete(labels=seq(from = 4, to = 20, by = 2))+
    lims(colour=c(min(mdd_dur,na.rm = T),max(mdd_dur,na.rm = T)))
})

saveRDS(rpt,'macd hist overnight')
