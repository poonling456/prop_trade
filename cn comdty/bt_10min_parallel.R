

security=dce_10m

#bt simple sma

start_timer=proc.time()
rpt=lapply(seq(from = 10, to = 150, by = 10),function(period)
{
  df=data.frame(`entry date`=as.Date(character()), `entry price`=numeric(),`exit date`=as.Date(character()), `exit price`=numeric())
  entry=xts(numeric(),as.POSIXct('1900-01-01'))
  exit=xts(numeric(),as.POSIXct('1900-01-01'))
  
  sig_n=seriesIncr(SMA(Cl(dce_10m),period))
  sig_x=!signal_n
  
  repeat
  {
    remain_n=sig_n[sig_n & time(sig_n)>=time(exit)] #extract by symbol, extract sig_n after last exit
    if(length(remain_n[remain_n])<=0 || head(time(remain_n),1)==tail(time(security),1)) break  #no more sig-> break
    
    #extract sig_x after sig_n
    #extract bsl after sig_n
    #extract tsl after sig_n
    #extract psl after sig_n(nsl return nothing)
    #if all length 0 break
    #use the closest one, if sl collide at the same day, use the highest one
    
    # construct entry and exit
    
    entry=time(head(sig_n[sig_n & time(sig_n)>time(exit)],1))
    if(length(entry)==0) break
    entry=xts(Op(security)[as.character(nxd(entry))],nxd(entry))
    
    remain_x=sig_x[sig_x & time(sig_x)>=time(entry)]
    if(length(remain_x[remain_x])<=0) break
    else
    {
      exit=time(head(sig_x[sig_x & time(sig_x)>=time(entry)],1))
      exit=xts(Op(security)[as.character(nxd(exit))],nxd(exit))
      
      temp=cbind.data.frame(`entry date`=time(entry), `entry price`=as.numeric(entry),`exit date`=time(exit), `exit price`=as.numeric(exit))
      df=rbind(df,temp)
      
    }
  }
  
  ret_day=xts(rep(0,length(time(security))),time(security))
  
  ret_day[df$`entry date`]=log(Cl(security[df$`entry date`])/df$`entry price`)
  
  hold_period=paste(nxd(df$`entry date`,1),'/',nxd(df$`exit date`,-1),sep = '')
  ret_day[hold_period]=ROC(Cl(security))[hold_period]
  
  ret_day[as.character(df$`exit date`)]=log(df$`exit price`/Cl(security[as.character(nxd(df$`exit date`,-1))]))
  
  mdd=maxdrawdown(cumsum(ret_day))
  
  setNames(c(mean(ret_day),
             nrow(df),
             length(which(df$`exit price`>df$`entry price`))/nrow(df),
             length(which(ret_day>0))/length(ret_day),
             length(which(ret_day<0))/length(ret_day),
             length(ret_day[ret_day!=0])/nrow(df),
             sqrt(var(ret_day)),
             mean(ret_day)/sqrt(var(ret_day)),
             mdd$maxdrawdown,
             head(mdd$to,1)-head(mdd$from,1)),
           c('minute return',
             'trade',
             'win rate',
             'up day',
             'down day',
             'holding period',
             'sd',
             'sr',
             'mdd',
             'mdd period'
           ))
})
proc.time()-start_timer


ret_mean=laply(rpt, function(x) x['minute return'])
count=laply(rpt, function(x) x['trade'])
win_rate=laply(rpt, function(x) x['win rate'])
up_day=laply(rpt, function(x) x['up day'])
dn_day=laply(rpt, function(x) x['down day'])
dur_mean=laply(rpt, function(x) x['holding period'])
std=laply(rpt, function(x) x['sd'])
sr=laply(rpt, function(x) x['sr'])
mdd_pct=laply(rpt, function(x) x['mdd'])
mdd_dur=laply(rpt, function(x) x['mdd period'])

write.csv(ret_mean,file = '+ve sma return.csv')
write.csv(win_rate,file = '+ve sma win rate.csv')
write.csv(sr,file = '+ve sma sharpe.csv')





#bt price cross sma

start_timer=proc.time()
rpt=lapply(seq(from = 10, to = 40, by = 10),function(period)
{
  df=data.frame(`entry date`=as.Date(character()), `entry price`=numeric(),`exit date`=as.Date(character()), `exit price`=numeric())
  entry=xts(numeric(),as.POSIXct('1900-01-01'))
  exit=xts(numeric(),as.POSIXct('1900-01-01'))
  
  sig_n=Cl(security)>SMA(Cl(security),period)
  sig_x=!signal_n
  
  repeat
  {
    remain_n=sig_n[sig_n & time(sig_n)>=time(exit)] #extract by symbol, extract sig_n after last exit
    if(length(remain_n[remain_n])<=0 || head(time(remain_n),1)==tail(time(security),1)) break  #no more sig-> break
    
    #extract sig_x after sig_n
    #extract bsl after sig_n
    #extract tsl after sig_n
    #extract psl after sig_n(nsl return nothing)
    #if all length 0 break
    #use the closest one, if sl collide at the same day, use the highest one
    
    # construct entry and exit
    
    entry=time(head(sig_n[sig_n & time(sig_n)>time(exit)],1))
    if(length(entry)==0) break
    entry=xts(Op(security)[as.character(nxd(entry))],nxd(entry))
    
    remain_x=sig_x[sig_x & time(sig_x)>=time(entry)]
    if(length(remain_x[remain_x])<=0) break
    else
    {
      exit=time(head(sig_x[sig_x & time(sig_x)>=time(entry)],1))
      exit=xts(Op(security)[as.character(nxd(exit))],nxd(exit))
      
      temp=cbind.data.frame(`entry date`=time(entry), `entry price`=as.numeric(entry),`exit date`=time(exit), `exit price`=as.numeric(exit))
      df=rbind(df,temp)
      
    }
  }
  
  ret_day=xts(rep(0,length(time(security))),time(security))
  
  ret_day[df$`entry date`]=log(Cl(security[df$`entry date`])/df$`entry price`)
  
  hold_period=paste(nxd(df$`entry date`,1),'/',nxd(df$`exit date`,-1),sep = '')
  ret_day[hold_period]=ROC(Cl(security))[hold_period]
  
  ret_day[as.character(df$`exit date`)]=log(df$`exit price`/Cl(security[as.character(nxd(df$`exit date`,-1))]))
  
  mdd=maxdrawdown(cumsum(ret_day))
  
  setNames(c(mean(ret_day),
             nrow(df),
             length(which(df$`exit price`>df$`entry price`))/nrow(df),
             length(which(ret_day>0))/length(ret_day),
             length(which(ret_day<0))/length(ret_day),
             length(ret_day[ret_day!=0])/nrow(df),
             sqrt(var(ret_day)),
             mean(ret_day)/sqrt(var(ret_day)),
             mdd$maxdrawdown,
             head(mdd$to,1)-head(mdd$from,1)),
           c('minute return',
             'trade',
             'win rate',
             'up day',
             'down day',
             'holding period',
             'sd',
             'sr',
             'mdd',
             'mdd period'
           ))
})
proc.time()-start_timer


ret_mean=laply(rpt, function(x) x['minute return'])
count=laply(rpt, function(x) x['trade'])
win_rate=laply(rpt, function(x) x['win rate'])
up_day=laply(rpt, function(x) x['up day'])
dn_day=laply(rpt, function(x) x['down day'])
dur_mean=laply(rpt, function(x) x['holding period'])
std=laply(rpt, function(x) x['sd'])
sr=laply(rpt, function(x) x['sr'])
mdd_pct=laply(rpt, function(x) x['mdd'])
mdd_dur=laply(rpt, function(x) x['mdd period'])

write.csv(ret_mean,file = 'cl cross sma return.csv')
write.csv(win_rate,file = 'cl cross sma win rate.csv')
write.csv(sr,file = 'cl cross sma sharpe.csv')





#bt rsi

start_timer=proc.time()
rpt=lapply(seq(from = 10, to = 250, by = 10),function(period)
#  for(period in seq(from = 10, to = 250, by = 10))
{
  df=data.frame(`entry date`=as.Date(character()), `entry price`=numeric(),`exit date`=as.Date(character()), `exit price`=numeric())
  entry=xts(numeric(),as.POSIXct('1900-01-01'))
  exit=xts(numeric(),as.POSIXct('1900-01-01'))
  
  sig_n=RSI(Cl(security),period)<50
  sig_x=!signal_n
  
  repeat
  {
    remain_n=sig_n[sig_n & time(sig_n)>=time(exit)] #extract by symbol, extract sig_n after last exit
    if(length(remain_n[remain_n])<=0 || head(time(remain_n),1)==tail(time(security),1)) break  #no more sig-> break
    
    #extract sig_x after sig_n
    #extract bsl after sig_n
    #extract tsl after sig_n
    #extract psl after sig_n(nsl return nothing)
    #if all length 0 break
    #use the closest one, if sl collide at the same day, use the highest one
    
    # construct entry and exit
    
    entry=time(head(sig_n[sig_n & time(sig_n)>time(exit)],1))
    if(length(entry)==0) break
    entry=xts(Op(security)[as.character(nxd(entry))],nxd(entry))
    
    remain_x=sig_x[sig_x & time(sig_x)>=time(entry)]
    if(length(remain_x[remain_x])<=0) break
    else
    {
      exit=time(head(sig_x[sig_x & time(sig_x)>=time(entry)],1))
      exit=xts(Op(security)[as.character(nxd(exit))],nxd(exit))
      
      temp=cbind.data.frame(`entry date`=time(entry), `entry price`=as.numeric(entry),`exit date`=time(exit), `exit price`=as.numeric(exit))
      df=rbind(df,temp)
      
    }
  }
  if(nrow(df)>0)
  {
    ret_day=xts(rep(0,length(time(security))),time(security))
    
    ret_day[df$`entry date`]=log(Cl(security[df$`entry date`])/df$`entry price`)
    
    hold_period=paste(nxd(df$`entry date`,1),'/',nxd(df$`exit date`,-1),sep = '')
    ret_day[hold_period]=ROC(Cl(security))[hold_period]
    
    ret_day[as.character(df$`exit date`)]=log(df$`exit price`/Cl(security[as.character(nxd(df$`exit date`,-1))]))
    
    mdd=maxdrawdown(cumsum(ret_day))
    
    setNames(c(mean(ret_day),
               nrow(df),
               length(which(df$`exit price`>df$`entry price`))/nrow(df),
               length(which(ret_day>0))/length(ret_day),
               length(which(ret_day<0))/length(ret_day),
               length(ret_day[ret_day!=0])/nrow(df),
               sqrt(var(ret_day)),
               mean(ret_day)/sqrt(var(ret_day)),
               mdd$maxdrawdown,
               head(mdd$to,1)-head(mdd$from,1)),
             c('minute return',
               'trade',
               'win rate',
               'up day',
               'down day',
               'holding period',
               'sd',
               'sr',
               'mdd',
               'mdd period'
             ))
  }
  else
  {
    setNames(c(NA,
               NA,
               NA,
               NA,
               NA,
               NA,
               NA,
               NA,
               NA,
               NA),
             c('minute return',
               'trade',
               'win rate',
               'up day',
               'down day',
               'holding period',
               'sd',
               'sr',
               'mdd',
               'mdd period'
             ))
  }
})
proc.time()-start_timer


ret_mean=laply(rpt, function(x) x['minute return'])
count=laply(rpt, function(x) x['trade'])
win_rate=laply(rpt, function(x) x['win rate'])
up_day=laply(rpt, function(x) x['up day'])
dn_day=laply(rpt, function(x) x['down day'])
dur_mean=laply(rpt, function(x) x['holding period'])
std=laply(rpt, function(x) x['sd'])
sr=laply(rpt, function(x) x['sr'])
mdd_pct=laply(rpt, function(x) x['mdd'])
mdd_dur=laply(rpt, function(x) x['mdd period'])

plot(ret_mean)
plot(win_rate)
plot(sr)


write.csv(ret_mean,file = 'rsi less than 50 return.csv')
write.csv(win_rate,file = 'rsi less than 50 win rate.csv')
write.csv(sr,file = 'rsi less than 50 sharpe.csv')





#bt donchian channel

start_timer=proc.time()
rpt=lapply(seq(from = 2, to = 20, by = 2),function(period_high)
{
  lapply(seq(from = 2, to = 20, by = 2),function(period_low)
  {
    df=data.frame(`entry date`=as.Date(character()), `entry price`=numeric(),`exit date`=as.Date(character()), `exit price`=numeric())
    entry=xts(numeric(),as.POSIXct('1900-01-01'))
    exit=xts(numeric(),as.POSIXct('1900-01-01'))
    
    sig_n=seriesIncr(DonchianChannel(Cl(security),period_high)$high)
    sig_n[cumsum(rle(as.character(as.Date(time(sig_n))))$lengths)]=F
    sig_x=seriesDecr(DonchianChannel(Cl(security),period_low)$low)
    sig_x[cumsum(rle(as.character(as.Date(time(sig_x))))$lengths)-1]=T
    
    repeat
    {
      remain_n=sig_n[sig_n & time(sig_n)>=time(exit)] #extract by symbol, extract sig_n after last exit
      if(length(remain_n[remain_n])<=0 || head(time(remain_n),1)==tail(time(security),1)) break  #no more sig-> break
      
      #extract sig_x after sig_n
      #extract bsl after sig_n
      #extract tsl after sig_n
      #extract psl after sig_n(nsl return nothing)
      #if all length 0 break
      #use the closest one, if sl collide at the same day, use the highest one
      
      # construct entry and exit
      
      entry=time(head(sig_n[sig_n & time(sig_n)>time(exit)],1))
      if(length(entry)==0) break
      entry=xts(Op(security)[as.character(nxd(entry))],nxd(entry))
      
      remain_x=sig_x[sig_x & time(sig_x)>=time(entry)]
      if(length(remain_x[remain_x])<=0) break
      else
      {
        exit=time(head(sig_x[sig_x & time(sig_x)>=time(entry)],1))
        exit=xts(Op(security)[as.character(nxd(exit))],nxd(exit))
        
        temp=cbind.data.frame(`entry date`=time(entry), `entry price`=as.numeric(entry),`exit date`=time(exit), `exit price`=as.numeric(exit))
        df=rbind(df,temp)
        
      }
    }
    if(nrow(df)>0)
    {
      ret_day=xts(rep(0,length(time(security))),time(security))
      
      ret_day[df$`entry date`]=log(Cl(security[df$`entry date`])/df$`entry price`)
      
      hold_period=paste(nxd(df$`entry date`,1),'/',nxd(df$`exit date`,-1),sep = '')
      ret_day[hold_period]=ROC(Cl(security))[hold_period]
      
      ret_day[as.character(df$`exit date`)]=log(df$`exit price`/Cl(security[as.character(nxd(df$`exit date`,-1))]))
      
      mdd=maxdrawdown(cumsum(ret_day))
      
      setNames(c(mean(ret_day),
                 nrow(df),
                 length(which(df$`exit price`>df$`entry price`))/nrow(df),
                 length(which(ret_day>0))/length(ret_day),
                 length(which(ret_day<0))/length(ret_day),
                 length(ret_day[ret_day!=0])/nrow(df),
                 sqrt(var(ret_day)),
                 mean(ret_day)/sqrt(var(ret_day)),
                 mdd$maxdrawdown,
                 head(mdd$to,1)-head(mdd$from,1)),
               c('minute return',
                 'trade',
                 'win rate',
                 'up day',
                 'down day',
                 'holding period',
                 'sd',
                 'sr',
                 'mdd',
                 'mdd period'
               ))
    }
    else
    {
      setNames(c(NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA),
               c('minute return',
                 'trade',
                 'win rate',
                 'up day',
                 'down day',
                 'holding period',
                 'sd',
                 'sr',
                 'mdd',
                 'mdd period'
               ))
    }
  })
})
proc.time()-start_timer


ret_mean=laply(rpt, function(x) laply(x, function(y) y['minute return']))
count=laply(rpt, function(x) laply(x, function(y) y['trade']))
win_rate=laply(rpt, function(x) laply(x, function(y) y['win rate']))
up_day=laply(rpt, function(x) laply(x, function(y) y['up day']))
dn_day=laply(rpt, function(x) laply(x, function(y) y['down day']))
dur_mean=laply(rpt, function(x) laply(x, function(y) y['holding period']))
std=laply(rpt, function(x) laply(x, function(y) y['sd']))
sr=laply(rpt, function(x) laply(x, function(y) y['sr']))
mdd_pct=laply(rpt, function(x) laply(x, function(y) y['mdd']))
mdd_dur=laply(rpt, function(x) laply(x, function(y) y['mdd period']))

ggplot(melt(ret_mean), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster() +ggtitle('ret_mean')+scale_x_discrete(labels=seq(from = 2, to = 20, by = 2))+scale_y_discrete(labels=seq(from = 2, to = 20, by = 2))
ggplot(melt(count), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster() +ggtitle('count')+scale_x_discrete(labels=seq(from = 2, to = 20, by = 2))+scale_y_discrete(labels=seq(from = 2, to = 20, by = 2))
ggplot(melt(win_rate), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('win_rate')+scale_x_discrete(labels=seq(from = 2, to = 20, by = 2))+scale_y_discrete(labels=seq(from = 2, to = 20, by = 2))
ggplot(melt(dur_mean), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('dur_mean')+scale_x_discrete(labels=seq(from = 2, to = 20, by = 2))+scale_y_discrete(labels=seq(from = 2, to = 20, by = 2))
ggplot(melt(up_day), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('up_day')+scale_x_discrete(labels=seq(from = 2, to = 20, by = 2))+scale_y_discrete(labels=seq(from = 2, to = 20, by = 2))
ggplot(melt(dn_day), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('dn_day')+scale_x_discrete(labels=seq(from = 2, to = 20, by = 2))+scale_y_discrete(labels=seq(from = 2, to = 20, by = 2))
ggplot(melt(std), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('std')+scale_x_discrete(labels=seq(from = 2, to = 20, by = 2))+scale_y_discrete(labels=seq(from = 2, to = 20, by = 2))
ggplot(melt(sr), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('sr')+scale_x_discrete(labels=seq(from = 2, to = 20, by = 2))+scale_y_discrete(labels=seq(from = 2, to = 20, by = 2))
ggplot(melt(mdd_pct), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('mdd_pct')+scale_x_discrete(labels=seq(from = 2, to = 20, by = 2))+scale_y_discrete(labels=seq(from = 2, to = 20, by = 2))
ggplot(melt(mdd_dur), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('mdd_dur')+scale_x_discrete(labels=seq(from = 2, to = 20, by = 2))+scale_y_discrete(labels=seq(from = 2, to = 20, by = 2))



write.csv(ret_mean,file = 'break out return.csv')
write.csv(win_rate,file = 'break out win rate.csv')
write.csv(sr,file = 'break out sharpe.csv')




#bt macd

security=dce_10m

cl=makeCluster(3, outfile='')
registerDoParallel(cl)

start_timer=proc.time()
rpt=foreach(slow = seq(from = 18, to = 32, by = 2), .packages = c("quantmod", "doParallel","tseries"))%:%
foreach(fast = seq(from = 4, to = 20, by = 2), .packages = c("quantmod", "doParallel","tseries"))%:%
foreach(sig = seq(from = 4, to = 14, by = 1), .packages = c("quantmod", "doParallel","tseries")) %dopar%
{ 
  cat(slow,fast,sig,'\n',file = 'progress.txt',append = T)
  
  if(fast<slow)
  {
    df=data.frame(`entry date`=as.Date(character()), `entry price`=numeric(),`exit date`=as.Date(character()), `exit price`=numeric())
    entry=xts(numeric(),as.POSIXct('1900-01-01'))
    exit=xts(numeric(),as.POSIXct('1900-01-01'))
    
    macd=MACD(Cl(security),nFast = fast,nSlow = slow,nSig = sig)
    
    sig_n=macd$macd > macd$signal
    sig_x=!sig_n
    
    repeat
    {
      remain_n=sig_n[sig_n & time(sig_n)>=time(exit)] #extract by symbol, extract sig_n after last exit
      if(length(remain_n[remain_n])<=0 || head(time(remain_n),1)==tail(time(security),1)) break  #no more sig-> break
      
      #extract sig_x after sig_n
      #extract bsl after sig_n
      #extract tsl after sig_n
      #extract psl after sig_n(nsl return nothing)
      #if all length 0 break
      #use the closest one, if sl collide at the same day, use the highest one
      
      # construct entry and exit
      
      entry=time(head(sig_n[sig_n & time(sig_n)>time(exit)],1))
      if(length(entry)==0) break
      entry=xts(Op(security)[as.character(nxd(entry))],nxd(entry))
      
      remain_x=sig_x[sig_x & time(sig_x)>=time(entry)]
      if(length(remain_x[remain_x])<=0) break
      else
      {
        exit=time(head(sig_x[sig_x & time(sig_x)>=time(entry)],1))
        exit=xts(Op(security)[as.character(nxd(exit))],nxd(exit))
        
        temp=cbind.data.frame(`entry date`=time(entry), `entry price`=as.numeric(entry),`exit date`=time(exit), `exit price`=as.numeric(exit))
        df=rbind(df,temp)
        
      }
    }
    if(nrow(df)>0)
    {
      ret_day=xts(rep(0,length(time(security))),time(security))
      
      ret_day[df$`entry date`]=log(Cl(security[df$`entry date`])/df$`entry price`)
      
      hold_period=paste(nxd(df$`entry date`,1),'/',nxd(df$`exit date`,-1),sep = '')
      ret_day[hold_period]=ROC(Cl(security))[hold_period]
      
      ret_day[as.character(df$`exit date`)]=log(df$`exit price`/Cl(security[as.character(nxd(df$`exit date`,-1))]))
      
      mdd=maxdrawdown(cumsum(ret_day))
      
      setNames(c(mean(ret_day),
                 nrow(df),
                 length(which(df$`exit price`>df$`entry price`))/nrow(df),
                 length(which(ret_day>0))/length(ret_day),
                 length(which(ret_day<0))/length(ret_day),
                 length(ret_day[ret_day!=0])/nrow(df),
                 sqrt(var(ret_day)),
                 mean(ret_day)/sqrt(var(ret_day)),
                 mdd$maxdrawdown,
                 head(mdd$to,1)-head(mdd$from,1)),
               c('minute return',
                 'trade',
                 'win rate',
                 'up day',
                 'down day',
                 'holding period',
                 'sd',
                 'sr',
                 'mdd',
                 'mdd period'
               ))
    }
    else
    {
      setNames(c(NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA),
               c('minute return',
                 'trade',
                 'win rate',
                 'up day',
                 'down day',
                 'holding period',
                 'sd',
                 'sr',
                 'mdd',
                 'mdd period'
               ))
    }
  }
  else
  {
    setNames(c(NA,
               NA,
               NA,
               NA,
               NA,
               NA,
               NA,
               NA,
               NA,
               NA),
             c('minute return',
               'trade',
               'win rate',
               'up day',
               'down day',
               'holding period',
               'sd',
               'sr',
               'mdd',
               'mdd period'
             ))
  }
}
proc.time()-start_timer
stopCluster(cl)


ret_mean=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['minute return'])))
count=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['trade'])))
win_rate=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['win rate'])))
up_day=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['up day'])))
dn_day=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['down day'])))
dur_mean=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['holding period'])))
std=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['sd'])))
sr=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['sr'])))
mdd_pct=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['mdd'])))
mdd_dur=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['mdd period'])))


lapply(1:11, function(sig)
{
  ggplot(melt(ret_mean[,,sig]), aes(factor(Var1),factor(Var2), fill=value)) + 
    geom_raster() +
    ggtitle(paste('ret_mean',sig))+
    scale_x_discrete(labels=seq(from = 18, to = 32, by = 2))+
    scale_y_discrete(labels=seq(from = 4, to = 20, by = 2))+
    lims(colour=c(min(ret_mean,na.rm = T),max(ret_mean,na.rm = T)))
})

lapply(1:11, function(sig)
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

lapply(1:11, function(sig)
{
  ggplot(melt(sr[,,sig]), aes(factor(Var1),factor(Var2), fill=value)) + 
    geom_raster() +
    ggtitle(paste('sr',sig))+
    scale_x_discrete(labels=seq(from = 18, to = 32, by = 2))+
    scale_y_discrete(labels=seq(from = 4, to = 20, by = 2))+
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

saveRDS(rpt,'macd cross overnight')




#ok, best sr 0.8 , win rate 38%, arround fast 18 slow 20 sig 4



#bt macd histogram

security=dce_10m

cl=makeCluster(3, outfile='')
registerDoParallel(cl)

start_timer=proc.time()
rpt=foreach(slow = seq(from = 36, to = 150, by = 2), .packages = c("quantmod", "doParallel","tseries"))%:%
  foreach(fast = seq(from = 18, to = 70, by = 2), .packages = c("quantmod", "doParallel","tseries"))%:%
  foreach(sig = seq(from = 24, to = 40, by = 2), .packages = c("quantmod", "doParallel","tseries")) %dopar%
  { 
    cat(slow,fast,sig,'\n',file = 'progress.txt',append = T)
    
    if(fast<slow)
    {
      df=data.frame(`entry date`=as.Date(character()), `entry price`=numeric(),`exit date`=as.Date(character()), `exit price`=numeric())
      entry=xts(numeric(),as.POSIXct('1900-01-01'))
      exit=xts(numeric(),as.POSIXct('1900-01-01'))
      
      macd=MACD(Cl(security),nFast = fast,nSlow = slow,nSig = sig)
      macd_hist=macd$macd-macd$signal
      
      sig_n=seriesIncr(macd_hist)
      sig_x=!sig_n
      
      repeat
      {
        remain_n=sig_n[sig_n & time(sig_n)>=time(exit)] #extract by symbol, extract sig_n after last exit
        if(length(remain_n[remain_n])<=0 || head(time(remain_n),1)==tail(time(security),1)) break  #no more sig-> break
        
        #extract sig_x after sig_n
        #extract bsl after sig_n
        #extract tsl after sig_n
        #extract psl after sig_n(nsl return nothing)
        #if all length 0 break
        #use the closest one, if sl collide at the same day, use the highest one
        
        # construct entry and exit
        
        entry=time(head(sig_n[sig_n & time(sig_n)>time(exit)],1))
        if(length(entry)==0) break
        entry=xts(Op(security)[as.character(nxd(entry))],nxd(entry))
        
        remain_x=sig_x[sig_x & time(sig_x)>=time(entry)]
        if(length(remain_x[remain_x])<=0) break
        else
        {
          exit=time(head(sig_x[sig_x & time(sig_x)>=time(entry)],1))
          exit=xts(Op(security)[as.character(nxd(exit))],nxd(exit))
          
          temp=cbind.data.frame(`entry date`=time(entry), `entry price`=as.numeric(entry),`exit date`=time(exit), `exit price`=as.numeric(exit))
          df=rbind(df,temp)
          
        }
      }
      if(nrow(df)>0)
      {
        ret_day=xts(rep(0,length(time(security))),time(security))
        
        ret_day[df$`entry date`]=log(Cl(security[df$`entry date`])/df$`entry price`)
        
        hold_period=paste(nxd(df$`entry date`,1),'/',nxd(df$`exit date`,-1),sep = '')
        ret_day[hold_period]=ROC(Cl(security))[hold_period]
        
        ret_day[as.character(df$`exit date`)]=log(df$`exit price`/Cl(security[as.character(nxd(df$`exit date`,-1))]))
        
        mdd=maxdrawdown(cumsum(ret_day))
        
        setNames(c(mean(ret_day),
                   nrow(df),
                   length(which(df$`exit price`>df$`entry price`))/nrow(df),
                   length(which(ret_day>0))/length(ret_day),
                   length(which(ret_day<0))/length(ret_day),
                   length(ret_day[ret_day!=0])/nrow(df),
                   sqrt(var(ret_day)),
                   mean(ret_day)/sqrt(var(ret_day)),
                   mdd$maxdrawdown,
                   head(mdd$to,1)-head(mdd$from,1)),
                 c('minute return',
                   'trade',
                   'win rate',
                   'up day',
                   'down day',
                   'holding period',
                   'sd',
                   'sr',
                   'mdd',
                   'mdd period'
                 ))
      }
      else
      {
        setNames(c(NA,
                   NA,
                   NA,
                   NA,
                   NA,
                   NA,
                   NA,
                   NA,
                   NA,
                   NA),
                 c('minute return',
                   'trade',
                   'win rate',
                   'up day',
                   'down day',
                   'holding period',
                   'sd',
                   'sr',
                   'mdd',
                   'mdd period'
                 ))
      }
    }
    else
    {
      setNames(c(NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA),
               c('minute return',
                 'trade',
                 'win rate',
                 'up day',
                 'down day',
                 'holding period',
                 'sd',
                 'sr',
                 'mdd',
                 'mdd period'
               ))
    }
  }
proc.time()-start_timer
stopCluster(cl)


ret_mean=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['minute return'])))
count=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['trade'])))
win_rate=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['win rate'])))
up_day=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['up day'])))
dn_day=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['down day'])))
dur_mean=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['holding period'])))
std=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['sd'])))
sr=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['sr'])))
mdd_pct=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['mdd'])))
mdd_dur=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['mdd period'])))


lapply(1:9, function(sig)
{
  ggplot(melt(ret_mean[,,sig]), aes(factor(Var1),factor(Var2), fill=value)) + 
    geom_raster() +
    ggtitle(paste('ret_mean',sig))+
    scale_x_discrete(labels=seq(from = 36, to = 100, by = 2))+
    scale_y_discrete(labels=seq(from = 18, to = 80, by = 2))+
    lims(colour=c(min(ret_mean,na.rm = T),max(ret_mean,na.rm = T)))
})

lapply(1:11, function(sig)
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

lapply(1:9, function(sig)
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

#best sr 0.75, win rate 33, arround fast 82 slow 30 sig 30
#buy only when macd hist -ve double the sr!








#bt macd histogram

security=dce_10m

start_timer=proc.time()

cl=makeCluster(3, outfile='')
registerDoParallel(cl)

rpt=foreach(slow = seq(from = 20, to = 150, by = 2), .packages = c("quantmod", "doParallel","tseries"))%:%
  foreach(fast = seq(from = 12, to = 70, by = 2), .packages = c("quantmod", "doParallel","tseries"))%:%
  foreach(sig = seq(from = 8, to = 40, by = 2), .packages = c("quantmod", "doParallel","tseries")) %dopar%
  { 
    cat(slow,fast,sig,'\n',file = 'progress.txt',append = T)
    
    if(fast<slow)
    {
      df=data.frame(`entry date`=as.Date(character()), `entry price`=numeric(),`exit date`=as.Date(character()), `exit price`=numeric())
      entry=xts(numeric(),as.POSIXct('1900-01-01'))
      exit=xts(numeric(),as.POSIXct('1900-01-01'))
      
      macd=MACD(Cl(security),nFast = fast,nSlow = slow,nSig = sig)
      macd_hist=macd$macd-macd$signal
      
      sig_n=seriesIncr(macd_hist) & macd_hist<0
      sig_x=seriesDecr(macd_hist)
      
      repeat
      {
        remain_n=sig_n[sig_n & time(sig_n)>=time(exit)] #extract by symbol, extract sig_n after last exit
        if(length(remain_n[remain_n])<=0 || head(time(remain_n),1)==tail(time(security),1)) break  #no more sig-> break
        
        #extract sig_x after sig_n
        #extract bsl after sig_n
        #extract tsl after sig_n
        #extract psl after sig_n(nsl return nothing)
        #if all length 0 break
        #use the closest one, if sl collide at the same day, use the highest one
        
        # construct entry and exit
        
        entry=time(head(sig_n[sig_n & time(sig_n)>time(exit)],1))
        if(length(entry)==0) break
        entry=xts(Op(security)[as.character(nxd(entry))],nxd(entry))
        
        remain_x=sig_x[sig_x & time(sig_x)>=time(entry)]
        if(length(remain_x[remain_x])<=0) break
        else
        {
          exit=time(head(sig_x[sig_x & time(sig_x)>=time(entry)],1))
          exit=xts(Op(security)[as.character(nxd(exit))],nxd(exit))
          
          temp=cbind.data.frame(`entry date`=time(entry), `entry price`=as.numeric(entry),`exit date`=time(exit), `exit price`=as.numeric(exit))
          df=rbind(df,temp)
          
        }
      }
      if(nrow(df)>0)
      {
        ret_day=xts(rep(0,length(time(security))),time(security))
        
        ret_day[df$`entry date`]=log(Cl(security[df$`entry date`])/df$`entry price`)
        
        hold_period=paste(nxd(df$`entry date`,1),'/',nxd(df$`exit date`,-1),sep = '')
        ret_day[hold_period]=ROC(Cl(security))[hold_period]
        
        ret_day[as.character(df$`exit date`)]=log(df$`exit price`/Cl(security[as.character(nxd(df$`exit date`,-1))]))
        
        mdd=maxdrawdown(cumsum(ret_day))
        
        setNames(c(mean(ret_day),
                   nrow(df),
                   length(which(df$`exit price`>df$`entry price`))/nrow(df),
                   length(which(ret_day>0))/length(ret_day),
                   length(which(ret_day<0))/length(ret_day),
                   length(ret_day[ret_day!=0])/nrow(df),
                   sqrt(var(ret_day)),
                   mean(ret_day)/sqrt(var(ret_day)),
                   mdd$maxdrawdown,
                   head(mdd$to,1)-head(mdd$from,1)),
                 c('minute return',
                   'trade',
                   'win rate',
                   'up day',
                   'down day',
                   'holding period',
                   'sd',
                   'sr',
                   'mdd',
                   'mdd period'
                 ))
      }
      else
      {
        setNames(c(NA,
                   NA,
                   NA,
                   NA,
                   NA,
                   NA,
                   NA,
                   NA,
                   NA,
                   NA),
                 c('minute return',
                   'trade',
                   'win rate',
                   'up day',
                   'down day',
                   'holding period',
                   'sd',
                   'sr',
                   'mdd',
                   'mdd period'
                 ))
      }
    }
    else
    {
      setNames(c(NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA),
               c('minute return',
                 'trade',
                 'win rate',
                 'up day',
                 'down day',
                 'holding period',
                 'sd',
                 'sr',
                 'mdd',
                 'mdd period'
               ))
    }
  }
stopCluster(cl)

proc.time()-start_timer


ret_mean=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['minute return'])))
count=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['trade'])))
win_rate=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['win rate'])))
up_day=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['up day'])))
dn_day=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['down day'])))
dur_mean=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['holding period'])))
std=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['sd'])))
sr=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['sr'])))
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

#



#bt macd position close at night

cl=makeCluster(3, outfile='')
registerDoParallel(cl)

start_timer=proc.time()
rpt=foreach(slow = seq(from = 18, to = 32, by = 2), .packages = c("quantmod", "doParallel","tseries"))%:%
  foreach(fast = seq(from = 4, to = 20, by = 2), .packages = c("quantmod", "doParallel","tseries"))%:%
  foreach(sig = seq(from = 4, to = 14, by = 1), .packages = c("quantmod", "doParallel","tseries")) %dopar%
  { 
    cat(slow,fast,sig,'\n',file = 'progress.txt',append = T)
    
    if(fast<slow)
    {
      df=data.frame(`entry date`=as.Date(character()), `entry price`=numeric(),`exit date`=as.Date(character()), `exit price`=numeric())
      entry=xts(numeric(),as.POSIXct('1900-01-01'))
      exit=xts(numeric(),as.POSIXct('1900-01-01'))
      
      macd=MACD(Cl(security),nFast = fast,nSlow = slow,nSig = sig)
      
      sig_n=macd$macd > macd$signal
      sig_x=!sig_n
      
      #no buy signal 1 bar before close
      sig_n[cumsum(rle(as.character(as.Date(time(sig_n))))$lengths)-1]=F
      #sell signal automatically on 1 bar before close
      sig_x[cumsum(rle(as.character(as.Date(time(sig_x))))$lengths)-1]=T
      
      repeat
      {
        remain_n=sig_n[sig_n & time(sig_n)>=time(exit)] #extract by symbol, extract sig_n after last exit
        if(length(remain_n[remain_n])<=0 || head(time(remain_n),1)==tail(time(security),1)) break  #no more sig-> break
        
        #extract sig_x after sig_n
        #extract bsl after sig_n
        #extract tsl after sig_n
        #extract psl after sig_n(nsl return nothing)
        #if all length 0 break
        #use the closest one, if sl collide at the same day, use the highest one
        
        # construct entry and exit
        
        entry=time(head(sig_n[sig_n & time(sig_n)>time(exit)],1))
        if(length(entry)==0) break
        entry=xts(Op(security)[as.character(nxd(entry))],nxd(entry))
        
        remain_x=sig_x[sig_x & time(sig_x)>=time(entry)]
        if(length(remain_x[remain_x])<=0) break
        else
        {
          exit=time(head(sig_x[sig_x & time(sig_x)>=time(entry)],1))
          exit=xts(Op(security)[as.character(nxd(exit))],nxd(exit))
          
          temp=cbind.data.frame(`entry date`=time(entry), `entry price`=as.numeric(entry),`exit date`=time(exit), `exit price`=as.numeric(exit))
          df=rbind(df,temp)
          
        }
      }
      if(nrow(df)>0)
      {
        ret_day=xts(rep(0,length(time(security))),time(security))
        
        ret_day[df$`entry date`]=log(Cl(security[df$`entry date`])/df$`entry price`)
        
        hold_period=paste(nxd(df$`entry date`,1),'/',nxd(df$`exit date`,-1),sep = '')
        ret_day[hold_period]=ROC(Cl(security))[hold_period]
        
        ret_day[as.character(df$`exit date`)]=log(df$`exit price`/Cl(security[as.character(nxd(df$`exit date`,-1))]))
        
        mdd=maxdrawdown(cumsum(ret_day))
        
        setNames(c(mean(ret_day),
                   nrow(df),
                   length(which(df$`exit price`>df$`entry price`))/nrow(df),
                   length(which(ret_day>0))/length(ret_day),
                   length(which(ret_day<0))/length(ret_day),
                   length(ret_day[ret_day!=0])/nrow(df),
                   sqrt(var(ret_day)),
                   mean(ret_day)/sqrt(var(ret_day)),
                   mdd$maxdrawdown,
                   head(mdd$to,1)-head(mdd$from,1)),
                 c('minute return',
                   'trade',
                   'win rate',
                   'up day',
                   'down day',
                   'holding period',
                   'sd',
                   'sr',
                   'mdd',
                   'mdd period'
                 ))
      }
      else
      {
        setNames(c(NA,
                   NA,
                   NA,
                   NA,
                   NA,
                   NA,
                   NA,
                   NA,
                   NA,
                   NA),
                 c('minute return',
                   'trade',
                   'win rate',
                   'up day',
                   'down day',
                   'holding period',
                   'sd',
                   'sr',
                   'mdd',
                   'mdd period'
                 ))
      }
    }
    else
    {
      setNames(c(NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA,
                 NA),
               c('minute return',
                 'trade',
                 'win rate',
                 'up day',
                 'down day',
                 'holding period',
                 'sd',
                 'sr',
                 'mdd',
                 'mdd period'
               ))
    }
  }
proc.time()-start_timer
stopCluster(cl)


ret_mean=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['minute return'])))
count=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['trade'])))
win_rate=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['win rate'])))
up_day=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['up day'])))
dn_day=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['down day'])))
dur_mean=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['holding period'])))
std=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['sd'])))
sr=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['sr'])))
mdd_pct=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['mdd'])))
mdd_dur=laply(rpt, function(x) laply(x, function(y) laply(y, function(z) z['mdd period'])))

lapply(1:11, function(sig)
{
  ggplot(melt(ret_mean[,,sig]), aes(factor(Var1),factor(Var2), fill=value)) + 
  geom_raster() +
  ggtitle(paste('ret_mean',sig))+
  scale_x_discrete(labels=seq(from = 18, to = 32, by = 2))+
  scale_y_discrete(labels=seq(from = 4, to = 20, by = 2))+
  lims(colour=c(min(ret_mean,na.rm = T),max(ret_mean,na.rm = T)))
})

lapply(1:11, function(sig)
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

lapply(1:11, function(sig)
{
  ggplot(melt(sr[,,sig]), aes(factor(Var1),factor(Var2), fill=value)) + 
    geom_raster() +
    ggtitle(paste('sr',sig))+
    scale_x_discrete(labels=seq(from = 18, to = 32, by = 2))+
    scale_y_discrete(labels=seq(from = 4, to = 20, by = 2))+
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

saveRDS(rpt,'macd cross close position at night')
