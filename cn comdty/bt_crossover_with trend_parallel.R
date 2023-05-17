i=seq(80,150,by = 10)
j=seq(100,200,by = 10)
k=(0:6)*10+180

cl=makeCluster(3)
registerDoParallel(cl)

start_run=proc.time()
rpt=foreach(p = k, .packages = c("quantmod", "doParallel","tseries"))%:%
foreach(n = j, .packages = c("quantmod", "doParallel","tseries"))%:%
  foreach(m = i, .packages = c("quantmod", "doParallel","tseries")) %dopar%
  { 
    if(n-m>0 & p-n>10)
    {
      trend=SMA(Cl(dce_1m),p)
      slow=SMA(Cl(dce_1m),n)
      fast=SMA(Cl(dce_1m),m)
      
      sig_n=(fast>slow) & seriesIncr(trend)
      sig_n[cumsum(rle(as.character(as.Date(time(sig_n))))$lengths)]=F
      sig_x=fast<slow
      sig_x[cumsum(rle(as.character(as.Date(time(sig_x))))$lengths)-1]=T
      
      df=data.frame(`entry date`=as.Date(character()), `entry price`=numeric(),`exit date`=as.Date(character()), `exit price`=numeric())
      entry=xts(numeric(),as.POSIXct('1900-01-01'))
      exit=xts(numeric(),as.POSIXct('1900-01-01'))
      
      repeat
      {
        remain_n=sig_n[sig_n & time(sig_n)>=time(exit)] #extract by symbol, extract sig_n after last exit
        if(length(remain_n[remain_n])<=0 || head(time(remain_n),1)==tail(time(dce_1m),1)) break  #no more sig-> break
        
        #extract sig_x after sig_n
        #extract bsl after sig_n
        #extract tsl after sig_n
        #extract psl after sig_n(nsl return nothing)
        #if all length 0 break
        #use the closest one, if sl collide at the same day, use the highest one
        
        # construct entry and exit
        
        entry=time(head(sig_n[sig_n & time(sig_n)>time(exit)],1))
        if(length(entry)==0) break
        entry=xts(Op(dce_1m)[as.character(nxd(entry))],nxd(entry))
        
        remain_x=sig_x[sig_x & time(sig_x)>=time(entry)]
        if(length(remain_x[remain_x])<=0) break
        else
        {
          exit=time(head(sig_x[sig_x & time(sig_x)>=time(entry)],1))
          exit=xts(Op(dce_1m)[as.character(nxd(exit))],nxd(exit))
          
          temp=cbind.data.frame(`entry date`=time(entry), `entry price`=as.numeric(entry),`exit date`=time(exit), `exit price`=as.numeric(exit))
          df=rbind(df,temp)
          
        }
      }
      
      if(nrow(df)>0)
      {
        ret_day=xts(rep(0,length(time(dce_1m))),time(dce_1m))
        
        ret_day[df$`entry date`]=log(Cl(dce_1m[df$`entry date`])/df$`entry price`)
        
        hold_period=paste(nxd(df$`entry date`,1),'/',nxd(df$`exit date`,-1),sep = '')
        ret_day[hold_period]=ROC(Cl(dce_1m))[hold_period]
        
        ret_day[as.character(df$`exit date`)]=log(df$`exit price`/Cl(dce_1m[as.character(nxd(df$`exit date`,-1))]))
        
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
proc.time()-start_run
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

ggplot(melt(ret_mean[12,,]), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster() +ggtitle('ret_mean')+scale_x_discrete(labels=j)+scale_y_discrete(labels=i)
ggplot(melt(win_rate), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster() +ggtitle('win_rate')+scale_x_discrete(labels=i)+scale_y_discrete(labels=i)
ggplot(melt(count), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('count')+scale_x_discrete(labels=i)+scale_y_discrete(labels=i)
ggplot(melt(dur_mean), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('dur_mean')+scale_x_discrete(labels=i)+scale_y_discrete(labels=i)
ggplot(melt(std), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('std')+scale_x_discrete(labels=i)+scale_y_discrete(labels=i)
ggplot(melt(sr), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('sr')+scale_x_discrete(labels=i)+scale_y_discrete(labels=i)
ggplot(melt(mdd_pct), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('mdd_pct')+scale_x_discrete(labels=i)+scale_y_discrete(labels=i)
ggplot(melt(mdd_dur), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('mdd_dur')+scale_x_discrete(labels=i)+scale_y_discrete(labels=i)

ret_mean_rank=melt(ret_mean)[order(melt(ret_mean)$value,decreasing = T),]
sr_rank=melt(sr)[order(melt(sr)$value,decreasing = T),]

vanila_cross_1_100=rpt
#saveRDS(rpt, 'crossover rpt.RData')
#vanila_cross_1_100=readRDS(file = 'crossover rpt.RData')

#all -ve, might want a filter of longer sma, more sensitive exit
#fast sma start from 70

#trend seems useless
#faster trend better, ie 180