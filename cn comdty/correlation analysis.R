lookback=c(1,2,4,6,8,10,15,20,25,30,45,60,90,120,240)
hold=c(1,2,4,6,8,10,15,20,25,30,45,60,90,120,240)


cor_test_result=lapply(lookback, function(lb)
{
  lapply(hold, function(hd)
  {
    non_overlap=seq(1,nrow(dce_1m),min(lb,hd))
    ret_lb=ROC(Cl(dce_1m),lb)[non_overlap]
    ret_hd=ROC(lag(Cl(dce_1m),-hd),hd)[non_overlap]
    
    invalid=union(which(is.na(ret_hd)), 
                  which(is.na(ret_lb)))*-1
    
    cor.test(as.numeric(ret_lb[invalid]),as.numeric(ret_hd[invalid]))
  })
})

corr_result=laply(cor_test_result, function(x) laply(x, function(y) y[['estimate']]))
p_value_result=laply(cor_test_result, function(x) laply(x, function(y) y[['p.value']]))

ggplot(melt(corr_result), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+scale_x_discrete(labels=lookback)+scale_y_discrete(labels=hold)
ggplot(melt(p_value_result), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+scale_x_discrete(labels=lookback)+scale_y_discrete(labels=hold)

###omit open

lookback=c(1,2,4,6,8,10,15,20,25,30,45,60,90,120,240)
hold=c(1,2,4,6,8,10,15,20,25,30,45,60,90,120,240)


cor_test_omit_open_result=lapply(lookback, function(lb)
{
  lapply(hold, function(hd)
  {
    open=as.numeric(time(dce_1m))-Lag(as.numeric(time(dce_1m)))<60000  
    non_overlap=seq(1,nrow(dce_1m),min(lb,hd))
    ret_lb=ROC(Cl(dce_1m),lb)[union(which(open),non_overlap)]
    ret_hd=ROC(lag(Cl(dce_1m),-hd),hd)[union(which(open),non_overlap)]
    
    invalid=union(which(is.na(ret_hd)), 
                  which(is.na(ret_lb)))*-1
    
    cor.test(as.numeric(ret_lb[invalid]),as.numeric(ret_hd[invalid]))
  })
})

corr_result_omit_open=laply(cor_test_omit_open_result, function(x) laply(x, function(y) y[['estimate']]))
p_value_result_omit_open=laply(cor_test_omit_open_result, function(x) laply(x, function(y) y[['p.value']]))

ggplot(melt(corr_result_omit_open), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+scale_x_discrete(labels=lookback)+scale_y_discrete(labels=hold)
ggplot(melt(p_value_result_omit_open), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+scale_x_discrete(labels=lookback)+scale_y_discrete(labels=hold)


## try longer time frame

lookback=c(1,2,4,6,8,10,12,16,20,24,30,36,42,48,60,72,96)
hold=c(1,2,4,6,8,10,12,16,20,24,30,36,42,48,60,72,96)

dce_5m=to.period(dce_px_vo,
                 period = 'minutes',
                 k=5,
                 OHLC = TRUE)
names(dce_5m)=c('Open','High','Low','Close','Volume')


cor_test_omit_open_result=lapply(lookback, function(lb)
{
  lapply(hold, function(hd)
  {
    non_overlap=seq(1,nrow(dce_5m),min(lb,hd))
    ret_lb=ROC(Cl(dce_5m),lb)[non_overlap]
    ret_hd=ROC(lag(Cl(dce_5m),-hd),hd)[non_overlap]
    
    invalid=union(which(is.na(ret_hd)), 
                  which(is.na(ret_lb)))*-1
    
    cor.test(as.numeric(ret_lb[invalid]),as.numeric(ret_hd[invalid]))
  })
})

corr_result_omit_open=laply(cor_test_omit_open_result, function(x) laply(x, function(y) y[['estimate']]))
p_value_result_omit_open=laply(cor_test_omit_open_result, function(x) laply(x, function(y) y[['p.value']]))

ggplot(melt(corr_result_omit_open), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+scale_x_discrete(labels=lookback)+scale_y_discrete(labels=hold)
ggplot(melt(p_value_result_omit_open), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+scale_x_discrete(labels=lookback)+scale_y_discrete(labels=hold)



## try longer time frame

lookback=c(1,2,4,6,8,10,12,16,20,24,30,36,42,48,60,72,96)
hold=c(1,2,4,6,8,10,12,16,20,24,30,36,42,48,60,72,96)

dce_5m=to.period(dce_px_vo,
                 period = 'minutes',
                 k=5,
                 OHLC = TRUE)
names(dce_5m)=c('Open','High','Low','Close','Volume')


cor_test_omit_open_result=lapply(lookback, function(lb)
{
  lapply(hold, function(hd)
  {
    open=as.numeric(time(dce_5m))-Lag(as.numeric(time(dce_5m)))<60000  
    non_overlap=seq(1,nrow(dce_5m),min(lb,hd))
    ret_lb=ROC(Cl(dce_5m),lb)[union(which(open),non_overlap)]
    ret_hd=ROC(lag(Cl(dce_5m),-hd),hd)[union(which(open),non_overlap)]
    
    invalid=union(which(is.na(ret_hd)), 
                  which(is.na(ret_lb)))*-1
    
    cor.test(as.numeric(ret_lb[invalid]),as.numeric(ret_hd[invalid]))
  })
})

corr_result_omit_open=laply(cor_test_omit_open_result, function(x) laply(x, function(y) y[['estimate']]))
p_value_result_omit_open=laply(cor_test_omit_open_result, function(x) laply(x, function(y) y[['p.value']]))

ggplot(melt(corr_result_omit_open), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+scale_x_discrete(labels=lookback)+scale_y_discrete(labels=hold)
ggplot(melt(p_value_result_omit_open), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+scale_x_discrete(labels=lookback)+scale_y_discrete(labels=hold)


## try longer time frame, x look back, y hold

lookback=c(1,seq(2,80,by = 2))
hold=c(1,seq(2,80,by = 2))

dce_30m=to.period(dce_px_vo,
                 period = 'minutes',
                 k=30,
                 OHLC = TRUE)
names(dce_30m)=c('Open','High','Low','Close','Volume')


cor_test_omit_open_result=lapply(lookback, function(lb)
{
  lapply(hold, function(hd)
  {
    open=as.numeric(time(dce_30m))-Lag(as.numeric(time(dce_30m)))<60000  
    non_overlap=seq(1,nrow(dce_30m),min(lb,hd))
    ret_lb=ROC(Cl(dce_30m),lb)[union(which(open),non_overlap)]
    ret_hd=ROC(lag(Cl(dce_30m),-hd),hd)[union(which(open),non_overlap)]
    
    invalid=union(which(is.na(ret_hd)), 
                  which(is.na(ret_lb)))*-1
    
    cor.test(as.numeric(ret_lb[invalid]),as.numeric(ret_hd[invalid]))
  })
})

corr_result_omit_open=laply(cor_test_omit_open_result, function(x) laply(x, function(y) y[['estimate']]))
p_value_result_omit_open=laply(cor_test_omit_open_result, function(x) laply(x, function(y) y[['p.value']]))

ggplot(melt(corr_result_omit_open), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+scale_x_discrete(labels=lookback)+scale_y_discrete(labels=hold)
ggplot(melt(p_value_result_omit_open), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+scale_x_discrete(labels=lookback)+scale_y_discrete(labels=hold)

#
signal_n=ROC(Cl(dce_30m),20)<0
entry=Op(dce_30m)[signal_n]
exit_i=which(signal_n)+60
exit_i=ifelse(exit_i>nrow(dce_30m),nrow(dce_30m),exit_i)
exit=Cl(dce_30m)[exit_i]
profit=exit-entry


sig_n=ROC(Cl(dce_30m),20)<0
#sig_n[cumsum(rle(as.character(as.Date(time(sig_n))))$lengths)]=F
sig_x=lag(sig_n,60)
#sig_x[cumsum(rle(as.character(as.Date(time(sig_x))))$lengths)-1]=T

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
