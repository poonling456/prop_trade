N=60
i=seq(15,N,by = 2)

ret_mean=matrix(0,nrow = length(i),ncol = length(i),dimnames = list(as.character(i),as.character(i)))
count=matrix(0,nrow = length(i),ncol = length(i),dimnames = list(as.character(i),as.character(i)))      
win_rate=matrix(0,nrow = length(i),ncol = length(i),dimnames = list(as.character(i),as.character(i)))      
up_day=matrix(0,nrow = length(i),ncol = length(i),dimnames = list(as.character(i),as.character(i)))      
dn_day=matrix(0,nrow = length(i),ncol = length(i),dimnames = list(as.character(i),as.character(i)))      
dur_mean=matrix(0,nrow = length(i),ncol = length(i),dimnames = list(as.character(i),as.character(i)))
std=matrix(0,nrow = length(i),ncol = length(i),dimnames = list(as.character(i),as.character(i)))
sr=matrix(0,nrow = length(i),ncol = length(i),dimnames = list(as.character(i),as.character(i)))

mdd_pct=matrix(0,nrow = length(i),ncol = length(i),dimnames = list(as.character(i),as.character(i)))
mdd_dur=matrix(0,nrow = length(i),ncol = length(i),dimnames = list(as.character(i),as.character(i)))


start_run=proc.time()

  for(n in i)
  { 
    if(n-m>=5)
    {
      fast=SMA(Cl(dce_1m),m)
      slow=SMA(Cl(dce_1m),n)
      
      sig_n=fast>slow
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
        
        ret_mean[as.character(m),as.character(n)]=mean(ret_day)
        count[as.character(m),as.character(n)]=nrow(df)
        win_rate[as.character(m),as.character(n)]=length(which(df$`exit price`>df$`entry price`))/count[as.character(m),as.character(n)]
        up_day[as.character(m),as.character(n)]=length(which(ret_day>0))/length(ret_day)
        dn_day[as.character(m),as.character(n)]=length(which(ret_day<0))/length(ret_day)
        dur_mean[as.character(m),as.character(n)]=length(ret_day[ret_day!=0])/count[as.character(m),as.character(n)]
        std[as.character(m),as.character(n)]=sqrt(var(ret_day))
        sr[as.character(m),as.character(n)]=ret_mean[as.character(m),as.character(n)]/std[as.character(m),as.character(n)]
        
        mdd=maxdrawdown(cumsum(ret_day))
        mdd_pct[as.character(m),as.character(n)]=mdd$maxdrawdown
        mdd_dur[as.character(m),as.character(n)]=head(mdd$to,1)-head(mdd$from,1)
      }
    }
  }
proc.time()-start_run

ggplot(melt(ret_mean), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster() +ggtitle('ret_mean')
ggplot(melt(count), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('count')
ggplot(melt(win_rate), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('win_rate')
ggplot(melt(up_day), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('up_day')
ggplot(melt(dn_day), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('dn_day')
ggplot(melt(dur_mean), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('dur_mean')
ggplot(melt(std), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('std')
ggplot(melt(sr), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('sr')
ggplot(melt(mdd_pct), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('mdd_pct')
ggplot(melt(mdd_dur), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('mdd_dur')

ret_mean_rank=melt(ret_mean)[order(melt(ret_mean)$value,decreasing = T),]
sr_rank=melt(sr)[order(melt(sr)$value,decreasing = T),]
