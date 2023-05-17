library(quantmod)
library(tseries)
library(ggplot2)
library(reshape2)

nxd=function(x,n=1)
{
  cond_tail=x+n>=tail(date,1)
  cond_head=x+n<=head(date,1)
  
  as.Date(mapply(function(x,cond_tail,cond_head)
    {
      if(cond_tail) tail(date,1)
      else if(cond_head) head(date,1)
      else date[which(date==x)+n]
    },x,cond_tail,cond_head))
}

rawdata=read.csv('hsi.csv')
date=as.Date(levels(rawdata$Date)[rawdata$Date],'%m/%d/%Y')

HSI=xts(rawdata[,-1],date)

N=c(seq(1,10),seq(12,30,2),seq(35,100,5),seq(110,160,10),seq(180,200,20),250)

ret_mean=matrix(0,nrow = length(N),ncol = length(N),dimnames = list(N,N))
recent_ret_mean=matrix(0,nrow = length(N),ncol = length(N),dimnames = list(N,N))
count=matrix(0,nrow = length(N),ncol = length(N),dimnames = list(N,N))
dur_mean=matrix(0,nrow = length(N),ncol = length(N),dimnames = list(N,N))
std=matrix(0,nrow = length(N),ncol = length(N),dimnames = list(N,N))
recent_std=matrix(0,nrow = length(N),ncol = length(N),dimnames = list(N,N))
sr=matrix(0,nrow = length(N),ncol = length(N),dimnames = list(N,N))
recent_sr=matrix(0,nrow = length(N),ncol = length(N),dimnames = list(N,N))

mdd_pct=matrix(0,nrow = length(N),ncol = length(N),dimnames = list(N,N))
mdd_dur=matrix(0,nrow = length(N),ncol = length(N),dimnames = list(N,N))


for(m in N)
{
  for(n in N)
  {
    if(n-m>=3)
    {
      
      fast=SMA(Cl(HSI),m)
      slow=SMA(Cl(HSI),n)
      
      sig_n=fast>slow
      sig_x=fast<slow
      
      df=data.frame(`entry date`=as.Date(character()), `entry price`=numeric(),`exit date`=as.Date(character()), `exit price`=numeric())
      entry=xts(numeric(),as.Date('1900-01-01'))
      exit=xts(numeric(),as.Date('1900-01-01'))
      
      repeat
      {
        remain_n=sig_n[sig_n & time(sig_n)>=time(exit)] #extract by symbol, extract sig_n after last exit
        if(length(remain_n[remain_n])<=0 || head(time(remain_n),1)==tail(date,1)) break  #no more sig-> break
        
        #extract sig_x after sig_n
        #extract bsl after sig_n
        #extract tsl after sig_n
        #extract psl after sig_n(nsl return nothing)
        #if all length 0 break
        #use the closest one, if sl collide at the same day, use the highest one
        
        # construct entry and exit
        
        entry=time(head(sig_n[sig_n & time(sig_n)>time(exit)],1))
        if(length(entry)==0) break
        entry=xts(Op(HSI)[nxd(entry)],nxd(entry))
        
        remain_x=sig_x[sig_x & time(sig_x)>=time(entry)]
        if(length(remain_x[remain_x])<=0) break
        else
        {
          exit=time(head(sig_x[sig_x & time(sig_x)>=time(entry)],1))
          exit=xts(Op(HSI)[nxd(exit)],nxd(exit))
          
          temp=cbind.data.frame(`entry date`=time(entry), `entry price`=as.numeric(entry),`exit date`=time(exit), `exit price`=as.numeric(exit))
          df=rbind(df,temp)
          
        }
      }
      
      if(nrow(df)>0)
      {
        ret_day=xts(rep(0,length(date)),date)
        
        ret_day[df$`entry date`]=log(Cl(HSI[df$`entry date`])/df$`entry price`)
        
        hold_period=paste(nxd(df$`entry date`,1),'/',nxd(df$`exit date`,-1),sep = '')
        ret_day[hold_period]=ROC(Cl(HSI))[hold_period]
        
        ret_day[df$`exit date`]=log(df$`exit price`/Cl(HSI[nxd(df$`exit date`,-1)]))
        
        ret_mean[as.character(m),as.character(n)]=mean(ret_day)
        recent_ret_mean[as.character(m),as.character(n)]=mean(tail(ret_day,1000))
        count[as.character(m),as.character(n)]=nrow(df)
        dur_mean[as.character(m),as.character(n)]=length(ret_day[ret_day!=0])/count[as.character(m),as.character(n)]
        std[as.character(m),as.character(n)]=sqrt(var(ret_day))
        recent_std[as.character(m),as.character(n)]=sqrt(var(tail(ret_day,1000)))
        sr[as.character(m),as.character(n)]=ret_mean[as.character(m),as.character(n)]/std[as.character(m),as.character(n)]
        recent_sr[as.character(m),as.character(n)]=recent_ret_mean[as.character(m),as.character(n)]/recent_std[as.character(m),as.character(n)]
        
        mdd=maxdrawdown(cumsum(ret_day))
        mdd_pct[as.character(m),as.character(n)]=mdd$maxdrawdown
        mdd_dur[as.character(m),as.character(n)]=head(mdd$to,1)-head(mdd$from,1)
      }
    }
  }
}

ggplot(melt(ret_mean), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster() +ggtitle('ret_mean')
ggplot(melt(recent_ret_mean), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster() +ggtitle('recent_ret_mean')
ggplot(melt(count), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('count')
ggplot(melt(dur_mean), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('dur_mean')
ggplot(melt(std), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('std')
ggplot(melt(recent_std), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('recent_std')
ggplot(melt(sr), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('sr')
ggplot(melt(recent_sr), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('recent_sr')
ggplot(melt(mdd_pct), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('mdd_pct')
ggplot(melt(mdd_dur), aes(factor(Var1),factor(Var2), fill=value)) + geom_raster()+ggtitle('mdd_dur')

ret_mean_rank=melt(ret_mean)[order(melt(ret_mean)$value,decreasing = T),]
recent_ret_mean_rank=melt(recent_ret_mean)[order(melt(recent_ret_mean)$value,decreasing = T),]
sr_rank=melt(sr)[order(melt(sr)$value,decreasing = T),]
recent_sr_rank=melt(recent_sr)[order(melt(recent_sr)$value,decreasing = T),]
