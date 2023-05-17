for(slow in seq(from = 18, to = 32, by = 2))
{
  for(fast in seq(from = 4, to = 20, by = 2))
  {
    for(sig in seq(from = 4, to = 14, by = 1))
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
  }
}
